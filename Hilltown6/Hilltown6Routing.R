# ============================================================
# Shortest Road Route Across 9 Locations
# Stack: OSRM (distance matrix + route geometry) +
#        TSP (optimal visit order) +
#        leaflet (visualization)
# ============================================================

library(tidyverse)
library(httr2)
library(jsonlite)
library(TSP)
library(leaflet)
library(sf)
library(tidygeocoder)

# ------------------------------------------------------------
# 1. Define locations
# ------------------------------------------------------------

potter_addresses <- tribble(
  ~name,                  ~address,
  "Home",                 "1 Aldrich St, Northampton, MA 01060",
  "Robbie Heidinger",     "1 Stage Road, Westhampton, MA 01027",
  "Christy Knox",         "108 Mount Road, Cummington, MA 01026",
  "Maya Machin",          "193 Hill Road, Ashfield, MA 01330",
  "Michael McCarthy",     "70 Bissell Road, Williamsburg, MA 01096",
  "Hiroshi Nakayama",     "193 Fisk Road, Worthington, MA 01098",
  "Mark Shapiro",         "42 Conwell Road, Worthington, MA 01098",
  "Constance Talbot",     "34 Shaw Road, Windsor, MA 01270",
  "Sam Taylor",           "35 Perry Hill Road Extension, Westhampton, MA 01027",
)

locations <- potter_addresses |>
  geocode(address = address, method = "opencage") |>
  rename(lon = long)

write_csv(locations, here::here("Hilltown6/locations.csv"))

# ------------------------------------------------------------
# 2. Helper functions
# ------------------------------------------------------------

build_osrm_matrix <- function(locs) {
  coords_str <- locs |>
    str_glue_data("{lon},{lat}") |>
    str_c(collapse = ";")

  url <- str_glue(
    "https://router.project-osrm.org/table/v1/driving/{coords_str}",
    "?annotations=duration,distance"
  )

  result <- request(url) |> req_perform() |> resp_body_json()

  if (result$code != "Ok") stop("OSRM table request failed: ", result$code)

  parse_matrix <- function(rows) {
    rows |>
      map(\(row) unlist(row)) |>
      reduce(rbind) |>
      `rownames<-`(locs$name) |>
      `colnames<-`(locs$name)
  }

  list(
    duration = parse_matrix(result$durations),
    distance = parse_matrix(result$distances)
  )
}

solve_tsp_order <- function(duration_matrix) {
  tour <- solve_TSP(ATSP(duration_matrix), method = "two_opt",
                    control = list(rep = 10))
  cat(str_glue("Optimal tour: {round(tour_length(tour) / 60, 1)} minutes\n\n"))
  as.integer(tour)
}

fetch_osrm_route <- function(locs_ordered) {
  coords_str <- locs_ordered |>
    mutate(coord = str_glue("{lon},{lat}")) |>
    pull(coord) |>
    str_c(collapse = ";")

  url <- str_glue(
    "https://router.project-osrm.org/route/v1/driving/{coords_str}",
    "?overview=full&geometries=geojson&steps=false"
  )

  result <- request(url) |> req_perform() |> resp_body_json()

  if (result$code != "Ok") stop("OSRM route request failed: ", result$code)

  route <- result$routes[[1]]

  coords_matrix <- route$geometry$coordinates |>
    map(\(pt) c(pt[[1]], pt[[2]])) |>
    reduce(rbind)

  st_linestring(coords_matrix) |>
    st_sfc(crs = 4326) |>
    st_sf(
      total_distance_mi  = round(route$distance / 1609.344, 2),
      total_duration_min = round(route$duration / 60, 1)
    )
}

# Build and solve a complete tour for a subset of locations.
# With rotate_to_home = TRUE the tour is rotated so "Home" is stop 1.
run_tour <- function(locs, rotate_to_home = FALSE) {
  cat("Fetching OSRM matrix...\n")
  mat  <- build_osrm_matrix(locs)
  tour <- solve_tsp_order(mat$duration)

  if (rotate_to_home) {
    home_pos <- which(tour == which(locs$name == "Home"))
    n        <- length(tour)
    if (home_pos != 1)
      tour <- c(tour[home_pos:n], tour[seq_len(home_pos - 1)])
  }

  ordered  <- locs |> slice(tour) |> bind_rows(slice(locs, tour[1]))
  route_sf <- fetch_osrm_route(ordered)
  cat(str_glue("Route: {route_sf$total_distance_mi} mi, ",
               "{route_sf$total_duration_min} min\n\n"))

  list(mat = mat, tour = tour, ordered = ordered, route_sf = route_sf)
}

# Add step numbers, labels, and per-leg driving times to a tour result.
# mid_color is stored as a column so leaflet formulas can reference it.
compute_stops <- function(tour_result, mid_color = "#2563EB") {
  n <- length(tour_result$tour)
  tour_result$ordered |>
    slice_head(n = n) |>
    mutate(
      step       = row_number(),
      label_text = str_glue("{step}. {name}"),
      mid_color  = mid_color,
      prev_idx   = tour_result$tour[if_else(step == 1L, n, step - 1L)],
      curr_idx   = tour_result$tour[step],
      drive_min  = map2_dbl(prev_idx, curr_idx,
                            \(i, j) round(tour_result$mat$duration[i, j] / 60, 1))
    )
}

# Build a leaflet map for one or more tours.
# tours: list of list(stops = <df>, route_sf = <sf>)
build_route_map <- function(tours, route_line_colors = c("#2563EB", "#E07020")) {
  map <- leaflet() |> addProviderTiles(providers$CartoDB.Positron)

  for (i in seq_along(tours)) {
    s  <- tours[[i]]$stops
    r  <- tours[[i]]$route_sf
    lc <- route_line_colors[[i]]

    map <- map |>
      addPolylines(
        data    = r,
        color   = lc,
        weight  = 4,
        opacity = 0.8,
        label   = str_glue("{r$total_distance_mi} mi / {r$total_duration_min} min")
      ) |>
      addCircleMarkers(
        data        = s,
        lng         = ~lon,
        lat         = ~lat,
        radius      = 10,
        color       = "white",
        weight      = 2,
        fillColor   = ~case_when(
          step == 1         ~ "#16A34A",
          step == max(step) ~ "#DC2626",
          TRUE              ~ mid_color
        ),
        fillOpacity = 0.9,
        label       = ~label_text,
        popup       = ~str_glue(
          "<b>Stop {step}</b><br>{name}<br>",
          "<small>Driving time from previous stop: {drive_min} min</small>"
        )
      ) |>
      addLabelOnlyMarkers(
        data         = s,
        lng          = ~lon,
        lat          = ~lat,
        label        = ~as.character(step),
        labelOptions = labelOptions(
          noHide    = TRUE,
          direction = "center",
          textOnly  = TRUE,
          style     = list(
            "color"       = "white",
            "font-weight" = "bold",
            "font-size"   = "11px"
          )
        )
      )
  }

  legend_rows <- imap_chr(tours, \(t, i) {
    prefix <- if (length(tours) > 1) str_glue("Day {i}: ") else ""
    str_glue("{prefix}{t$route_sf$total_distance_mi} mi &bull; ",
             "{t$route_sf$total_duration_min} min")
  }) |>
    str_c(collapse = "<br>")

  map |>
    addControl(
      html = str_glue(
        "<div style='background:white;padding:8px 12px;border-radius:6px;",
        "box-shadow:0 1px 4px rgba(0,0,0,.3);font-size:13px;'>",
        "<b>TSP Route</b><br>{legend_rows}<br>",
        "<span style='color:#16A34A'>&#9679;</span> Home &nbsp;",
        "<span style='color:#2563EB'>&#9679;</span> Stop &nbsp;",
        "<span style='color:#DC2626'>&#9679;</span> Last stop",
        "</div>"
      ),
      position = "bottomright"
    )
}

# Google Maps directions URL for an ordered stops data frame.
# Joins addresses from potter_addresses, then closes the loop back to stop 1.
directions_url <- function(stops) {
  addrs <- c(stops$address, stops$address[[1]])
  paste0(
    "https://www.google.com/maps/dir/",
    str_c(URLencode(addrs, reserved = TRUE), collapse = "/")
  )
}

# Per-leg summary table for a tour.
build_leg_table <- function(stops, tour_result) {
  idx_pairs <- list(
    from = tour_result$tour,
    to   = c(tour_result$tour[-1], tour_result$tour[1])
  )
  tibble(
    from         = stops$name,
    to           = c(stops$name[-1], stops$name[1]),
    duration_min = map2_dbl(idx_pairs$from, idx_pairs$to,
                            \(i, j) round(tour_result$mat$duration[i, j] / 60, 1)),
    distance_mi  = map2_dbl(idx_pairs$from, idx_pairs$to,
                            \(i, j) round(tour_result$mat$distance[i, j] / 1609.344, 2))
  )
}

# ------------------------------------------------------------
# 3. Single-day tour (all locations)
# ------------------------------------------------------------

cat("=== Single-day tour ===\n")
full_tour <- run_tour(locations, rotate_to_home = TRUE)
stops     <- compute_stops(full_tour)

cat("Tour order:\n")
stops |> select(step, name) |> print()

map1 <- build_route_map(list(list(stops = stops, route_sf = full_tour$route_sf)))
map1

cat("Directions URL:\n")
cat(directions_url(stops), "\n\n")

cat("Leg-by-leg summary:\n")
print(build_leg_table(stops, full_tour))

# ------------------------------------------------------------
# 4. Two-day split
# ------------------------------------------------------------

cat("\n=== Two-day split ===\n")

# Rotate the full tour so Home is first, then split at position 5.
# The TSP has already clustered geographically close stops, so the
# first 5 and last 4 (each with Home) form two coherent day trips.
home_idx     <- which(locations$name == "Home")
home_in_tour <- which(full_tour$tour == home_idx)
n_locs       <- nrow(locations)
tour_rotated <- c(
  full_tour$tour[home_in_tour:n_locs],
  full_tour$tour[seq_len(home_in_tour - 1)]
)

group1_locs <- locations[tour_rotated[1:5], ]
group2_locs <- locations[tour_rotated[c(1, 6:n_locs)], ]

cat("--- Day 1 ---\n")
trip1  <- run_tour(group1_locs, rotate_to_home = TRUE)
stops1 <- compute_stops(trip1, mid_color = "#2563EB")

cat("Day 1 tour order:\n")
stops1 |> select(step, name) |> print()

cat("--- Day 2 ---\n")
trip2  <- run_tour(group2_locs, rotate_to_home = TRUE)
stops2 <- compute_stops(trip2, mid_color = "#E07020")

cat("Day 2 tour order:\n")
stops2 |> select(step, name) |> print()

map2 <- build_route_map(
  list(
    list(stops = stops1, route_sf = trip1$route_sf),
    list(stops = stops2, route_sf = trip2$route_sf)
  )
)
map2

cat("Day 1 directions URL:\n")
cat(directions_url(stops1), "\n\n")
cat("Day 1 leg summary:\n")
print(build_leg_table(stops1, trip1))

cat("Day 2 directions URL:\n")
cat(directions_url(stops2), "\n\n")
cat("Day 2 leg summary:\n")
print(build_leg_table(stops2, trip2))

# ------------------------------------------------------------
# 5. Write directions HTML
# ------------------------------------------------------------

make_link <- function(label, subtitle, stops) {
  url <- directions_url(stops)
  str_glue(
    "<li>",
    "<a href='{url}'>{label}</a>",
    "<span class='sub'>{subtitle}</span>",
    "</li>"
  )
}

html <- str_glue(
  "<!DOCTYPE html>",
  "<html lang='en'><head><meta charset='UTF-8'>",
  "<title>Hilltown 6 Directions</title>",
  "<style>",
  "  body {{ font-family: sans-serif; max-width: 480px; margin: 2rem auto; }}",
  "  ul {{ list-style: none; padding: 0; }}",
  "  li {{ margin: 1rem 0; }}",
  "  a {{ font-size: 1.1rem; }}",
  "  .sub {{ display: block; color: #666; font-size: 0.85rem; margin-top: 0.2rem; }}",
  "</style></head><body>",
  "<h2>Hilltown 6 Pottery Tour</h2>",
  "<ul>",
  make_link(
    "Full tour (all 8 potters)",
    str_glue("{full_tour$route_sf$total_distance_mi} mi &bull; ",
             "{full_tour$route_sf$total_duration_min} min"),
    stops
  ),
  make_link(
    "Day 1",
    str_glue("{trip1$route_sf$total_distance_mi} mi &bull; ",
             "{trip1$route_sf$total_duration_min} min &bull; ",
             "{str_c(stops1$name[stops1$name != 'Home'], collapse = ', ')}"),
    stops1
  ),
  make_link(
    "Day 2",
    str_glue("{trip2$route_sf$total_distance_mi} mi &bull; ",
             "{trip2$route_sf$total_duration_min} min &bull; ",
             "{str_c(stops2$name[stops2$name != 'Home'], collapse = ', ')}"),
    stops2
  ),
  "</ul></body></html>",
  .sep = "\n"
)

writeLines(html, here::here("Hilltown6/Hilltown6_directions.html"))
cat("Wrote Hilltown6_directions.html\n")
