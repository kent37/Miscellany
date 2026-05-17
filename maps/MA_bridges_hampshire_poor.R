library(tidyverse)
library(leaflet)
library(htmlwidgets)

bridges <- read_csv(here::here("data/MA_bridges_2025.csv", show_col_types = FALSE))

# Parse NBI lat/lon: DDMMSSCC (lat, 8 digits) / DDDMMSSCC (lon, 9 digits)
# Last 4 digits are SSCC where SS = whole seconds, CC = hundredths
parse_lat <- function(x) {
  s <- str_pad(as.character(as.integer(x)), 8, pad = "0")
  deg <- as.numeric(substr(s, 1, 2))
  min <- as.numeric(substr(s, 3, 4))
  sec <- as.numeric(substr(s, 5, 8)) / 100
  deg + min / 60 + sec / 3600
}

parse_lon <- function(x) {
  s <- str_pad(as.character(as.integer(x)), 9, pad = "0")
  deg <- as.numeric(substr(s, 1, 3))
  min <- as.numeric(substr(s, 4, 5))
  sec <- as.numeric(substr(s, 6, 9)) / 100
  -(deg + min / 60 + sec / 3600)
}

cond_label <- function(x) {
  case_when(
    x == "N" ~ "N/A",
    x == "0" ~ "0 – Failed",
    x == "1" ~ "1 – Imminent Failure",
    x == "2" ~ "2 – Critical",
    x == "3" ~ "3 – Serious",
    x == "4" ~ "4 – Poor",
    x == "5" ~ "5 – Fair",
    x == "6" ~ "6 – Satisfactory",
    x == "7" ~ "7 – Good",
    x == "8" ~ "8 – Very Good",
    x == "9" ~ "9 – Excellent",
    TRUE ~ as.character(x)
  )
}

hamp_poor <- bridges |>
  filter(COUNTY_CODE_003 == "015", BRIDGE_CONDITION == "P") |>
  mutate(
    lat = parse_lat(LAT_016),
    lon = parse_lon(LONG_017),
    route      = str_trim(str_remove_all(FACILITY_CARRIED_007, "'")),
    crosses    = str_trim(str_remove_all(FEATURES_DESC_006A, "'")),
    location   = str_trim(str_remove_all(LOCATION_009, "'")),
    deck_lbl   = cond_label(as.character(DECK_COND_058)),
    super_lbl  = cond_label(as.character(SUPERSTRUCTURE_COND_059)),
    sub_lbl    = cond_label(as.character(SUBSTRUCTURE_COND_060)),
    marker_col = case_when(
      LOWEST_RATING <= 1 ~ "#7b0000",
      LOWEST_RATING == 2 ~ "#cc0000",
      LOWEST_RATING == 3 ~ "#e05c00",
      TRUE               ~ "#e09200"   # rating 4
    )
  )

# Sanity check coordinates
cat("Lat range:", range(hamp_poor$lat), "\n")
cat("Lon range:", range(hamp_poor$lon), "\n")
cat("Bridges to map:", nrow(hamp_poor), "\n")

# Build popup HTML
hamp_poor <- hamp_poor |>
  mutate(popup = sprintf(
    "<b>%s</b><br>
     <i>Crosses:</i> %s<br>
     <i>Location:</i> %s<br>
     <hr style='margin:4px 0'>
     <b>Overall condition: <span style='color:%s'>%s</span></b><br>
     Lowest rating: <b>%g</b><br>
     <hr style='margin:4px 0'>
     Deck: %s<br>
     Superstructure: %s<br>
     Substructure: %s<br>
     <hr style='margin:4px 0'>
     Year built: %s<br>
     Avg daily traffic: %s<br>
     Structure #: %s",
    route, crosses, location,
    marker_col, BRIDGE_CONDITION,
    LOWEST_RATING,
    deck_lbl, super_lbl, sub_lbl,
    YEAR_BUILT_027,
    format(ADT_029, big.mark = ","),
    STRUCTURE_NUMBER_008
  ))

# Rating legend labels
legend_colors <- c("#7b0000", "#cc0000", "#e05c00", "#e09200")
legend_labels <- c("1 – Imminent Failure", "2 – Critical", "3 – Serious", "4 – Poor")
# Only include ratings that exist in the data
present_ratings <- sort(unique(hamp_poor$LOWEST_RATING))
legend_colors <- legend_colors[present_ratings]
legend_labels <- legend_labels[present_ratings]

map <- leaflet(hamp_poor) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    radius = 9,
    color = "white", weight = 1,
    fillColor = ~marker_col, fillOpacity = 0.9,
    popup = ~popup,
    label = ~route
  ) |>
  addLegend(
    position = "bottomright",
    colors = legend_colors,
    labels = legend_labels,
    title = "Lowest component rating",
    opacity = 0.9
  )

saveWidget(map, "MA_bridges_hampshire_poor.html", selfcontained = TRUE)
cat("Saved MA_bridges_hampshire_poor.html\n")
