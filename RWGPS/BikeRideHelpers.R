library(httr2)

# Authenticate at RwGPS
get_auth_token <- function(api_key) {
  password = keyring::key_get('RideWithGPS_Password')
  req <- request("https://ridewithgps.com/api/v1/auth_tokens.json") |>
    req_headers('x-rwgps-api-key'=api_key) |>
    req_body_json(list(
      user = list(email='kent3737@gmail.com',
      password = password
      ))
  )

  resp <- req_perform(req)
  auth_token = resp |> resp_body_json() |> pluck('auth_token', 'auth_token')
  auth_token
}

# Request rides
get_rides <- function(api_key, auth_token) {
  # Get the number of pages
  req = request('https://ridewithgps.com/api/v1/trips.json') |>
    req_headers('x-rwgps-api-key'=api_key,
                'x-rwgps-auth-token' = auth_token)
  resp <- req_perform(req)
  pages = resp |> resp_body_json() |> pluck('meta', 'pagination', 'page_count')

  # Get one page of trips
  get_trips_page = function(page) {
   req = request(
     glue::glue('https://ridewithgps.com/api/v1/trips.json?page={page}')) |>
    req_headers('x-rwgps-api-key'=api_key,
                'x-rwgps-auth-token' = auth_token)
    resp <- req_perform(req)
    resp |> resp_body_json() |> pluck('trips')
  }

  # Get all the trips in a data frame
  trips_raw = 1:pages |>
    map(get_trips_page) |>
    bind_rows()

  trips_raw
}

plot_miles <- function(tracks_data, y, title) {
  ggplot(tracks_data, aes(yday, {{y}}, color=factor(year))) +
    geom_step() +
    scale_x_continuous(breaks=breaks$year_day, labels=breaks$label,
                       minor_breaks=NULL, 
                       guide=guide_axis(check.overlap=TRUE)) +
    scale_y_continuous(limits=c(0, 3000), labels=scales::comma) +
    scale_color_manual(values=year_colors) +
    labs(x='', y='Miles', title=title, color='')
}

plot_climb <- function(tracks_data, y, title) {
  ggplot(tracks_data, aes(yday, {{y}}, color=factor(year))) +
    geom_step() +
    scale_x_continuous(breaks=breaks$year_day, labels=breaks$label,
                       minor_breaks=NULL, 
                       guide=guide_axis(check.overlap=TRUE)) +
    scale_y_continuous(labels=scales::comma) +
    scale_color_manual(values=year_colors) +
    labs(x='', y='Feet climbed', title=title, color='')
}

plot_time <- function(tracks_data, y, title) {
  ggplot(tracks_data, aes(yday, {{y}}, color=factor(year))) +
    geom_step() +
    scale_x_continuous(breaks=breaks$year_day, labels=breaks$label,
                       minor_breaks=NULL, 
                       guide=guide_axis(check.overlap=TRUE)) +
    scale_y_continuous(labels=scales::comma) +
    scale_color_manual(values=year_colors) +
    labs(x='', y='Moving time', title=title, color='')
}

plot_rides <- function(tracks_data, y, title) {
  ggplot(tracks_data, aes(yday, {{y}}, color=factor(year))) +
    geom_step() +
    scale_x_continuous(breaks=breaks$year_day, labels=breaks$label,
                       minor_breaks=NULL,
                       guide=guide_axis(check.overlap=TRUE)) +
    scale_y_continuous(labels=scales::comma) +
    scale_color_manual(values=year_colors) +
    labs(x='', y='Number of rides', title=title, color='')
}

# Compare the current year's latest cumulative value to the most recent prior
# year's cumulative value at the same point in the year (day-of-year)
progress_vs_prior_year <- function(data, col, unit, transform = identity, digits = 0) {
  col <- enquo(col)
  current_year <- max(data$year)
  prior_year <- max(data$year[data$year < current_year])

  value_at <- function(yr, day) {
    rows <- data |> filter(year == yr, yday <= day)
    if (nrow(rows) == 0) return(0)
    rows |> slice_max(yday, n = 1, with_ties = FALSE) |> pull(!!col)
  }

  this_day <- max(data$yday[data$year == current_year])
  diff <- transform(value_at(current_year, this_day) - value_at(prior_year, this_day))
  direction <- if (diff >= 0) 'ahead of' else 'behind'
  glue::glue('{scales::comma(abs(diff), accuracy=10^-digits)} {unit} {direction} {prior_year}')
}
