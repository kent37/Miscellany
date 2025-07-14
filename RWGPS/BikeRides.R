library(tidyverse)
library(lubridate)
library(httr2)
library(sf)

# Get tracks from a downloaded backup
gps_path = here::here('RWGPS/rwgps_backup/')
gps_files = list.files(gps_path, '*.gpx', full.names=TRUE)

# Read a GPX, return length and climb in feet and start time
# Don't use this
read_gpx = function(path) {
  track = read_sf(path, layer='tracks')
  length = st_length(st_transform(track, 2249))
  pts = read_sf(path, layer='track_points')
  time = pts$time[[1]]
  
  # This roughly approximates what ride with GPS does
  # It does not give exactly the same results!
  # It does give approximately the same cumulative values for 2024 and 2025
  # though much larger values for 2022 and 2023 (?)
  # window_size = 4
  # ele_smoothed = slider::slide_dbl(ele, mean, .before=window_size-1)
  # ele_diff = diff(ele_smoothed)
  # min_threshold = 1/feet_per_meter
  # ele_diff_threshold = ifelse(abs(ele_diff) < min_threshold, 0, ele_diff)
  # 
  # climb = sum(ele_diff_threshold[ele_diff_threshold > 0]) * feet_per_meter
  # 
  # tibble(length=length, climb=climb, time=time)
}

# tracks_meta = map(gps_files, read_gpx, .progress=TRUE) |> 
#   list_rbind()

# tracks_data = tracks_meta |> 
#   arrange(time) |> 
#   mutate(year=year(time), month=month(time), yday=yday(time), 
#          miles=unclass(length/5280)) |> 
#   group_by(year) |> 
#   mutate(cum_miles=cumsum(miles), cum_climb=cumsum(climb)) |> 
#   ungroup()

# Authenticate at RwGPS
api_key = keyring::key_get('RideWithGPS_API_Key')
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

# Request rides
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

# Data to use for plotting
feet_per_meter = 3.28084
tracks_data = trips_raw |> 
  transmute(datestamp=lubridate::ymd_hms(departed_at),
    year=year(datestamp), month=month(datestamp), yday=yday(datestamp), 
    miles=distance*feet_per_meter/5280,
    elevation=elevation_gain*feet_per_meter,
    moving_time=moving_time/60) |> 
  arrange(datestamp) |> 
  group_by(year) |> 
  mutate(cum_miles=cumsum(miles), 
         cum_climb=cumsum(elevation),
         cum_time=cumsum(moving_time),
         n=seq_along(miles)) |> 
  ungroup()

# Make break points and labels
breaks = tibble(
  date=seq.Date(ymd('2023-01-01'), ymd('2023-12-1'), by='month'),
  year_day=yday(date),
  label=format(date, '%b'))

# Cumulative miles
(miles = ggplot(tracks_data, aes(yday, cum_miles, color=factor(year))) +
  geom_line() +
  scale_x_continuous(breaks=breaks$year_day, labels=breaks$label, 
                     minor_breaks=NULL) +
  scale_y_continuous(limits=c(0, 3000), labels=scales::comma) +
  labs(x='', y='Miles', title='Cumulative miles ridden by year', color='') +
  theme_minimal() +
  theme(axis.text.x=element_text(hjust=-0.2),
          plot.title=element_text(face='bold', size=rel(1.5))))

# Cumulative climb
(climb = ggplot(tracks_data, aes(yday, cum_climb, color=factor(year))) +
  geom_step() +
  scale_x_continuous(breaks=breaks$year_day, labels=breaks$label, 
                     minor_breaks=NULL) +
  scale_y_continuous(labels=scales::comma) +
  labs(x='', y='Feet climbed', 
       title='Cumulative feet climbed by year', color='') +
  theme_minimal() +
  theme(axis.text.x=element_text(hjust=-0.2),
          plot.title=element_text(face='bold', size=rel(1.5))))

# Cumulative moving time
(time = ggplot(tracks_data, aes(yday, cum_time, color=factor(year))) +
  geom_line() +
  scale_x_continuous(breaks=breaks$year_day, labels=breaks$label, 
                     minor_breaks=NULL) +
  scale_y_continuous(labels=scales::comma) +
  labs(x='', y='Moving time', 
       title='Cumulative moving time by year', color='') +
  theme_minimal() +
  theme(axis.text.x=element_text(hjust=-0.2),
          plot.title=element_text(face='bold', size=rel(1.5))))

# Cumulative number of rides
(rides = ggplot(tracks_data, aes(yday, n, color=factor(year))) +
  geom_step() +
  scale_x_continuous(breaks=breaks$year_day, labels=breaks$label, 
                     minor_breaks=NULL) +
  scale_y_continuous(labels=scales::comma) +
  labs(x='', y='Number of rides', 
       title='Cumulative rides by year', color='') +
  theme_minimal() +
  theme(axis.text.x=element_text(hjust=-0.2),
          plot.title=element_text(face='bold', size=rel(1.5))))

# Monthly miles
tracks_data |> 
  summarize(miles=sum(miles), .by=c(year, month)) |> 
  ggplot(aes(month, miles, fill=factor(year))) +
  geom_col(position=position_dodge(preserve='single')) +
  scale_x_continuous(breaks = 1:12, labels=month.abb, minor_breaks=NULL) +
  labs(title='Monthly miles', fill=NULL) +
  theme_minimal()

# Monthly climb
tracks_data |> 
  summarize(climb=sum(elevation), .by=c(year, month)) |> 
  ggplot(aes(month, climb, fill=factor(year))) +
  geom_col(position=position_dodge(preserve='single')) +
  scale_x_continuous(breaks = 1:12, labels=month.abb, minor_breaks=NULL) +
  labs(title='Monthly climp', fill=NULL) +
  theme_minimal()

# Monthly moving time
tracks_data |> 
  summarize(moving_time=sum(moving_time), .by=c(year, month)) |> 
  ggplot(aes(month, moving_time, fill=factor(year))) +
  geom_col(position=position_dodge(preserve='single')) +
  scale_x_continuous(breaks = 1:12, labels=month.abb, minor_breaks=NULL) +
  labs(title='Monthly moving time', fill=NULL) +
  theme_minimal()

# Histogram of miles
tracks_data |> 
  ggplot(aes(miles, fill=factor(year), group=year)) +
  geom_histogram(binwidth=5, position='dodge') +
  scale_y_continuous(minor_breaks=NULL) +
  labs(title='Ride length', y='Number of rides', fill=NULL) +
  facet_wrap(~year, ncol=1) +
  theme_minimal()
