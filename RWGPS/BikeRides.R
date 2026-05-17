library(tidyverse)
library(lubridate)
library(httr2)
library(sf)

source(here::here('RWGPS/BikeRideHelpers.R'))

# Get tracks from a downloaded backup
# gps_path = here::here('RWGPS/rwgps_backup/')
# gps_files = list.files(gps_path, '*.gpx', full.names=TRUE)

# Read a GPX, return length and climb in feet and start time
# Don't use this
# read_gpx = function(path) {
#   track = read_sf(path, layer='tracks')
#   length = st_length(st_transform(track, 2249))
#   pts = read_sf(path, layer='track_points')
#   time = pts$time[[1]]
  
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
# }

# tracks_meta = map(gps_files, read_gpx, .progress=TRUE) |> 
#   list_rbind()

# tracks_data = tracks_meta |> 
#   arrange(time) |> 
#   mutate(year=year(time), month=month(time), yday=yday(time), 
#          miles=unclass(length/5280)) |> 
#   group_by(year) |> 
#   mutate(cum_miles=cumsum(miles), cum_climb=cumsum(climb)) |> 
#   ungroup()

feet_per_meter = 3.28084

# Make break points and labels
breaks = tibble(
  date=seq.Date(ymd('2023-01-01'), ymd('2023-12-1'), by='month'),
  year_day=yday(date),
  label=format(date, '%b'))

theme_set(theme_minimal() +
  theme(axis.text.x=element_text(hjust=-0.2),
          plot.title=element_text(face='bold', size=rel(1.5))))


# Authenticate
api_key = keyring::key_get('RideWithGPS_API_Key')
auth_token = get_auth_token(api_key)

# Get ride data
tracks_raw = get_rides(api_key, auth_token)

# Data to use for plotting
tracks_data = tracks_raw |> 
  filter(activity_type != 'cycling:virtual') |> # No Zwift
  transmute(datestamp=lubridate::ymd_hms(departed_at),
    year=year(datestamp), month=month(datestamp), yday=yday(datestamp), 
    miles=distance*feet_per_meter/5280,
    elevation=elevation_gain*feet_per_meter,
    moving_time=moving_time/60,
  activity=str_remove(activity_type, 'cycling:')) |> 
  arrange(datestamp) |> 
  group_by(year) |> 
  mutate(cum_miles=cumsum(miles), 
         cum_climb=cumsum(elevation),
         cum_time=cumsum(moving_time),
         n=seq_along(miles)) |> 
  group_by(year, activity) |> 
  mutate(activity_miles=cumsum(miles), 
         activity_climb=cumsum(elevation),
         activity_time=cumsum(moving_time),
         activity_rides=seq_along(miles)) |> 
  ungroup()

# Data for year-to-date plots (filtered to current day-of-year)
tracks_ytd <- tracks_data |> filter(yday <= yday(today() + days(7)))
as_of_label <- paste('Showing rides as of',
                     month(today(), label=TRUE, abbr=FALSE), mday(today()))


# Cumulative miles
plot_miles(tracks_data, cum_miles, 'Cumulative miles ridden by year')

# Miles to date
plot_miles(tracks_ytd, cum_miles, 'Cumulative miles ridden by year') +
    scale_y_continuous(labels=scales::comma) +
  labs(subtitle=as_of_label)

# Miles by activity
plot_miles(tracks_data, activity_miles, 'Cumulative miles ridden by year by activity') +
  facet_wrap(~activity)

# Cumulative climb
plot_climb(tracks_data, cum_climb, 'Cumulative feet climbed by year')

# Climb to date
plot_climb(tracks_ytd, cum_climb, 'Cumulative feet climbed by year') +
  labs(subtitle=as_of_label)

# Climb by activity
plot_climb(tracks_data, activity_climb, 'Cumulative feet climbed by year by activity') +
  facet_wrap(~activity)

# Cumulative moving time
plot_time(tracks_data, cum_time, 'Cumulative moving time by year')

# Time to date
plot_time(tracks_ytd, cum_time, 'Cumulative moving time by year') +
  labs(subtitle=as_of_label)

# Time by activity
plot_time(tracks_data, activity_time, 'Cumulative moving time by year and activity') +
  facet_wrap(~activity)

# Cumulative number of rides
plot_rides(tracks_data, n, 'Cumulative rides by year')

# Rides to date
plot_rides(tracks_ytd, n, 'Cumulative rides by year') +
  labs(subtitle=as_of_label)

# Rides by activity
plot_rides(tracks_data, activity_rides, 'Cumulative rides by year and activity') +
  facet_wrap(~activity)

# Monthly miles
(tracks_data |> 
  summarize(miles=sum(miles), .by=c(year, month)) |> 
  ggplot(aes(month, miles, fill=factor(year))) +
  geom_col(position=position_dodge(preserve='single')) +
  scale_x_continuous(breaks = 1:12, labels=month.abb, minor_breaks=NULL) +
  scale_fill_brewer(palette='Set1') +
  labs(title='Monthly miles', fill=NULL)) |> 
  plotly::ggplotly()

# Monthly climb
tracks_data |> 
  summarize(climb=sum(elevation), .by=c(year, month)) |> 
  ggplot(aes(month, climb, fill=factor(year))) +
  geom_col(position=position_dodge(preserve='single')) +
  scale_x_continuous(breaks = 1:12, labels=month.abb, minor_breaks=NULL) +
  scale_fill_brewer(palette='Set1') +
  labs(x='', y='Feet climbed', 
       title='Monthly climb', fill=NULL)

# Monthly moving time
tracks_data |> 
  summarize(moving_time=sum(moving_time), .by=c(year, month)) |> 
  ggplot(aes(month, moving_time, fill=factor(year))) +
  geom_col(position=position_dodge(preserve='single')) +
  scale_x_continuous(breaks = 1:12, labels=month.abb, minor_breaks=NULL) +
  scale_fill_brewer(palette='Set1') +
  labs(x='', y='Moving time', 
       title='Monthly moving time', fill=NULL)

# Monthly rides
tracks_data |> 
  summarize(rides=n(), .by=c(year, month)) |> 
  ggplot(aes(month, rides, fill=factor(year))) +
  geom_col(position=position_dodge(preserve='single')) +
  scale_x_continuous(breaks = 1:12, labels=month.abb, minor_breaks=NULL) +
  scale_fill_brewer(palette='Set1') +
  labs(x='', y='Number of rides', 
       title='Monthly rides', fill=NULL)

# Histogram of miles
tracks_data |> 
  ggplot(aes(miles, fill=factor(year), group=year)) +
  geom_histogram(binwidth=5, position='dodge') +
  scale_y_continuous(minor_breaks=NULL) +
  scale_fill_brewer(palette='Set1') +
  labs(title='Ride length', y='Number of rides', fill=NULL) +
  facet_grid(year ~ activity)


# Calculate average moving speed and create the plot
tracks_data |>
  mutate(avg_speed = miles / (moving_time / 60)) |>  # miles per hour
  ggplot(aes(x = miles, y = avg_speed, color = factor(activity))) +
  geom_point(alpha = 0.7) +
  geom_smooth(se=FALSE) +
  scale_color_brewer(palette = 'Set1') +
  labs(
    x = 'Distance (miles)',
    y = 'Average Moving Speed (mph)',
    title = 'Average Moving Speed vs Distance',
    color = 'Year'
  )
