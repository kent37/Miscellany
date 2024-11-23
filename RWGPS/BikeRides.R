library(tidyverse)
library(lubridate)
library(sf)

gps_path = here::here('RWGPS/rwgps_backup/')
gps_files = list.files(gps_path, '*.gpx', full.names=TRUE)

st_layers(gps_files[1])
track = st_read(gps_files[1], layer='tracks')
pts = st_read(gps_files[1], layer='track_points')
time = pts$time[[1]]

# Read a GPX, return length in feet and start time
read_gpx = function(path) {
  track = read_sf(path, layer='tracks')
  length = st_length(st_transform(track, 2249))
  pts = read_sf(path, layer='track_points')
  time = pts$time[[1]]
  tibble(length=length, time=time)
}

tracks_meta = map(gps_files, read_gpx, .progress=TRUE) |> 
  list_rbind()

tracks_data = tracks_meta |> 
  arrange(time) |> 
  mutate(year=year(time), month=month(time), yday=yday(time), 
         miles=unclass(length/5280)) |> 
  group_by(year) |> 
  mutate(cum_miles=cumsum(miles)) |> 
  ungroup()

# Make break points and labels
breaks = tibble(
  date=seq.Date(ymd('2023-01-01'), ymd('2023-12-1'), by='month'),
  year_day=yday(date),
  label=format(date, '%b'))

# Cumulative miles
ggplot(tracks_data, aes(yday, cum_miles, color=factor(year))) +
  geom_line() +
  scale_x_continuous(breaks=breaks$year_day, labels=breaks$label, 
                     minor_breaks=NULL) +
  scale_y_continuous(limits=c(0, 3000), labels=scales::comma) +
  labs(x='', y='Miles', title='Cumulative miles ridden by year', color='') +
  theme_minimal() +
  theme(axis.text.x=element_text(hjust=-0.2),
          plot.title=element_text(face='bold', size=rel(1.5)))

# Monthly miles
tracks_data |> 
  summarize(miles=sum(miles), .by=c(year, month)) |> 
  ggplot(aes(month, miles, fill=factor(year))) +
  geom_col(position=position_dodge(preserve='single')) +
  scale_x_continuous(breaks = 1:12, labels=month.abb, minor_breaks=NULL) +
  theme_minimal()
