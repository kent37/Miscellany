# Analyze heating oil use
library(tidyverse)
library(lubridate)

purchases_raw = read_csv('Oil/Oil_purchases.csv', comment='#')

purchases = purchases_raw |> 
  mutate(purchase_date = mdy(date),
         purchase_year = year(purchase_date),
         previous_date=lag(purchase_date),
         previous_year = year(previous_date),
         daily_use = gallons/as.integer(purchase_date-previous_date))

# Make break points and labels
breaks = tibble(
  date=seq.Date(ymd('2023-01-01'), ymd('2023-12-1'), by='month'),
  year_day=yday(date),
  label=format(date, '%b'))

lw = 1

# We have to add points at the beginning and end of each year
# to make step charts by year
same_year = purchases |> 
  filter(purchase_year==previous_year) |> 
  select("previous_date", "previous_year", 
         "purchase_date", "purchase_year", "daily_use")
diff_year = purchases |> 
  filter(purchase_year!=previous_year) |> 
  select("previous_date", "previous_year", 
         "purchase_date", "purchase_year", "daily_use")

diff_year = diff_year |> 
  rowwise() |> 
  reframe(
    previous_date = c(previous_date, mdy(paste('1/1/', purchase_year))),
    purchase_date = c(mdy(paste('12/31/', previous_year)), purchase_date),
     daily_use = daily_use
  ) |> 
  mutate(
    previous_year = year(previous_date),
    purchase_year = year(purchase_date)
  )

chart_data = bind_rows(same_year, diff_year) |> 
  mutate(previous_day=yday(previous_date),
         purchase_day=yday(purchase_date),
         year=factor(previous_year))

# Daily use grouped by year
ggplot(chart_data) +
  # geom_point(aes(previous_day, daily_use, color=year, group=year), size=2) +
  geom_step(aes(previous_day, daily_use, color=year, group=year), linewidth=lw)+
  geom_segment(aes(x=previous_day, xend=purchase_day,
                     y=daily_use, yend=daily_use, color=year),
                 linewidth=lw) +
  scale_x_continuous(breaks=breaks$year_day, labels=breaks$label, 
                     minor_breaks=NULL) +
  scale_color_brewer(palette='Set1') +
  ylim(0, NA) +
  labs(x='Date', y='Average daily use (Gal)', color='Year',
       title='Average daily oil use (Gal)') +
  theme_minimal() +
  theme(axis.text.x=element_text(hjust=-0.4))

# Price
ggplot(purchases, aes(purchase_date, unit_price)) +
  geom_line() +
  theme_minimal()
