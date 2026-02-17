library(tidyverse)
library(patchwork)

options = tribble(
  ~age, ~amount,
  65, 1855,
  66, 1998,
  67, 2141,
  68, 2169,
  69, 2341,
  70, 2655,
)

time_value_of_money = 0.04
base_age = 65

results <- expand_grid(
  start_age = options$age,
  current_age = 65:100
) |>
  left_join(options, by = c("start_age" = "age")) |>
  filter(current_age >= start_age) |> 
  mutate(
    months = (current_age - start_age + 1) * 12,
    total_payments = months * amount
  ) |>
  rowwise() |>
  mutate(
    present_value = sum(amount / (1 + time_value_of_money/12)^(((start_age - 65)*12 + 1:months)))
  ) |>
  ungroup()

theme_set(theme_minimal())

(p=ggplot(results, aes(current_age, present_value, color=factor(start_age), group=start_age)) +
  geom_line(linewidth=1.5) +
  geom_line(aes(y=total_payments), linewidth=1.5)+
  scale_color_brewer('Start age', palette='Set2') +
  scale_y_continuous(labels=scales::dollar_format()) +
  labs(x='Age', y='Total Payments', 
       title=str_glue('Present value at {time_value_of_money*100}%')))

p + coord_cartesian(xlim=c(75, 90), ylim=c(200000, 500000))+
  scale_x_continuous(breaks = c(75, 80, 85, 90))

wide_results <- results |>
  select(start_age, current_age, total_payments, present_value) |>
  rename(fv = total_payments, pv = present_value) |>
  pivot_wider(
    id_cols = current_age,
    names_from = start_age,
    values_from = c(fv, pv)
  ) |>
  rename(age = current_age)

# Comparison across discount rates
discount_rates <- c(0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06)

results_comparison <- expand_grid(
  start_age = options$age,
  current_age = 65:100,
  discount_rate = discount_rates
) |>
  left_join(options, by = c("start_age" = "age")) |>
  filter(current_age >= start_age) |>
  mutate(
    months = (current_age - start_age + 1) * 12,
    fv = months * amount
  ) |>
  rowwise() |>
  mutate(
    pv = sum(amount / (1 + discount_rate/12)^(((start_age - 65)*12 + 1:months)))
  ) |>
  ungroup()

ggplot(results_comparison, 
       aes(current_age, pv, color=factor(start_age), 
           group=interaction(start_age, discount_rate)
           #linetype=factor(discount_rate)
           )
       ) +
  geom_line(linewidth=1.5) +
  scale_color_brewer('Start age', palette='Set2') +
  scale_y_continuous(labels=scales::dollar_format()) +
  labs(x='Age', y='Total Payments', title='Present value')

# Analysis: Find optimal start age for each age and discount rate
optimal_strategy <- results_comparison |>
  group_by(current_age, discount_rate) |>
  slice_max(pv, n = 1) |>
  ungroup() |>
  select(current_age, discount_rate, optimal_start_age = start_age, max_pv = pv)

ggplot(optimal_strategy, aes(current_age, optimal_start_age, color=factor(discount_rate))) +
  geom_point(position=position_jitter(width=0, height=0.1), size=3) +
  scale_color_brewer(palette='Set1')

# Summary at key ages
key_ages_summary <- results_comparison |>
  filter(current_age %in% c(80, 85, 90)) |>
  select(current_age, discount_rate, start_age, pv) |>
  group_by(current_age, discount_rate) |>
  mutate(
    rank = rank(-pv),
    is_best = rank == 1
  ) |>
  ungroup()

ggplot(key_ages_summary, aes(current_age, pv, 
                             color=factor(start_age), shape=factor(discount_rate))) +
  geom_point(position=position_jitter(height=0, width=0.1), size=3) +
  scale_color_brewer(palette='Set2')

# Wide format for easy comparison at key ages
key_ages_wide <- key_ages_summary |>
  select(current_age, discount_rate, start_age, pv) |>
  pivot_wider(
    id_cols = c(current_age, discount_rate),
    names_from = start_age,
    names_prefix = "start_",
    values_from = pv
  )
