library(tidyverse)

# Rear cassette tooth counts
roubaix_rear   <- c(11, 12, 14,       16, 18, 20, 22, 25,28,32)
scott_rear     <- c(11, 12, 13, 14, 15, 17, 19, 21, 24, 27, 30, 34)
scott_alt_rear <- c(11, 12, 13, 14, 15, 17, 19, 21, 24, 28, 32, 36)  # Shimano CS-R7100 11-36

# All gear combinations
gears <- bind_rows(
  expand.grid(bike = "Roubaix",          chainring = c(34L, 50L), sprocket = roubaix_rear),
  expand.grid(bike = "Scott Addict",     chainring = c(34L, 50L), sprocket = scott_rear),
  expand.grid(bike = "Scott Addict Alt", chainring = c(34L, 50L), sprocket = scott_alt_rear)
) |>
  mutate(
    bike = factor(bike, levels = c("Roubaix", "Scott Addict", "Scott Addict Alt")),
    ratio = chainring / sprocket,
    chainring_label = factor(
      chainring,
      levels = c(50, 34),
      labels = c("50t chainring — high range", "34t chainring — low range")
    )
  )

# Range bars (min to max ratio per combo)
gear_ranges <- gears |>
  summarise(lo = min(ratio), hi = max(ratio), .by = c(bike, chainring_label))

bike_colors <- c(
  "Roubaix"          = "#C8102E",
  "Scott Addict"     = "#1A4F9C",
  "Scott Addict Alt" = "#2A8C55"
)
bike_shapes <- c(
  "Roubaix"          = 16,
  "Scott Addict"     = 17,
  "Scott Addict Alt" = 15
)

# Plot 1: strip chart — gear ratio by bike/chainring combo
ggplot(gears, aes(x = ratio, y = bike, color = bike, shape = bike)) +
  geom_segment(
    data = gear_ranges,
    aes(x = lo, xend = hi, y = bike, yend = bike, color = bike),
    linewidth = 4, alpha = 0.12, show.legend = FALSE
  ) +
  geom_point(size = 3) +
  geom_text(
    aes(label = sprocket),
    vjust = -0.75, size = 2.6, show.legend = FALSE
  ) +
  facet_wrap(~chainring_label, ncol = 1) +
  scale_color_manual(values = bike_colors, name = NULL) +
  scale_shape_manual(values = bike_shapes, name = NULL) +
  scale_x_continuous(
    name = "Gear ratio (chainring ÷ rear sprocket)",
    breaks = seq(1, 5, 0.5),
    minor_breaks = seq(1, 5, 0.25)
  ) +
  scale_y_discrete(expand = expansion(add = 0.8)) +
  labs(
    title = "Gear Ratios: Specialized Roubaix vs. Scott Addict",
    subtitle = paste(
      "Roubaix: 10-speed 11–32t",
      "Scott Addict: 12-speed 11–34t",
      "Scott Addict Alt: 12-speed 11–36t (Shimano 105 Di2 CS-R7100)",
      sep = "  ·  "
    ),
    y = NULL,
    caption = "Each point is one gear combination. Labels show rear sprocket size. Higher ratio = harder gear."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    legend.key.size = unit(1.2, "lines"),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.subtitle = element_text(size = 9.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50"),
    plot.title.position = "plot"
  )

# Plot 2: line plot — gear ratio vs rear sprocket size
ggplot(gears |> filter(bike != 'Roubaix'), aes(
  x = sprocket, y = ratio,
  color = bike, shape = bike, linetype = chainring_label,
  group = interaction(bike, chainring_label)
)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5, position='jitter') +
  scale_color_manual(values = bike_colors, name = NULL) +
  scale_shape_manual(values = bike_shapes, name = NULL) +
  scale_linetype_manual(
    values = c("50t chainring — high range" = "solid", "34t chainring — low range" = "dashed"),
    name = "Chainring"
  ) +
  scale_x_continuous(
    name = "Rear sprocket (teeth)",
    breaks = sort(unique(gears$sprocket)),
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    name = "Gear ratio (chainring ÷ sprocket)",
    trans = "log2",
    breaks = c(1, 1.5, 2, 2.5, 3, 4),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  labs(
    title = "Gear Ratios by Rear Sprocket Size",
    subtitle = paste(
      "Roubaix: 10-speed 11–32t",
      "Scott Addict: 12-speed 11–34t",
      "Scott Addict Alt: 12-speed 11–36t (Shimano 105 Di2 CS-R7100)",
      sep = "  ·  "
    ),
    caption = "Solid lines = 50t chainring (high range). Dashed = 34t chainring (low range)."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.subtitle = element_text(size = 9.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50"),
    plot.title.position = "plot"
  )

# Plot 3: proportional step size between consecutive cassette positions
steps <- gears |>
  distinct(bike, sprocket) |>
  arrange(bike, sprocket) |>
  mutate(
    sprocket_next = lead(sprocket),
    step_pct = (sprocket_next / sprocket - 1) * 100,
    .by = bike
  ) |>
  filter(!is.na(sprocket_next))

ggplot(steps, aes(x = sprocket, y = step_pct, color = bike, shape = bike, group = bike)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(values = bike_colors, name = NULL) +
  scale_shape_manual(values = bike_shapes, name = NULL) +
  scale_x_continuous(
    name = "Rear sprocket — start of step (teeth)",
    breaks = sort(unique(steps$sprocket)),
    guide = guide_axis(check.overlap = TRUE)
  ) +
  scale_y_continuous(
    name = "Step size (% change in gear ratio)",
    labels = scales::label_percent(scale = 1, accuracy = 1)
  ) +
  labs(
    title = "Proportional Step Size Between Consecutive Gears",
    subtitle = paste(
      "Roubaix: 10-speed 11–32t",
      "Scott Addict: 12-speed 11–34t",
      "Scott Addict Alt: 12-speed 11–36t (Shimano 105 Di2 CS-R7100)",
      sep = "  ·  "
    ),
    caption = "Each point = % change in gear ratio when shifting from that sprocket to the next larger one."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.subtitle = element_text(size = 9.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50"),
    plot.title.position = "plot"
  ) +
  facet_grid(bike~.)
