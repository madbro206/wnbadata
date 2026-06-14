#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html
# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, ggplot2, scales, grid)

#load data
#wnba full play by play
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(season=c(2026))
})
tictoc::toc()


#aggregate player FT
player_ft <- wnba_player_box |>
  filter(season == 2026) |>
  group_by(athlete_id, athlete_display_name) |>
  summarise(
    ft_made = sum(free_throws_made, na.rm = TRUE),
    ft_att  = sum(free_throws_attempted, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    ft_pct = if_else(ft_att > 0, ft_made / ft_att, NA_real_)
  ) |>
  filter(!is.na(ft_pct))


plum_name  <- "Kelsey Plum"
plum_color <- "#D55E00"  

plum_ft <- player_ft |>
  filter(athlete_display_name == plum_name)

league_avg <- mean(player_ft$ft_pct, na.rm = TRUE)

ggplot(player_ft, aes(x = 0, y = ft_pct)) +
  geom_jitter(
    aes(color = ft_att),
    width = 0.08,
    height = 0,
    size = 2.7,
    alpha = 0.9
  ) +
  scale_color_viridis_c(
    option = "viridis",
    direction = 1,
    trans = "sqrt",
    guide = "none"
  ) +
  geom_hline(
    yintercept = league_avg,
    linetype = "dashed",
    linewidth = 0.7,
    color = "black"
  ) +
  geom_point(
    data = plum_ft,
    aes(x = 0, y = ft_pct),
    color = plum_color,
    size = 4.6
  ) +
  geom_text(
    data = plum_ft,
    aes(
      x = 0.13,
      y = ft_pct + 0.05,
      label = paste0(plum_name, "\n", percent(ft_pct, accuracy = 0.1))
    ),
    color = plum_color,
    hjust = 0,
    size = 3.8,
    fontface = "bold"
  ) +
  # arrow from label down to point
  annotate(
    "segment",
    x = plum_ft$ft_pct * 0 + 0.11,         # start a bit left of label
    xend = 0,                              # Plum's x
    y = plum_ft$ft_pct + 0.03,             # slightly below label
    yend = plum_ft$ft_pct,                 # Plum's y
    colour = plum_color,
    linewidth = 0.6,
    arrow = arrow(length = unit(0.15, "cm"), type = "closed")
  ) +
  annotate(
    "text",
    x = -0.13,
    y = league_avg+.025,
    label = "League avg",
    hjust = 1,
    size = 3.2,
    color = "black"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.02)
  ) +
  coord_cartesian(xlim = c(-0.18, 0.30), clip = "off") +
  labs(
    x = NULL,
    y = "free throw percentage",
    title = "Kelsey Plum vs. WNBA",
    subtitle = "each dot is one player in 2026\ndarker dots have more FT attempts",
    caption = "chart: @wnbadata | data: wehoop"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 13),
    axis.title.y = element_text(size = 15),
    plot.caption = element_text(size = 11),
    plot.margin = margin(15, 45, 15, 45)
  )