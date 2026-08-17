# ============================================================
# Jessica Shepard (2026) vs Candace Parker (2015)
# Cumulative PPG / RPG / APG vs the 10/10/5 threshold
# ============================================================
 
# install.packages(c("tidyverse", "janitor"))  # run once if you don't have these
library(tidyverse)
library(janitor)
library(patchwork)
 
# ---- 1. IMPORT ----------------------------------------------
# Save your two box-score exports as "js.csv" and "cp.csv" in the same
# folder as this script (or edit the paths below). janitor::clean_names()
# handles messy Basketball-Reference headers (e.g. "3P%" -> "x3p_percent")
# so this works even if the raw export doesn't match exactly.

js_raw <- read_csv("/Users/maddy/Desktop/wehoop/2026-7-19 jessica shepard is candace parker/js_2026.csv") %>% clean_names()
cp_raw <- read_csv("/Users/maddy/Desktop/wehoop/2026-7-19 jessica shepard is candace parker/cp_2015.csv") %>% clean_names()

# Keep just what we need and tag each player
js <- js_raw %>%
  transmute(game_num = row_number(), pts, trb, ast, player = "Shepard '26")
 
cp <- cp_raw %>%
  transmute(game_num = row_number(), pts, trb, ast, player = "Parker '15")

# ---- 2. CUMULATIVE AVERAGES ----------------------------------
 
cume <- bind_rows(js, cp) %>%
  group_by(player) %>%
  arrange(game_num, .by_group = TRUE) %>%
  mutate(
    cum_ppg = cummean(pts),
    cum_rpg = cummean(trb),
    cum_apg = cummean(ast)
  ) %>%
  ungroup()
 
# ---- 3. COLORS (validated CVD-safe pair) ----------------------
 
player_colors <- c("Shepard '26" = "#2a78d6", "Parker '15" = "#eb6834")
 
# ---- 4. SHARED THEME -------------------------------------------
 
theme_hooks <- theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    axis.title = element_text(color = "grey40", size = 10)
  )
 
# ---- 5. CHART BUILDER FUNCTION ----------------------------------
 
make_pace_chart <- function(data, y_col, threshold, y_label, title, subtitle) {
  end_points <- data |>
    group_by(player) |>
    filter(game_num == max(game_num)) |>
    ungroup()

  ggplot(data, aes(x = game_num, y = .data[[y_col]], color = player)) +
    geom_hline(yintercept = threshold, linetype = "dashed", color = "grey55", linewidth = 0.6) +
    annotate("text", x = max(data$game_num), y = threshold, label = paste0(threshold, " ", y_label, " threshold"),
             hjust = 1, vjust = -0.6, size = 3, color = "grey45") +
    geom_line(linewidth = 1) +
    geom_point(size = 1.6) +
    geom_text(
      data = end_points,
      aes(label = round(.data[[y_col]], 1)),
      vjust = -1, fontface = "bold", size = 3.5, show.legend = FALSE
    ) +
    scale_color_manual(values = player_colors) +
    scale_x_continuous(breaks = seq(0, max(data$game_num), by = 4)) +
    scale_y_continuous(
        limits = c(0, NA),
        expand = expansion(mult = c(0.02, 0.12)),
        breaks = scales::breaks_width(1)
      ) +
    labs(title = title, subtitle = subtitle, x = "Game number", y = y_label) +
    theme_hooks
}
 
# ---- 6. BUILD THE THREE CHARTS -----------------------------------
 
chart_pts <- make_pace_chart(
  cume, "cum_ppg", threshold = 10, y_label = "PPG",
  title = "Cumulative points per game",
  subtitle = "Threshold: 10 PPG"
)
 
chart_reb <- make_pace_chart(
  cume, "cum_rpg", threshold = 10, y_label = "RPG",
  title = "Cumulative rebounds per game",
  subtitle = "Threshold: 10 RPG - Parker dipped under it games 8-10"
)
 
chart_ast <- make_pace_chart(
  cume, "cum_apg", threshold = 5, y_label = "APG",
  title = "Cumulative assists per game",
  subtitle = "Threshold: 5 APG"
)
 
# ---- 7. VIEW + SAVE -----------------------------------------------
 
chart_pts
chart_reb
chart_ast

