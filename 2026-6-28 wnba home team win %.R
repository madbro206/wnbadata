# ============================
# Libraries
# ============================
library(wehoop)
library(progressr)
library(tictoc)
library(dplyr)
library(tidyr)
library(ggplot2)


# ============================
# 0. Load data
# ============================
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season = 2006:2026)
})
tictoc::toc()

# keep regular season only
wnba_team_box <- wnba_team_box %>%
  filter(season_type == 2)

colnames(wnba_team_box)

#total home win%
wnba_team_box %>%
  filter(team_home_away == "home") %>%
  summarise(
    games = n(),
    home_wins = sum(team_winner, na.rm = TRUE),
    home_win_pct = mean(team_winner, na.rm = TRUE) * 100
  ) 

home_win_pct <- wnba_team_box %>%
  filter(team_home_away == "home") %>%
  group_by(season) %>%
  summarise(
    games = n(),
    home_wins = sum(team_winner, na.rm = TRUE),
    home_win_pct = mean(team_winner, na.rm = TRUE) * 100
  ) %>%
  arrange(season)

home_win_pct%>% arrange(home_win_pct)


historical <- home_win_pct %>% filter(season != 2026)
current    <- home_win_pct %>% filter(season == 2026)

hist_mean <- mean(historical$home_win_pct)
hist_sd   <- sd(historical$home_win_pct)

z_score <- (current$home_win_pct - hist_mean) / hist_sd

hist_mean; hist_sd; z_score



# Get distinct games per season, ordered by date, keep first 137
first_137_games <- wnba_team_box %>%
  distinct(season, game_id, game_date) %>%
  group_by(season) %>%
  arrange(game_date) %>%
  slice_head(n = 137) %>%
  ungroup()

# filter the home-team rows to just those games
home_win_pct_first137 <- wnba_team_box %>%
  filter(team_home_away == "home") %>%
  semi_join(first_137_games, by = c("season", "game_id")) %>%
  group_by(season) %>%
  summarise(
    games = n(),
    home_wins = sum(team_winner, na.rm = TRUE),
    home_win_pct = mean(team_winner, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(season))

home_win_pct_first137


hist_137 <- home_win_pct_first137 %>% filter(season != 2026)
curr_137  <- home_win_pct_first137 %>% filter(season == 2026)

mean_137 <- mean(hist_137$home_win_pct)
sd_137   <- sd(hist_137$home_win_pct)
z_137    <- (curr_137$home_win_pct - mean_137) / sd_137
p_137    <- pnorm(z_137)  # one-tailed: probability of seeing this or lower

cat("Historical mean:", round(mean_137, 1), "%\n")
cat("Historical SD:  ", round(sd_137, 1), "%\n")
cat("2026 z-score:   ", round(z_137, 2), "\n")
cat("Percentile:     ", round(p_137 * 100, 1), "%\n")


# Probability of winning 9 or fewer out of 30 if true p = 0.571
pbinom(9, size = 30, prob = 0.571)

# ============================
# Cumulative home win% by game number — last 10 seasons
# ============================
# Step 1: one clean cumulative series per season, all games, no cap
cumulative_home <- wnba_team_box %>%
  filter(team_home_away == "home", !is.na(team_winner)) %>%
  distinct(season, game_id, game_date, team_winner) %>%
  arrange(season, game_date, game_id) %>%
  group_by(season) %>%
  mutate(
    game_num        = row_number(),
    cumulative_wins = cumsum(as.integer(team_winner)),
    cum_win_pct     = cumulative_wins / game_num * 100
  ) %>%
  ungroup()

# Step 2: just filter to last 10 seasons — no re-summarising, no re-computing
last10_seasons <- cumulative_home %>%
  filter(season >= 2016)

# Step 3: palette
seasons_vec <- sort(unique(last10_seasons$season))
n_seasons   <- length(seasons_vec)

season_colors <- setNames(
  c(rep("grey70", n_seasons - 1), "#E05A2B"),
  seasons_vec
)
season_sizes <- setNames(
  c(rep(0.4, n_seasons - 1), 1.2),
  seasons_vec
)
season_alpha <- setNames(
  c(rep(0.5, n_seasons - 1), 1),
  seasons_vec
)

ggplot(last10_seasons, aes(
  x         = game_num,
  y         = cum_win_pct,
  group     = factor(season),
  color     = factor(season),
  linewidth = factor(season),
  alpha     = factor(season)
)) +
  geom_line(lineend = "round", linejoin = "round") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  scale_color_manual(values = season_colors, name = "Season") +
  scale_linewidth_manual(values = season_sizes, name = "Season") +
  scale_alpha_manual(values = season_alpha, name = "Season") +
  scale_x_continuous(
    breaks = seq(0, 300, by = 20),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = scales::label_number(suffix = "%")
  ) +
  labs(
    title    = "WNBA cumulative home win % by game (2016–2026)",
    subtitle = "2026 highlighted in orange; dashed line = 50%",
    x        = "Home games played (regular season)",
    y        = "Cumulative home win %",
    caption  = "data: wehoop | chart: @wnbadata"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.key.size  = unit(0.6, "lines")
  )


# Get the final point for 2026
label_2026 <- last10_seasons %>%
  filter(season == 2026) %>%
  slice_tail(n = 1)

ggplot(last10_seasons, aes(
  x         = game_num,
  y         = cum_win_pct,
  group     = factor(season),
  color     = factor(season),
  linewidth = factor(season),
  alpha     = factor(season)
)) +
  geom_line(lineend = "round", linejoin = "round") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey40", linewidth = 0.4) +
    geom_text(
      data  = label_2026,
      aes(x = game_num, y = cum_win_pct, label = paste0(round(cum_win_pct, 1), "%")),
      color = "#E05A2B",
      fontface = "bold",
      hjust = -0.2,
      vjust = 0.4,
      size  = 4,
      inherit.aes = FALSE
    ) +
  scale_color_manual(values = season_colors, name = "Season") +
  scale_linewidth_manual(values = season_sizes, name = "Season") +
  scale_alpha_manual(values = season_alpha, name = "Season") +
  scale_x_continuous(
    breaks = seq(0, 300, by = 20),
    expand = expansion(mult = c(0.01, 0.08)) 
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = scales::label_number(suffix = "%")
  ) +
  labs(
    title    = "WNBA cumulative home win % by game (2016–2026)",
    subtitle = "2026 highlighted in orange; dashed line = 50%",
    x        = "Home games played (regular season)",
    y        = "Cumulative home win %",
    caption  = "data: wehoop | chart: @wnbadata"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.key.size  = unit(0.6, "lines")
  )
