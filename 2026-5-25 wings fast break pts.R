if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::pkg_install(c("wehoop", "dplyr", "glue", "progressr", "tictoc"))
library(ggplot2)
library(dplyr)
library(patchwork)

tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season=c(2023:2026))
})
tictoc::toc()


wnba_team_box2 <- wnba_team_box %>% 
  filter(fast_break_points >=0) %>%
  mutate(
    fast_break_points = as.numeric(fast_break_points),
    team_winner = as.logical(team_winner)
  ) %>% 
  group_by(game_id) %>% 
  arrange(game_id, team_home_away) %>% 
  mutate(
    fb_diff = fast_break_points - rev(fast_break_points)
  ) %>% 
  ungroup()

wnba_team_box2 <- wnba_team_box2 %>% 
  mutate(margin = team_score - opponent_team_score)

ggplot(wnba_team_box2, aes(x = fb_diff, y = margin)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Fast break point advantage",
    subtitle= "WNBA 2023-2026",
    y = "Point differential",
    title = "Fast break point margin vs game final margin",
    caption= "data: wehoop | chart: @wnbadata"
  ) +
  theme_minimal()

cor(
  wnba_team_box2$fb_diff,
  wnba_team_box2$margin,
  use = "complete.obs"
)

wnba_team_box2 %>% 
  mutate(
    fb_result = case_when(
      fb_diff > 0  ~ "win",
      fb_diff < 0  ~ "loss",
      TRUE         ~ "tie"   # this is the fb_diff == 0 case
    )
  ) %>% 
  group_by(fb_result) %>% 
  summarise(
    n = n(),
    win_pct = mean(team_winner),
    .groups = "drop"
  )

wnba_team_box3 <- wnba_team_box2 %>% 
  mutate(
    fb_win = fb_diff > 0
  ) 


wnba_team_box4 <- wnba_team_box %>% 
  filter(season_type==2) %>%
  mutate(
    fast_break_points = as.numeric(fast_break_points),
    team_score        = as.numeric(team_score),
    fb_pct_of_points  = fast_break_points / team_score
  )

team_season_fb <- wnba_team_box4 %>% 
  group_by(season, team_id, team_abbreviation, team_name) %>% 
  summarise(
    games_played        = n(),
    total_points        = sum(team_score, na.rm = TRUE),
    total_fb_points     = sum(fast_break_points, na.rm = TRUE),
    avg_fb_points       = mean(fast_break_points, na.rm = TRUE),
    avg_fb_pct_of_pts   = mean(fb_pct_of_points, na.rm = TRUE),
    .groups = "drop"
  )

top_avg_fb <- team_season_fb %>% 
  arrange(desc(avg_fb_points)) %>% 
  slice_head(n = 10)

cat("\n===== TOP 10 by fast break ppg 2026 =====\n")
team_season_fb %>% 
  filter(season==2026) %>%
  arrange(desc(avg_fb_points)) %>% 
  slice_head(n = 15)  %>% select(season, team_name, games_played, avg_fb_points)

cat("\n===== TOP 10 by fast break ppg since 2023 =====\n")
top_avg_fb %>% select(season, team_name, games_played, avg_fb_points)

top_total_fb <- team_season_fb %>% 
  arrange(desc(total_fb_points)) %>% 
  slice_head(n = 10)

top_total_fb

top_fb_share <- team_season_fb %>% 
  arrange(desc(avg_fb_pct_of_pts)) %>% 
  slice_head(n = 10)

top_fb_share



ggplot(
  wnba_team_box2,
  aes(x = fast_break_points, fill = team_winner)
) +
  geom_histogram(
    position = "identity",  # overlay, not stack
    alpha    = 0.5,         # transparency so overlap is visible
    bins     = 15,          # tweak as needed
    color    = "black"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02"),
    labels = c("Loss", "Win")    # if team_winner is logical, order is FALSE/TRUE
  ) +
  labs(
    x = "Fast break points",
    y = "Number of team-games",
    fill = "Game result",
    title = "Distribution of fast break points in wins vs losses"
  ) +
  theme_minimal()



# 1. Compute common axis ranges
x_range <- range(wnba_team_box2$fast_break_points, na.rm = TRUE)

# if you want the same y-scale too, get the max bin height across both subsets
win_counts <- wnba_team_box2 %>% 
  filter(team_winner) %>% 
  count(fast_break_points)

loss_counts <- wnba_team_box2 %>% 
  filter(!team_winner) %>% 
  count(fast_break_points)

y_max <- 200

library(ggplot2)

p_win <- wnba_team_box2 %>% 
  filter(team_winner) %>% 
  ggplot(aes(x = fast_break_points)) +
  geom_histogram(
    bins  = 15,
    fill  = "#1b9e77",
    color = "black"
  ) +
  scale_x_continuous(limits = x_range) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(
    title = "Fast break points in wins",
    x = "Fast break points",
    y = "Number of team-games"
  ) +
  theme_minimal()

p_loss <- wnba_team_box2 %>% 
  filter(!team_winner) %>% 
  ggplot(aes(x = fast_break_points)) +
  geom_histogram(
    bins  = 15,
    fill  = "#d95f02",
    color = "black"
  ) +
  scale_x_continuous(limits = x_range) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(
    title = "Fast break points in losses",
    x = "Fast break points",
    y = NULL
  ) +
  theme_minimal()



p_win + p_loss
