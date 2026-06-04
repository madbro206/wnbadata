library(wehoop)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(ggrepel)

#download data
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season=c(2010:2026))
})
tictoc::toc()

games <- wnba_team_box %>%
  filter(!str_detect(team_name, regex("team", ignore_case = TRUE))) %>% #filter out all star teams
  filter(!str_detect(team_name, regex("west", ignore_case = TRUE))) %>%
  filter(!str_detect(team_name, regex("east", ignore_case = TRUE))) %>%
  select(season, game_id, game_date, team_name, team_score, opponent_team_score, team_winner)

teams <- games %>%
  group_by(season, team_name) %>%
  summarize(pts_scored = sum(team_score), pts_allowed = sum(opponent_team_score), wins=sum(team_winner), games=n(), losses=games-wins, win_pct = wins/games)

#pythagorean expectation with exponent 2
pythag <- teams %>%
  mutate(pred_win_pct = (pts_scored^2)/(pts_scored^2+pts_allowed^2)) %>%
  mutate(diff = win_pct-pred_win_pct) %>%
  arrange(desc(diff))


################
#find optimal exponent for wnba

pythag_mse <- function(p, data = teams %>%filter(season!=2026)) { #leave incomplete 2026 season out
  preds <- data %>%
    mutate(
      pred_win_pct = (pts_scored^p) / (pts_scored^p + pts_allowed^p),
      sq_err       = (win_pct - pred_win_pct)^2
    )
  
  mean(preds$sq_err, na.rm = TRUE)
}

p_grid <- seq(5, 20, by = 0.05) 

results <- tibble(
  p   = p_grid,
  mse = map_dbl(p_grid, pythag_mse)
)

results_best <- results %>% arrange(mse) %>% slice(1)
results_best

best_p <- results_best$p

pythag <- teams %>%
  mutate(
    pred_win_pct = (pts_scored^best_p) / (pts_scored^best_p + pts_allowed^best_p),
    diff         = win_pct - pred_win_pct
  ) %>%
  arrange(desc(diff))

pythag

#exponents plot
ggplot(results, aes(p, mse)) +
  geom_line(linewidth = 1.5) +
  geom_vline(xintercept = best_p, linetype = "dashed",
             color = "red", linewidth = 1.3) +
  labs(
    x = "Pythagorean exponent p",
    y = "Mean squared error of win%",
    title = "WNBA Pythagorean exponent search",
    subtitle = "using WNBA data 2010-2025"
  ) +
  theme_minimal(base_size = 18) +  # raise global text size
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    axis.title = element_text(size = 25),
    axis.text  = element_text(size = 16)
  )

############### teams plot #############

# flag 2026 teams
teams_plot <- pythag %>%
  mutate(is_2026 = season == 2026)


ggplot(teams_plot, aes(x = pred_win_pct, y = win_pct)) +
  # all seasons in gray
  geom_point(color = "gray65", size = 2.5) +
  # 45-degree line: teams exactly matching expectation
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "black", linewidth = 1.1) +
  # highlight 2026 teams in color
  geom_point(
    data = filter(teams_plot, is_2026),
    aes(x = pred_win_pct, y = win_pct),
    color = "blue",
    size = 4
  ) +
  # label 2026 points with team_name
  ggrepel::geom_text_repel(
    data = filter(teams_plot, is_2026),
    aes(label = team_name),
    color = "blue",
    size = 5,
    fontface = "bold" 
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Expected win%",
    y = "Actual win%",
    title = "WNBA team actual vs expected win%",
    subtitle = "Pythagorean exponent=11, 2026 teams in blue"
  ) +
  coord_equal() +
  theme_minimal(base_size = 18) +  # global text size bump
  theme(
    plot.title    = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 18),
    axis.title    = element_text(size = 18),
    axis.text     = element_text(size = 16)
  )



########### top teams tables ############
# overachievers:
pythag %>%
  select(season, team_name, pts_scored, pts_allowed, win_pct, pred_win_pct, diff)


# underachievers: actual < expected
pythag %>%
  arrange(diff) %>%
  select(season, team_name, pts_scored, pts_allowed, win_pct, pred_win_pct, diff)


print(pythag, n=124)
