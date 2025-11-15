pacman::p_load(wehoop, dplyr, tidyr, tictoc, progressr, ggplot2)

#load WNBA data
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season=c(2024:2025))
})
tictoc::toc()

games <- wnba_team_box %>%
  filter(team_home_away == "home") %>%
  mutate(
    game_date = as.Date(game_date),
    game_type = case_when(
      # Commissioner's Cup games
      season == 2024 & 
        game_date >= as.Date("2024-06-01") & 
        game_date <= as.Date("2024-06-13") ~ "Commissioner's Cup",

      #cc finals
      game_date==as.Date("2024-06-25") ~"Commissioner's Cup",
      game_date==as.Date("2025-07-01") ~"Commissioner's Cup",
      
      season == 2025 & 
        game_date >= as.Date("2025-06-01") & 
        game_date <= as.Date("2025-06-17") ~ "Commissioner's Cup",
      
      # Playoff games
      season_type == 3 ~ "Playoffs",
      
      # Regular season (non-Cup)
      season_type==2 ~ "Regular Season"
    ),
    winner_score = pmax(team_score, opponent_team_score),
    loser_score = pmin(team_score, opponent_team_score)
  )

#plot
ggplot(games, aes(x = winner_score, y = loser_score, color = game_type)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_manual(
    values = c(
      "Regular Season" = "gray60",
      "Commissioner's Cup" = "#FF6600",
      "Playoffs" = "#00A9E0"
    ),
    name = "Game Type"
  ) +
  labs(x = "Winner Score", y = "Loser Score") +
  theme_minimal()


#########################################################
#game margin distribution
# Calculate game margin
games <- games %>%
  mutate(margin = winner_score - loser_score)

# Summary statistics by game type
games %>%
  group_by(game_type) %>%
  summarise(
    n = n(),
    median_margin = median(margin),
    close_games_pct = mean(margin <= 10) * 100
  )

# Visualize with boxplot
ggplot(games, aes(x = game_type, y = margin, fill = game_type)) +
  geom_boxplot() +
  labs(y = "Game Margin (points)", x = "Game Type") +
  theme_minimal()



ggplot(games, aes(x = game_type, y = margin, fill = game_type)) +
  geom_boxplot() +
  labs(
    title = "Competitiveness by Game Type",
    subtitle = "WNBA (2024-2025)",
    y = "Game Margin (points)", 
    x = ""  # Remove x-axis label since categories are self-explanatory
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since colors match x-axis
