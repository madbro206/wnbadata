#how to get the WNBA data

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, tidyr, glue, tictoc, progressr, ggplot2)


#wnba full play by play
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp(season=c(2024))
})
tictoc::toc()

data <- select(wnba_pbp, game_id, game_date, game_play_number, id, sequence_number, type_text, text, away_score, home_score, period_number, time, scoring_play, score_value, team_id, home_team_name, away_team_name, end_game_seconds_remaining)


#count points per basket (1, 2, or 3)
result <- data %>%
  filter(scoring_play == TRUE) %>%
  group_by(score_value) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

print(result)


#count points per play (1-6pts)
result2 <- data %>%
  filter(scoring_play == TRUE) %>%
  group_by(game_id, team_id, period_number, time) %>%
  summarise(total_points = sum(score_value), .groups = "drop") %>%
  count(total_points) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  arrange(total_points)

print(result2)

ggplot(result2, aes(x = factor(total_points), y = percentage)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f%%", percentage)), vjust = -0.5) +
  labs(x = "Total Points per Scoring Play", y = "Percentage", 
       title = "Distribution of Points per Scoring Play") +
  theme_minimal()
  
  
#return 4 point plays 
four_point_plays <- data %>%
  filter(scoring_play == TRUE) %>%
  group_by(game_id, time, period_number, team_id) %>%
  summarise(total_points = sum(score_value), .groups = "drop") %>%
  filter(total_points == 4)

# Retrieve the original rows for 5-point scoring plays
original_rows_four_point_plays <- data %>%
  inner_join(four_point_plays, by = c("game_id", "time", "period_number", "team_id"))

print(original_rows_four_point_plays)


#return 5 point plays 
five_point_plays <- data %>%
  filter(scoring_play == TRUE) %>%
  group_by(game_id, time, period_number, team_id) %>%
  summarise(total_points = sum(score_value), .groups = "drop") %>%
  filter(total_points == 5)

# Retrieve the original rows for 5-point scoring plays
original_rows_five_point_plays <- data %>%
  inner_join(five_point_plays, by = c("game_id", "time", "period_number", "team_id"))

print(original_rows_five_point_plays)
