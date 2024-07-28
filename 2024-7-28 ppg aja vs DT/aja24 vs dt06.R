#wehoop data
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, ggplot2)

#load box score data for 2006 and 2024
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(seasons=c(2024,2006))
})
tictoc::toc()

#take only DT in 06 and AW in 24
wnba_player_box <- subset(wnba_player_box, (athlete_display_name=="A'ja Wilson" & season==2024) | (athlete_display_name=="Diana Taurasi" & season==2006))

#correcting DT's points, take out all star games
wnba_player_box$points[wnba_player_box$game_date == as.Date("2006-06-02")] <- 16
wnba_player_box <- wnba_player_box %>% 
  filter(team_name != "West")
wnba_player_box <- wnba_player_box %>% 
  filter(team_name != "Team USA")

#calculate total points
total_points <- wnba_player_box %>%
  group_by(athlete_id, athlete_display_name) %>%
  summarise(total_points = sum(points, na.rm = TRUE)) %>%
  arrange(desc(total_points))

# Print the resulting dataframe
print(total_points)


#count number of games played by each player
#Group by athlete_id and athlete_display_name, calculate the count of unique games where player's points were not NA
games_played <- wnba_player_box %>%
  filter(!is.na(points)) %>%
  group_by(athlete_id, athlete_display_name) %>%
  summarise(games_played = n_distinct(game_id))

print(games_played, n=322)


#Left join total_rebounds_current and games_played on athlete_id and athlete_display_name
joined_data <- left_join(total_points, games_played, by = c("athlete_id", "athlete_display_name"))

joined_data$ppg <- joined_data$total_points/joined_data$games_played


#graph of cumulative points vs games played
dt_data <- wnba_player_box %>%
  filter(athlete_display_name == "Diana Taurasi") %>%
  arrange(game_date)

aw_data <- wnba_player_box %>%
  filter(athlete_display_name == "A'ja Wilson") %>%
  arrange(game_date)

#calculate cumulative points
dt_data$cumulative_points <- cumsum(dt_data$points)
aw_data $cumulative_points <- cumsum(aw_data $points)

#count game number
dt_data$game_number <- 1:nrow(dt_data)
aw_data $game_number <- 1:nrow(aw_data)

#combine data
combined_data <- rbind(dt_data, aw_data)
combined_data$athlete_display_name <- factor(combined_data$athlete_display_name)

combined_data <- combined_data %>%
	select(athlete_display_name, game_date, game_number, cumulative_points)

#plot
ggplot(combined_data, aes(x = game_number, y = cumulative_points, color = athlete_display_name)) +
  geom_line() +
  geom_hline(yintercept = 860, linetype = "dashed", color = "red") +
  labs(title = "Cumulative Points by Games Played",
       x = "Games Played",
       y = "Cumulative Points",
       color = "Player") +
  theme_minimal()

write.csv(combined_data, file = "~/Desktop/points_vs_games.csv", row.names = FALSE)
