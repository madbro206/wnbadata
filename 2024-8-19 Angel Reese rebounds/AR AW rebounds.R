#wehoop data
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, ggplot2)

tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(seasons=c(2008:2024))
})
tictoc::toc()

#count number of games played by each player
#Group by athlete_id and athlete_display_name, calculate the count of unique games where player's minutes were not NA or 0
games_played <- wnba_player_box %>%
  filter(!is.na(rebounds) & !is.na(points) & minutes > 0 & rebounds >0 & season_type == 2 & active == TRUE) %>% # regular season games
  group_by(athlete_id, athlete_display_name) %>%
  summarise(games_played = n_distinct(game_id))

  


  
#filter data by player
angel_data <- wnba_player_box %>%
  filter(athlete_display_name == "Angel Reese") %>%
  arrange(game_date)
angel_data$cumulative_reb <- cumsum(ifelse(is.na(angel_data$rebounds), 0, angel_data$rebounds))
angel_data$game_number <- 1:nrow(angel_data)

aja_data <- wnba_player_box %>%
  filter(athlete_display_name == "A'ja Wilson") %>%
  arrange(game_date)
aja_data$cumulative_reb <- cumsum(ifelse(is.na(aja_data$rebounds), 0, aja_data$rebounds))
aja_data$game_number <- 1:nrow(aja_data)

syl_data <- wnba_player_box %>%
  filter(athlete_display_name == "Sylvia Fowles") %>%
  arrange(game_date)
syl_data$cumulative_reb <- cumsum(ifelse(is.na(syl_data$rebounds), 0, syl_data$rebounds))
syl_data$game_number <- 1:nrow(syl_data)


#combine data
combined_data <- rbind(angel_data, aja_data)
combined_data$athlete_display_name <- factor(combined_data$athlete_display_name)

combined_data <- combined_data %>%
	select(athlete_display_name, game_date, game_number, cumulative_reb)

#plot
ggplot(combined_data, aes(x = game_number, y = cumulative_reb, color = athlete_display_name)) +
  geom_line() +
  geom_text(data = label_data, aes(label = athlete_display_name), 
            hjust = -0.1, 
            vjust = 0.5, 
            size = 4.5) +
  labs(title = "Angel Reese vs A'ja Wilson Early Career Rebounding", 
       subtitle = "Cumulative Rebounds by Games Played, First 27 Games of WNBA Career", 
       caption = "data: wehoop R package | graphic: @wnbadata",
       x = "Games Played",
       y = "Cumulative Rebounds") +
  scale_x_continuous(limits = c(0, 27)) +
  scale_y_continuous(limits = c(0, 332)) +
  scale_color_manual(values = c("Angel Reese" = "#418FDE", "A'ja Wilson" = "#BA0C2F")) + # Replace with desired colors
  theme_minimal() +
  theme(legend.position = "none")



#write.csv(combined_data, file = "~/Desktop/points_vs_games.csv", row.names = FALSE)
