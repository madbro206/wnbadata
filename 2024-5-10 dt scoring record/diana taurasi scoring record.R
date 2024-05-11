#wehoop data
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, ggplot2)

tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(seasons=c(2003:2023))
})
tictoc::toc()

#find the players with the most total points since the 2013-14 season, including players that are not active
total_points <- wnba_player_box %>%
  filter(season_type==2) %>%
  group_by(athlete_id, athlete_display_name) %>%
  summarise(total_points = sum(points, na.rm = TRUE)) %>%
  arrange(desc(total_points))

# Print the resulting dataframe
print(total_points)


#count number of games played by each player
#Group by athlete_id and athlete_display_name, calculate the count of unique games where player's points were not NA
games_played <- wnba_player_box %>%
  filter(season_type==2) %>%
  filter(!is.na(points)) %>%
  group_by(athlete_id, athlete_display_name) %>%
  summarise(games_played = n_distinct(game_id))

print(games_played)


#current players total rebounds and games played since 2020-21 season
#Left join total_rebounds_current and games_played on athlete_id and athlete_display_name
joined_data <- left_join(total_points, games_played, by = c("athlete_id", "athlete_display_name"))

joined_data$ppg <- joined_data$total_points/joined_data$games_played

ggplot(joined_data, aes(x = games_played, y = total_points, color = ppg)) +
  geom_point(alpha = 0.5, size=1.3) +
  geom_point(data = filter(joined_data, athlete_display_name == "Diana Taurasi"), 
             aes(x = games_played, y = total_points), 
             color = "black", size = 2, shape = 16) +  
  geom_text(data = filter(joined_data, athlete_display_name == "Diana Taurasi"), 
            aes(x = games_played, y = total_points, label = athlete_display_name),
            vjust = -0.5, hjust = 1, size = 3, color = "black") +  
  geom_point(data = filter(joined_data, athlete_display_name == "Breanna Stewart"), 
             aes(x = games_played, y = total_points), 
             color = "black", size = 2, shape = 16) +  
  geom_text(data = filter(joined_data, athlete_display_name == "Breanna Stewart"), 
            aes(x = games_played, y = total_points, label = athlete_display_name),
            vjust = 1, hjust = 1, size = 3, color = "black") +  
  geom_point(data = filter(joined_data, athlete_display_name == "Rhyne Howard"), 
             aes(x = games_played, y = total_points), 
             color = "black", size = 2, shape = 16) +  
  geom_text(data = filter(joined_data, athlete_display_name == "Rhyne Howard"), 
            aes(x = games_played, y = total_points, label = athlete_display_name),
            vjust = 1, hjust = 1, size = 3, color = "black") +              
  labs(title = "Games Played vs. Total Points",
       subtitle = "for WNBA Players Since 2003",
       x = "Games Played",
       y = "Points Scored",
       color = "Points per game") +
  scale_color_gradient(name = "Points per game") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),  
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    axis.title.x = element_text(margin = margin(t = 10))  # Adjust x-axis title position
  ) +
  scale_x_continuous(breaks = seq(0, 600, by = 100))

  
###########################################################################################################  
library(ggplot2)
library(plotly)

# Your original ggplot code
p <- ggplot(joined_data, aes(x = games_played, y = total_points, color = ppg)) +
  geom_point(alpha = 0.5, size=1.3) +
  labs(title = "Games Played vs. Total Points",
       subtitle = "for WNBA Players Since 2003",
       x = "Games Played",
       y = "Points Scored",
       color = "Points per game") +
  scale_color_gradient(name = "Points per game") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)  
  )

p <- ggplotly(p, tooltip = c("label"))p
p
###########################################################################################################  
 