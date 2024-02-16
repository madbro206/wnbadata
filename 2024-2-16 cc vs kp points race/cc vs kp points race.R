#wehoop data
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr)

tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box(seasons=c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024))
})
tictoc::toc()

#find the players with the most total points since the 2013-14 season, including players that are not active
total_points <- wbb_player_box %>%
  group_by(athlete_id, athlete_display_name) %>%
  summarise(total_points = sum(points, na.rm = TRUE)) %>%
  arrange(desc(total_points))

# Print the resulting dataframe
print(total_points)


#count number of games played by each player
#Group by athlete_id and athlete_display_name, calculate the count of unique games where player's points were not NA
games_played <- wbb_player_box %>%
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
  geom_hline(yintercept = 3569, linetype = "dashed", color = "red") +
  geom_point(data = filter(joined_data, athlete_display_name == "Caitlin Clark"), 
             aes(x = games_played, y = total_points), 
             color = "yellow", size = 3, shape = 16) +  # Customize the color and size
  geom_text(data = filter(joined_data, athlete_display_name == "Caitlin Clark"), 
            aes(x = games_played, y = total_points, label = athlete_display_name),
            vjust = -0.5, hjust = 1, size = 3, color = "black") +  
  geom_point(data = filter(joined_data, athlete_display_name == "Kelsey Plum"), 
             aes(x = games_played, y = total_points), 
             color = "purple", size = 3, shape = 16) +  # Customize the color and size
  geom_text(data = filter(joined_data, athlete_display_name == "Kelsey Plum"), 
            aes(x = games_played, y = total_points, label = athlete_display_name),
            vjust = 1, hjust = 1, size = 3, color = "black") +  
  labs(title = "Games Played vs. Total Points",
       subtitle = "for NCAAWBB Players Since 2013-14",
       x = "Games Played",
       y = "Games Played",
       color = "Points per game") +
  scale_color_gradient(name = "Points per game") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)  # Adjust the size as needed
  )
  
joined_data <- joined_data %>%
	filter(games_played>50)%>%
	arrange(desc(ppg))
  

#graph of cumulative points vs games played
caitlin_data <- wbb_player_box %>%
  filter(athlete_display_name == "Caitlin Clark") %>%
  arrange(game_date)

#calculate cumulative points
caitlin_data$cumulative_points <- cumsum(caitlin_data$points)

#count game number
caitlin_data$game_number <- 1:nrow(caitlin_data)

# Filter the dataframe for Kelsey Plum's data
kelseyp_data <- wbb_player_box %>%
  filter(athlete_display_name == "Kelsey Plum") %>%
  arrange(game_date)
kelseyp_data$cumulative_points <- cumsum(kelseyp_data$points)
kelseyp_data$game_number <- 1:nrow(kelseyp_data)

kelseym_data <- wbb_player_box %>%
  filter(athlete_display_name == "Kelsey Mitchell" & team_location=="Ohio State") %>%
  arrange(game_date)
kelseym_data$cumulative_points <- cumsum(kelseym_data$points)
kelseym_data$game_number <- 1:nrow(kelseym_data)

dyaisha_data <- wbb_player_box %>%
  filter(athlete_display_name == "Dyaisha Fair") %>%
  arrange(game_date)
dyaisha_data <- na.omit(dyaisha_data)
dyaisha_data $cumulative_points <- cumsum(dyaisha_data $points)
dyaisha_data $game_number <- 1:nrow(dyaisha_data)

ashley_data <- wbb_player_box %>%
  filter(athlete_display_name == "Ashley Joens") %>%
  arrange(game_date)
ashley_data <- na.omit(ashley_data)
ashley_data $cumulative_points <- cumsum(ashley_data $points)
ashley_data $game_number <- 1:nrow(ashley_data)

#combine data
combined_data <- rbind(caitlin_data, kelseyp_data, kelseym_data, dyaisha_data, ashley_data)
combined_data$athlete_display_name <- factor(combined_data$athlete_display_name)

#plot
ggplot(combined_data, aes(x = game_number, y = cumulative_points, color = athlete_display_name)) +
  geom_line() +
  geom_hline(yintercept = 3569, linetype = "dashed", color = "red") +
  labs(title = "Cumulative Points by Games Played",
         subtitle = "for Top 5 NCAAWBB Players in Points Since 2013-14",
       x = "Games Played",
       y = "Cumulative Points",
       color = "Player") +
  theme_minimal()
