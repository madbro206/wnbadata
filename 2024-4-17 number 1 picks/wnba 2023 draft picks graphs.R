#wehoop data
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, ggplot2)

#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box()
})
tictoc::toc()


############################################################################################################

#find points scored for players in 2023, includes all star game and playoffs
total_points <- wnba_player_box %>%
  group_by(athlete_id, athlete_display_name) %>%
  summarise(total_points = sum(points, na.rm = TRUE)) %>%
  arrange(desc(total_points))

# Print the resulting dataframe
print(total_points)


#count number of games played by each player (nonzero minutes)
#Group by athlete_id and athlete_display_name, calculate the count of unique games where player's minutes were not NA
games_played <- wnba_player_box %>%
  filter(!is.na(minutes)) %>%
  group_by(athlete_id, athlete_display_name) %>%
  summarise(games_played = n_distinct(game_id)) 

print(games_played)

minutes_played <- wnba_player_box %>%
  group_by(athlete_id, athlete_display_name) %>%
  summarise(total_minutes = sum(minutes, na.rm = TRUE))

print(minutes_played)

#current players total points and games played in 2023
#Left join total_rebounds_current and games_played on athlete_id and athlete_display_name
joined_data <- left_join(total_points, games_played, by = c("athlete_id", "athlete_display_name"))
joined_data <- left_join(joined_data, minutes_played, by = c("athlete_id", "athlete_display_name"))

joined_data$ppg <- joined_data$total_points/joined_data$games_played


#plot all 2023 players points vs games
ggplot(joined_data, aes(x = games_played, y = total_points, color = ppg)) +
  geom_point(alpha = 0.5, size=1.3) +
  #geom_hline(yintercept = 3569, linetype = "dashed", color = "red") + 
  geom_vline(xintercept = 40, linetype = "dashed", color = "red") +
  labs(title = "Games Played vs. Total Points in WNBA 2023",
       subtitle = "Includes Playoffs and All Star Game",
       x = "Games Played",
       y = "Total points scored",
       color = "Points per game") +
  scale_color_gradient(name = "Points per game") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)  # Adjust the size as needed
  )
  
#plot all 2023 players points vs minutes
ggplot(joined_data, aes(x = total_minutes, y = total_points, color = ppg)) +
  geom_point(alpha = 0.5, size=1.3) +
  #geom_hline(yintercept = 3569, linetype = "dashed", color = "red") + 
  #geom_vline(xintercept = 40, linetype = "dashed", color = "red") +
  labs(title = "Minutes Played vs. Total Points in WNBA 2023",
       subtitle = "Includes Playoffs and All Star Game",
       x = "Minutes Played",
       y = "Total points scored",
       color = "Points per game") +
  scale_color_gradient(name = "Points per game") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)  # Adjust the size as needed
  )

#plot 2023 rookies only
rookies<- subset(joined_data, athlete_display_name=="Aliyah Boston"|athlete_display_name=="Diamond Miller"|athlete_display_name=="Maddy Siegrist"|athlete_display_name=="Haley Jones"|athlete_display_name=="Grace Berger"|athlete_display_name=="Laeticia Amihere"|athlete_display_name=="Jordan Horston"|athlete_display_name=="Zia Cooke"|athlete_display_name=="Abby Meyers"|athlete_display_name=="Taylor Mikesell"|athlete_display_name=="Leigha Brown"|athlete_display_name=="Dorka Juhasz"|athlete_display_name=="Madi Williams"|athlete_display_name=="Ashley Joens"|athlete_display_name=="Dulcy Fankam Mendjiadeu"|athlete_display_name=="Kayana Traylor"|athlete_display_name=="Victaria Saxton"|athlete_display_name=="Taylor Soule"|athlete_display_name=="Kadi Sissoko")
rookies

#rookies points vs games
ggplot(rookies, aes(x = games_played, y = total_points, color = ppg, label = athlete_display_name)) +
  geom_point(alpha = 0.5, size=1.3) +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +  # Adjust the position and size of labels
  geom_vline(xintercept = 40, linetype = "dashed", color = "red") +
  labs(title = "Games Played vs. Total Points in WNBA 2023",
       subtitle = "Includes Playoffs and All Star Game",
       x = "Games Played",
       y = "Total points scored",
       color = "Points per game") +
  scale_color_gradient(name = "Points per game") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)  # Adjust the size as needed
  )

ggplot(rookies, aes(x = games_played, y = total_points, color = ppg, label = ifelse(games_played >= 20 | total_points >= 100, athlete_display_name, ""))) +
  geom_point(alpha = 0.5, size = 1.3) +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  geom_vline(xintercept = 40, linetype = "dashed", color = "red") +
  labs(title = "Rookie Games Played vs. Total Points in WNBA 2023",
       subtitle = "Includes Playoffs and All Star Game",
       x = "Games Played",
       y = "Total points scored",
       color = "Points per game") +
  scale_color_gradient(name = "Points per game") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )


  
#rookies points vs minutes
ggplot(rookies, aes(x = total_minutes, y = total_points, color = ppg, label = athlete_display_name)) +
  geom_point(alpha = 0.5, size=1.3) +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +  # Adjust the position and size of labels
  #geom_vline(xintercept = 40, linetype = "dashed", color = "red") +
  labs(title = "Rookie Minutes Played vs. Total Points in WNBA 2023",
       subtitle = "Includes Playoffs and All Star Game",
       x = "Minutes Played",
       y = "Total points scored",
       color = "Points per game") +
  scale_color_gradient(name = "Points per game") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)  # Adjust the size as needed
  )

ggplot(rookies, aes(x = total_minutes, y = total_points, color = ppg, label = ifelse(total_minutes >= 100, athlete_display_name, ""))) +
  geom_point(alpha = 0.5, size=1.3) +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +  # Adjust the position and size of labels
  #geom_vline(xintercept = 40, linetype = "dashed", color = "red") +
  labs(title = "Rookie Minutes Played vs. Total Points in WNBA 2023",
       subtitle = "Includes Playoffs and All Star Game",
       x = "Minutes Played",
       y = "Total points scored",
       color = "Points per game") +
  scale_color_gradient(name = "Points per game") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)  # Adjust the size as needed
  )
############################################################################################################
#plot cumulative points per GAME in wnba for 2023 wnba draft picks

#graph of cumulative points vs games played
aliyah_data <- wnba_player_box %>%
  filter(athlete_display_name == "Aliyah Boston") %>%
  arrange(game_date)

#calculate cumulative points
aliyah_data $cumulative_points <- cumsum(aliyah_data $points)

#count game number
aliyah_data $game_number <- 1:nrow(aliyah_data)


#combine data
combined_data <- rbind(aliyah_data)
combined_data$athlete_display_name <- factor(combined_data$athlete_display_name)

combined_data <- combined_data %>%
	select(athlete_display_name, game_date, game_number, cumulative_points)

#plot
ggplot(combined_data, aes(x = game_number, y = cumulative_points, color = athlete_display_name)) +
  geom_line() +
  labs(title = "Cumulative Points by Games Played",
       x = "Games Played",
       y = "Cumulative Points",
       color = "Player") +
  theme_minimal()
  



# List of athlete names
athlete_names <- subset(rookies, games_played>20& total_points>100)$athlete_display_name

all_combined_data <- list()

# Loop over each athlete
for (athlete_name in athlete_names) {
  # Filter data for the current athlete and remove rows where minutes is NA
  athlete_data <- wnba_player_box %>%
    filter(athlete_display_name == athlete_name, !is.na(minutes)) %>%
    arrange(game_date)
  
  # Calculate cumulative points
  athlete_data$cumulative_points <- cumsum(athlete_data$points)
  
  # Count game number
  athlete_data$game_number <- 1:nrow(athlete_data)
  
  # Combine data
  combined_data <- athlete_data %>%
    select(athlete_display_name, game_date, game_number, cumulative_points)
  
  # Store combined data for the current athlete
  all_combined_data[[athlete_name]] <- combined_data
}

# Combine data frames for all athletes
all_combined_data <- do.call(rbind, all_combined_data)

# Plot cumulative points vs games played for all athletes
ggplot(all_combined_data, aes(x = game_number, y = cumulative_points, color = athlete_display_name)) +
  geom_line() +
  labs(title = "Rookie Cumulative Points vs. Games Played in WNBA 2023", subtitle="At least 20 games",
       x = "Game Number",
       y = "Cumulative Points",
       color = "Athlete") +
  theme_minimal()
  
  
############################################################################################################
#plot cumulative minutes per GAME in wnba for 2023 wnba draft picks  

athlete_names <- rookies$athlete_display_name

all_combined_data <- list()

# Loop over each athlete
for (athlete_name in athlete_names) {
  # Filter data for the current athlete and remove rows where minutes is NA
  athlete_data <- wnba_player_box %>%
    filter(athlete_display_name == athlete_name, !is.na(minutes)) %>%
    arrange(game_date)
  
  # Calculate cumulative minutes
  athlete_data$cumulative_minutes <- cumsum(athlete_data$minutes)
  
  # Count game number
  athlete_data$game_number <- 1:nrow(athlete_data)
  
  # Combine data
  combined_data <- athlete_data %>%
    select(athlete_display_name, game_date, game_number, cumulative_minutes)
  
  # Store combined data for the current athlete
  all_combined_data[[athlete_name]] <- combined_data
}

# Combine data frames for all athletes
all_combined_data <- do.call(rbind, all_combined_data)

# Plot cumulative minutes vs games played for all athletes
ggplot(all_combined_data, aes(x = game_number, y = cumulative_minutes, color = athlete_display_name)) +
  geom_line() +
  labs(title = "Rookie Cumulative Minutes vs. Games Played in WNBA 2023", subtitle="",
       x = "Game Number",
       y = "Cumulative Minutes",
       color = "Athlete") +
  theme_minimal()
  

#write.csv(combined_data, file = "~/Desktop/points_vs_games.csv", row.names = FALSE)



############################################################################################################
#cumulative ppg for number 1 picks 

#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box1 <- wehoop::load_wnba_player_box(season=c(2002:2023))
})
tictoc::toc()

# List of athlete names
athlete_names <- c("Sue Bird", "LaToya Thomas", "Diana Taurasi", "Janel McCarville", "Seimone Augustus", "Lindsey Harding", "Candace Parker", "Angel McCoughtry", "Tina Charles", "Maya Moore", "Nneka Ogwumike", "Brittney Griner", "Chiney Ogwumike", "Jewell Loyd", "Breanna Stewart", "Kelsey Plum", "A'ja Wilson", "Jackie Young", "Sabrina Ionescu", "Charli Collier", "Rhyne Howard", "Aliyah Boston")

all_combined_data <- list()

# Loop over each athlete
for (athlete_name in athlete_names) {
  # Filter data for the current athlete and remove rows where minutes is NA
  athlete_data <- wnba_player_box1 %>%
    filter(athlete_display_name == athlete_name, !is.na(minutes)) %>%
    arrange(game_date)
  
  # Calculate cumulative points
  athlete_data$cumulative_points <- cumsum(athlete_data$points)
  
  # Count game number
  athlete_data$game_number <- 1:nrow(athlete_data)
  
  # Combine data
  combined_data <- athlete_data %>%
    select(athlete_display_name, game_date, game_number, cumulative_points)
  
  # Store combined data for the current athlete
  all_combined_data[[athlete_name]] <- combined_data
}

# Combine data frames for all athletes
all_combined_data <- do.call(rbind, all_combined_data)

# Plot cumulative points vs games played for all athletes
ggplot(all_combined_data, aes(x = game_number, y = cumulative_points, color = athlete_display_name)) +
  geom_line() +
  labs(title = "Number 1 Pick Cumulative Points vs. Games Played", subtitle="Since 2002",
       x = "Game Number",
       y = "Cumulative Points",
       color = "Athlete") +
  theme_minimal()
