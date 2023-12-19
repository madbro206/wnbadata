#wehoop data
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr)

library(dplyr)

#equivalent in NCAAW:
tictoc::tic()
progressr::with_progress({
  wbb_pbp <- wehoop::load_wbb_pbp(seasons=c(2021,2022,2023,2024))
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_team_box <- wehoop::load_wbb_team_box(seasons=c(2021,2022,2023,2024))
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box(seasons=c(2021,2022,2023,2024))
})
tictoc::toc()

#find the players with the most total rebounds since the 2020-21 season, including players that are not active
total_rebounds <- wbb_player_box %>%
  group_by(athlete_id, athlete_display_name) %>%
  summarise(total_rebounds = sum(rebounds, na.rm = TRUE)) %>%
  arrange(desc(total_rebounds))

# Print the resulting dataframe
print(total_rebounds)

#find players with the most total rebounds since the 2020-21 season, ONLY including players that are currently active in the 2023-24 season
total_rebounds_current <- wbb_player_box %>%
  group_by(athlete_id, athlete_display_name) %>%
  filter(any(season == 2024, na.rm = TRUE)) %>%
  summarise(total_rebounds_current = sum(rebounds, na.rm = TRUE)) %>%
  filter(total_rebounds_current > 15) %>% #trying to only include the D1 players if I can
  arrange(desc(total_rebounds_current))

print(total_rebounds_current, n=100)

#count number of game played by each player
#Group by athlete_id and athlete_display_name, calculate the count of unique games where player's rebounds were not NA
games_played <- wbb_player_box %>%
  filter(!is.na(rebounds)) %>%
  group_by(athlete_id, athlete_display_name) %>%
  summarise(games_played = n_distinct(game_id))

print(games_played)

#current players total rebounds and games played since 2020-21 season
#Left join total_rebounds_current and games_played on athlete_id and athlete_display_name
joined_data <- left_join(total_rebounds_current, games_played, by = c("athlete_id", "athlete_display_name"))

joined_data$rpg <- joined_data$total_rebounds_current/joined_data$games_played

print(joined_data, n=200)

joined_data<- ungroup(joined_data)

#visualization


library(ggplot2)

ggplot(joined_data, aes(x = total_rebounds_current, y = games_played, color = rpg)) +
  geom_point(alpha = 0.7, size=1.3) +
  geom_vline(xintercept = 500, linetype = "dashed", color = "red") +
  geom_point(data = filter(joined_data, athlete_display_name == "Dalayah Daniels"), 
             aes(x = total_rebounds_current, y = games_played), 
             color = "yellow", size = 3, shape = 16) +  # Customize the color and size
  geom_text(data = filter(joined_data, athlete_display_name == "Dalayah Daniels"), 
            aes(x = total_rebounds_current, y = games_played, label = athlete_display_name),
            vjust = -0.5, hjust = 1, size = 3, color = "yellow") +  # Adjust the position and appearance of the label
  labs(title = "Total Rebounds vs. Games Played",
       subtitle = "for Active NCAAWBB Players Since 2020-21",
       x = "Total Rebounds",
       y = "Games Played",
       color = "Rebounds per Game") +
  scale_color_gradient(name = "Rebounds per Game") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)  # Adjust the size as needed
  )





#search for a specific player
specific_player <-games_played %>%
  filter(athlete_display_name == "Dalayah Daniels")

print(specific_player)
