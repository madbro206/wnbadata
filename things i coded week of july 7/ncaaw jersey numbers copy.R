#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html
# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, ggplot2)

#NCAAW player box scores
wbb_player_box <- wehoop::load_wbb_player_box()

#filter the data for the 2025 season
wbb_player_box_2025 <- wbb_player_box %>%
  filter(season == 2025)

#create a new data frame with unique player information
#leaves just one row per player
unique_players_2025 <- wbb_player_box_2025 %>%
  distinct(athlete_id, athlete_display_name, athlete_jersey, team_location, .keep_all = TRUE) %>%	
  select(athlete_display_name, athlete_jersey, team_location)%>%
  mutate(athlete_jersey = as.integer(athlete_jersey))

#filters to only players with the digits 6-9 in their athlete_jersey
filtered_players_2025 <- unique_players_2025 %>%
  filter(grepl("[6-9]", as.character(athlete_jersey))) %>%
  distinct(athlete_display_name, .keep_all = TRUE) %>%
  arrange(athlete_jersey)

#print the players
print(filtered_players_2025, n= 372) 


#barchart of frequency of jersey numbers with digits 6-9
#complete set of jersey numbers from 0 to 99
all_jersey_numbers <- 0:99

#barchart of frequency of jersey numbers of all players
jersey_table <- table(unique_players_2025 $athlete_jersey)

#create a data frame with all jersey numbers and their frequencies
jersey_df <- data.frame(Athlete_Jersey = all_jersey_numbers, Frequency = as.integer(jersey_table[as.character(all_jersey_numbers)]))
                        
 jersey_df <- jersey_df %>%
  mutate(Highlight = grepl("[6-9]", as.character(Athlete_Jersey)))                  

#replace NAs with 0 for jersey numbers with no occurrences
jersey_df[is.na(jersey_df$Frequency), "Frequency"] <- 0

ggplot(jersey_df, aes(x = factor(Athlete_Jersey), y = Frequency, fill = Highlight)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("FALSE" = "skyblue", "TRUE" = "orange")) +
  labs(title = "Frequency of Jersey Numbers in NCAAW 2024-25",
       x = "Jersey Number",
       y = "Frequency") +
  scale_x_discrete(breaks = seq(0, 99, by = 5)) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend




#check WNBA numbers as well
wnba_player_box <- wehoop::load_wnba_player_box()

#create a new data frame with unique player information
unique_wnba <- wnba_player_box %>%
  distinct(athlete_id, .keep_all = TRUE) %>%
  select(athlete_jersey, athlete_display_name, team_name)%>%
  mutate(athlete_jersey = as.integer(athlete_jersey))

#filter unique_players to find players whose athlete_jersey contains 6-9
filtered_wnba <- unique_wnba %>%
  filter(grepl("[6-9]", as.character(athlete_jersey))) %>%
  arrange(athlete_jersey)

print(filtered_wnba, n=31)


