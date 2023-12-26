#how to get the WNBA data

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr)


#NCAAW player box scores
tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box()
})
tictoc::toc()



# Assuming your data frame is called wbb_player_box
# install.packages("dplyr")
library(dplyr)

# Filter the data for the season 2024
wbb_player_box_2024 <- wbb_player_box %>%
  filter(season == 2024)

# Create a new data frame with unique player information
unique_players_info <- wbb_player_box_2024 %>%
  distinct(athlete_display_name, .keep_all = TRUE) %>%
  select(athlete_display_name, athlete_jersey, team_location)%>%
  mutate(athlete_jersey = as.integer(athlete_jersey))

# Print or View the new data frame
print(unique_players_info)


# Filter unique_players_info to include players whose athlete_jersey contains 6, 7, 8, or 9
filtered_players_info <- unique_players_info %>%
  filter(grepl("[6-9]", as.character(athlete_jersey)))

# Print or View the filtered data frame
print(filtered_players_info)


#barchart of frequency of jersey numbers with digits 6-9
# Create a complete set of jersey numbers from 0 to 99
all_jersey_numbers <- 0:99

# Create a table of the frequencies of each unique value
jersey_table <- table(filtered_players_info$athlete_jersey)

# Create a data frame with all jersey numbers and their frequencies
jersey_df <- data.frame(Athlete_Jersey = all_jersey_numbers,
                        Frequency = as.integer(jersey_table[as.character(all_jersey_numbers)]))

# Replace NAs with 0 for jersey numbers with no occurrences
jersey_df[is.na(jersey_df$Frequency), "Frequency"] <- 0

# Create the bar chart
barplot(jersey_df$Frequency, names.arg = jersey_df$Athlete_Jersey,
        col = "skyblue", main = "Frequency of Athlete Jersey Numbers",
        xlab = "Athlete Jersey Number", ylab = "Frequency")


#barchart of frequency of jersey numbers of all players
# Create a complete set of jersey numbers from 0 to 99
all_jersey_numbers <- 0:99

# Create a table of the frequencies of each unique value
jersey_table <- table(unique_players_info$athlete_jersey)

# Create a data frame with all jersey numbers and their frequencies
jersey_df <- data.frame(Athlete_Jersey = all_jersey_numbers,
                        Frequency = as.integer(jersey_table[as.character(all_jersey_numbers)]))

# Replace NAs with 0 for jersey numbers with no occurrences
jersey_df[is.na(jersey_df$Frequency), "Frequency"] <- 0

# Create the bar chart
# Assuming you have a data frame named jersey_df
# Create a bar chart with x-axis labels for multiples of 5
ggplot(jersey_df, aes(x = factor(Athlete_Jersey), y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Frequency of Athlete Jersey Numbers",
       x = "Athlete Jersey Number",
       y = "Frequency") +
  scale_x_discrete(breaks = seq(0, 99, by = 5)) +  # Set breaks for multiples of 5
  theme_minimal()



