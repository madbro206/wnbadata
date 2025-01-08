#Use wehoop package to download NCAAW data
#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, ggplot2)

#load data
tictoc::tic()
progressr::with_progress({
  wbb_pbp <- wehoop::load_wbb_pbp()
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_pbp2015 <- wehoop::load_wbb_pbp(season=2015)
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_team_box <- wehoop::load_wbb_team_box()
})
tictoc::toc()
##############################################################################################################
#find length of games (realtime from tip to final buzzer)

# Convert the wallclock variable to POSIXct datetime format
wbb_pbp$wallclock <- as.POSIXct(wbb_pbp$wallclock, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")

# Group by game_id and calculate the length of the game, keeping home_team_name and away_team_name
# Group by game_id and calculate the length of the game, keeping home_team_name, away_team_name, and game_date
game_lengths <- wbb_pbp %>%
  filter(!is.na(wallclock)) %>%
  group_by(game_id) %>%
  summarize(
    game_date = first(game_date),
    home_team_name = first(home_team_name),
    away_team_name = first(away_team_name),
    game_length = if(n() >= 2) difftime(max(wallclock, na.rm = TRUE), min(wallclock, na.rm = TRUE), units = "mins") else NA_real_,
    start_time = min(wallclock, na.rm = TRUE),
    end_time = max(wallclock, na.rm = TRUE)
    #valid_wallclock_count = sum(!is.na(wallclock))
  ) %>%
  arrange(desc(game_length))

print(game_lengths, n=15)

subset(game_lengths, home_team_name=="Oklahoma")
unique(subset(wbb_pbp, home_team_name=="Oklahoma")$game_date)

################################plot fouls vs game length##############################################################################
fouls_data <- wbb_team_box %>%
  group_by(game_id) %>%
  summarize(total_fouls = sum(fouls, na.rm = TRUE)) %>%
  arrange(desc(total_fouls))

print(fouls_data, n=5)
subset(game_lengths, game_id=="401718897")

#merge team box with game_lengths
plot_data <- game_lengths %>%
  filter(game_length<200 & game_length>60) %>%
  inner_join(fouls_data, by = "game_id") %>%
  mutate(game_length_minutes = as.numeric(game_length))

cor(plot_data$game_length_minutes, plot_data$total_fouls)

print(plot_data, n=5)

ggplot(plot_data, aes(x = total_fouls, y = game_length_minutes)) +
  geom_point(alpha = 0.6, size=2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Total Fouls vs. Game Length", subtitle="NCAAW 2025 Season, where data is available",
       x = "Total Fouls (Both Teams)",
       y = "Approx Game Length (Minutes)") +
  theme_minimal()

##############################################################################################################
#take average game length by home team
average_game_length_by_home_team <- game_lengths %>%
  group_by(home_team_name) %>%
  summarize(average_game_length = mean(game_length)) %>%
  arrange(desc(average_game_length))

# View the result
print(average_game_length_by_home_team, n=20)
##############################################################################################################
#bar chart
# Convert average_game_length from difftime to numeric
average_game_length_by_home_team <- average_game_length_by_home_team %>%
  mutate(average_game_length_numeric = as.numeric(average_game_length))

# Create the bar chart with labels inside the bars
ggplot(average_game_length_by_home_team, aes(x = reorder(home_team_name, average_game_length_numeric), y = average_game_length_numeric)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(average_game_length_numeric, 1)), 
            hjust = 0.5,  # Center the label horizontally
            vjust = 0.5,  # Center the label vertically within the bar
            color = "black",  # Optional: Change label color for better visibility
            size = 3.5) +  # Adjust the text size
  labs(
    x = "Home Team",
    y = "Average Game Length (minutes)",
    title = "Average Game Length by Home Team"
  ) +
  theme_minimal() +
  coord_flip()  # Flips the x and y axes for better readability


#########################################old code#####################################################################
#sparks v storm game on july 16 is broken because in the middle there's random times assigned
game<- subset(wnba_pbp, game_id==401620361)
# Convert character strings to POSIXct
start_time <- as.POSIXct("2024-07-16 19:33:56 UTC", format="%Y-%m-%d %H:%M:%S", tz="UTC")
end_time <- as.POSIXct("2024-07-16 21:44:59 UTC", format="%Y-%m-%d %H:%M:%S", tz="UTC")

# Calculate the difference
time_difference <- difftime(end_time, start_time, units="mins")

# Update the specific row in game_lengths
#game_lengths <- game_lengths %>%
#  mutate(game_length = ifelse(game_id == 401620361, time_difference, game_length))

# Remove the row with game_id = 401620361
game_lengths <- game_lengths %>%
  filter(game_id != 401620361)

subset(game_lengths, home_team_name=='Los Angeles')



#game_id where there are several NA entries
result <- wbb_pbp %>%
  filter(is.na(wallclock)) %>%
  group_by(game_id) %>%
  filter(n() > 30) %>% #at least 30=NA
  distinct(game_id) %>%
  pull(game_id)

print(result)
