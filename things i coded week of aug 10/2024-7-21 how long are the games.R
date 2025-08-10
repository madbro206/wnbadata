#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, ggplot2)

#load data

#wnba full play by play
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp()
})
tictoc::toc()

#equivalent in NCAAW:
tictoc::tic()
progressr::with_progress({
  wbb_pbp <- wehoop::load_wbb_pbp()
})
tictoc::toc()
##############################################################################################################
#find length of wnba games (realtime from tip to final buzzer)

# Convert the wallclock variable to POSIXct datetime format
wnba_pbp$wallclock <- as.POSIXct(wnba_pbp$wallclock, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")

# Group by game_id and calculate the length of the game, keeping home_team_name and away_team_name
# Group by game_id and calculate the length of the game, keeping home_team_name, away_team_name, and game_date
game_lengths <- wnba_pbp %>%
  group_by(game_id) %>%
  summarize(
  	game_date = first(game_date), 
    home_team_name = first(home_team_name),
    away_team_name = first(away_team_name),
    game_length = difftime(max(wallclock), min(wallclock), units = "mins")
  ) 

game_lengths <- game_lengths %>%
  filter(game_id != 401620361)
  
# View the result
print(game_lengths, n=148)

##############################################################################################################
# Sort the game_lengths data frame by game_length
game_lengths_sorted <- game_lengths %>%
  arrange(desc(game_length))

# View the sorted data frame
print(game_lengths_sorted, n = 148)
##############################################################################################################
#take average game length by home team

average_game_length_by_home_team <- game_lengths %>%
  group_by(home_team_name) %>%
  summarize(average_game_length = mean(game_length))

# View the result
print(average_game_length_by_home_team)
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


############################################################################################################
#debug
weird_games <- game_lengths %>% filter(game_length > 180) # e.g., longer than 3 hours
wnba_pbp %>% filter(game_id %in% weird_games$game_id) %>%
  select(game_id, wallclock, period) %>%
  arrange(game_id, wallclock)

wnba_pbp$wallclock

#plot of messed up clock
one_game_id <- weird_games$game_id[1]
wnba_pbp %>%
  filter(game_id == one_game_id) %>%
  mutate(play_number = row_number()) %>%
  ggplot(aes(x = play_number, y = wallclock)) +
  geom_line() + labs(title = paste("Wallclock timeline for game", one_game_id))

wnba_pbp %>%
  filter(game_id == weird_games$game_id[2]) %>%
  mutate(play_number = row_number()) %>%
  ggplot(aes(x = play_number, y = wallclock)) +
  geom_line() + labs(title = paste("Wallclock timeline for game", one_game_id))

wnba_pbp %>%
  filter(game_id == weird_games$game_id[3]) %>%
  mutate(play_number = row_number()) %>%
  ggplot(aes(x = play_number, y = wallclock)) +
  geom_line() + labs(title = paste("Wallclock timeline for game", weird_games$game_id[3]))

wnba_pbp %>%
  filter(game_id == weird_games$game_id[4]) %>%
  mutate(play_number = row_number()) %>%
  ggplot(aes(x = play_number, y = wallclock)) +
  geom_line() + labs(title = paste("Wallclock timeline for game", weird_games$game_id[4]))

wnba_pbp %>%
  filter(game_id == weird_games$game_id[5]) %>%
  mutate(play_number = row_number()) %>%
  ggplot(aes(x = play_number, y = wallclock)) +
  geom_line() + labs(title = paste("Wallclock timeline for game", weird_games$game_id[5]))

wnba_pbp %>%
  filter(game_id == weird_games$game_id[6]) %>%
  mutate(play_number = row_number()) %>%
  ggplot(aes(x = play_number, y = wallclock)) +
  geom_line() + labs(title = paste("Wallclock timeline for game", weird_games$game_id[6]))

wnba_pbp %>%
  filter(game_id == weird_games$game_id[7]) %>%
  mutate(play_number = row_number()) %>%
  ggplot(aes(x = play_number, y = wallclock)) +
  geom_line() + labs(title = paste("Wallclock timeline for game", weird_games$game_id[7]))


##############################################################################################################
#sparks v storm game on july 16 2024 is broken because in the middle there's random times assigned
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


###################################################################################
#fixed version to minimize weirdness
#this version takes the difference of the clock at the max and min play numbers, instead of diff between max and min clocks
# Make sure wallclock is parsed as POSIXct
wnba_pbp$wallclock <- as.POSIXct(wnba_pbp$wallclock, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# For each game, find the play with MIN and MAX game_play_number and extract wallclock
game_lengths_precise <- wnba_pbp %>%
  group_by(game_id) %>%
  # Find row corresponding to min game_play_number (start)
  mutate(
    start_play = if_else(game_play_number == min(game_play_number, na.rm = TRUE), TRUE, FALSE),
    end_play = if_else(game_play_number == max(game_play_number, na.rm = TRUE), TRUE, FALSE)
  ) %>%
  summarize(
    game_date = first(game_date),
    home_team_name = first(home_team_name),
    away_team_name = first(away_team_name),
    start_wallclock = wallclock[start_play][1],
    end_wallclock = wallclock[end_play][1],
    game_length_minutes = as.numeric(difftime(end_wallclock, start_wallclock, units = "mins"))
  ) %>%
  filter(home_team_name != "Team WNBA") %>%
  select(game_date, home_team_name, away_team_name, game_length_minutes) %>%
  arrange(desc(game_length_minutes))

print(game_lengths_precise, n = Inf)


wnba_pbp %>%
  filter(game_id == game_lengths_precise$game_id[1]) %>%
  mutate(play_number = row_number()) %>%
  ggplot(aes(x = play_number, y = wallclock)) +
  geom_line() + labs(title = paste("Wallclock (UTC) timeline for game", game_lengths_precise$game_id[1]))