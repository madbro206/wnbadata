#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, ggplot)

#load data

#wnba full play by play
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp()
})
tictoc::toc()

#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box()
})

tictoc::toc()

#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box()
})
tictoc::toc()

#######################################################################################################
#translate into player box score

Pbox <- wnba_player_box %>%
  group_by(season, team_location, athlete_display_name) %>%
  summarise(GP=sum(minutes>0, na.rm=TRUE), MIN=sum(minutes, na.rm = TRUE), PTS=sum(points, na.rm = TRUE),
    P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), P2p=100*P2M/P2A,
    P3M=sum(three_point_field_goals_made, na.rm = TRUE), P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), P3p=100*P3M/P3A,
    FTM=sum(free_throws_made, na.rm = TRUE), FTA=sum(free_throws_attempted, na.rm = TRUE), FTp=100*FTM/FTA,
    OREB=sum(offensive_rebounds, na.rm = TRUE), DREB=sum(defensive_rebounds, na.rm = TRUE), AST=sum(assists, na.rm = TRUE),
    TOV=sum(turnovers, na.rm = TRUE), STL=sum(steals, na.rm = TRUE), BLK=sum(blocks, na.rm = TRUE),
    PF=sum(fouls, na.rm = TRUE)) %>%
  rename(Season=season, Team=team_location,
         Player=athlete_display_name) %>%
  as.data.frame()
 
##############################################################################################################
#cumulative blocks by game date
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Ensure game_date is in Date format
wnba_player_box$game_date <- as.Date(wnba_player_box$game_date)

# Filter data for Ezi Magbegor and Cameron Brink
players_data <- wnba_player_box %>%
  filter(athlete_display_name %in% c("Ezi Magbegor", "Cameron Brink", "A'ja Wilson")) %>%
  filter(did_not_play==FALSE) %>%
  filter(season_type==2) %>%
  filter(team_name !="Team USA") %>%
  arrange(game_date)

# Calculate cumulative blocks
players_data <- players_data %>%
  group_by(athlete_display_name) %>%
  arrange(game_date) %>%
  mutate(cumulative_blocks = cumsum(blocks))

# Define custom colors
custom_colors <- c("Ezi Magbegor" = "darkgreen", "Cameron Brink" = "purple", "A'ja Wilson"="red")

# Get unique game dates
game_dates <- unique(players_data$game_date)

# Plotting
ggplot(players_data, aes(x = game_date, y = cumulative_blocks, color = athlete_display_name, group = athlete_display_name)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "solid", alpha=0.7) +
  scale_color_manual(values = custom_colors) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  scale_x_date(breaks = game_dates, date_labels = "%m/%d") +
  labs(title = "A'ja Wilson vs Cameron Brink vs Ezi Magbegor Blocks", 
       subtitle = "2024 Cumulative Blocks by Date",
       x = "Game Date", y = "Cumulative Blocks", caption="source: stats.wnba.com | graphic: @wnbadata") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1, size=5)) # Rotating x-axis labels for better readability



