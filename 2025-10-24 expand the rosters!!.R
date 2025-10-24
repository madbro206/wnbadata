library(wehoop)
library(dplyr)

wnba_player_box <- load_wnba_player_box(season=2006:2025)

players_per_team_season <- wnba_player_box %>%
  filter(!grepl("team", team_name, ignore.case = TRUE)) %>%
  group_by(team_name, season) %>%
  summarize(count = n_distinct(athlete_id), .groups = 'drop') %>%
  arrange(desc(count))


players_per_team_season 
players_per_team_season %>%filter(season==2025)

#total_unique_players <- n_distinct(wnba_player_box$athlete_id)
#total_unique_players

# Average number of players per team by season
average_by_season <- players_per_team_season %>%
  group_by(season) %>%
  summarize(avg_players_per_team = mean(count))

average_by_season


storm <- wnba_player_box %>%
  filter(team_name=="Storm", season==2025)

unique(storm$athlete_display_name)
