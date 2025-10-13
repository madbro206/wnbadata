pacman::p_load(wehoop, dplyr, tidyr, tictoc, progressr, ggplot2)

#load WNBA data
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(season=c(2015:2025))
})
tictoc::toc()

#take out 2020 i dont care
wnba_player_box <- wnba_player_box %>% filter(season !=2020 & !is.na(minutes) & minutes > 0) #if minutes is NA, did not play

#filter Jewell Loyd's stats at Mercury's arena (away games vs Phoenix)
jewell_phx <- wnba_player_box %>%
  filter(athlete_display_name == "Jewell Loyd" & opponent_team_name == "Mercury" & home_away == "away") %>%
  filter(team_name=="Storm" | team_name=="Aces") 
  #distinct(game_id, .keep_all = TRUE)

#filter Jewell Loyd's stats not at Mercury's arena (still away)
jewell_elsewhere <- wnba_player_box %>%
  filter(athlete_display_name == "Jewell Loyd" & home_away=="away" & opponent_team_name != "Mercury") %>%
  filter(team_name=="Storm" | team_name=="Aces") #try to eliminate all star games
  #distinct(game_id, .keep_all = TRUE)

#filter home games
jewell_home <- jewell_home %>%
  filter(athlete_display_name == "Jewell Loyd" & home_away == "home") %>%
  filter(team_name=="Storm" | team_name=="Aces")
  #distinct(game_id, .keep_all = TRUE)


#stats to compare
stat_cols <- c("minutes", "points", "rebounds", "assists", "steals", "blocks", "field_goals_made", "field_goals_attempted", "three_point_field_goals_made", "three_point_field_goals_attempted", "free_throws_made", "free_throws_attempted", "turnovers")

#averages
phx_averages <- jewell_phx %>%
  summarize(across(all_of(stat_cols), mean, na.rm=TRUE)) %>%
  mutate(location = "Mercury Arena")

elsewhere_averages <- jewell_elsewhere %>%
  summarize(across(all_of(stat_cols), mean, na.rm=TRUE)) %>%
  mutate(location = "Other Venues")

home_averages <- jewell_home %>%
  summarize(across(all_of(stat_cols), mean, na.rm=TRUE)) %>%
  mutate(location = "Home")


# Combine all summaries
comparison_table <- bind_rows(
  phx_averages,
  elsewhere_averages,
  home_averages
) %>%
  pivot_longer(cols = -location, names_to = "stat", values_to = "average") %>%
  pivot_wider(names_from = location, values_from = average)

#number of games is number of rows (where minutes is not NA, actually played the game)
phx_games <- nrow(jewell_phx)
elsewhere_games <- nrow(jewell_elsewhere)
home_games <- nrow(jewell_home)

home_games <- nrow(jewell_home)
game_count <- tibble(
  stat = "games_played",
  `Mercury Arena` = phx_games,
  `Other Venues` = elsewhere_games,
  `Home` = home_games
)

comparison_table <- bind_rows(comparison_table, game_count)

#count wins
phx_wins <- sum(jewell_phx$team_winner, na.rm = TRUE)
elsewhere_wins <- sum(jewell_elsewhere$team_winner, na.rm = TRUE)
home_wins <- sum(jewell_home$team_winner, na.rm = TRUE)

#include wins
wins_count <- tibble(
  stat = "team_wins",
  `Mercury Arena` = phx_wins,
  `Other Venues` = elsewhere_wins,
  `Home` = home_wins
)

final_table <- bind_rows(comparison_table, wins_count)

final_table <- final_table %>% select('stat', 'Mercury Arena', 'Home', 'Other Venues')

final_table


abbrevs <- c(
  minutes = "MIN",
  points = "PTS",
  rebounds = "REB",
  assists = "AST",
  steals = "STL",
  blocks = "BLK",
  field_goals_made = "FGM",
  field_goals_attempted = "FGA",
  three_point_field_goals_made = "3PM",
  three_point_field_goals_attempted = "3PA",
  free_throws_made = "FTM",
  free_throws_attempted = "FTA",
  turnovers = "TO",
  games_played = "GP",
  team_wins = "W"
)
final_table <- final_table %>%
  mutate(stat = abbrevs[stat])

final_table


#401620457 did not play
#subset(jewell_phx, game_id==401620457)$minutes
#subset(final_table, stat=="games_played" | stat=="team_wins")