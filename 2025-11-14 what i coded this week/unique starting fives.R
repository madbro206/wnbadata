#Use wehoop package to download WNBA data

#this loads all the packages I need at once, i could instead just do "library(wehoop)" for each package
pacman::p_load(wehoop, dplyr, tidyr, lubridate, glue, tictoc, progressr, lubridate, ggplot2)

#load data
#wbb player full box score
tictoc::tic()
progressr::with_progress({
  wbb_player_box2 <- wehoop::load_wbb_player_box(season=2026)
})
tictoc::toc()

#Create a unique identifier for each starting five
starting_lineups <- wbb_player_box2 %>%
  filter(starter == TRUE) %>%  #only starters
  group_by(game_id, team_id, team_location) %>%
  arrange(athlete_id) %>% 
  summarize(
    lineup = paste(athlete_id, collapse = "_"),  # Create unique lineup ID
    .groups = 'drop'
  )

#Count unique lineups per team
lineup_counts <- starting_lineups %>%
  group_by(team_id, team_location) %>%
  summarize(
    total_games = n(),
    unique_lineups = n_distinct(lineup),
    .groups = 'drop'
  ) %>%
  arrange(desc(unique_lineups))

# View results
print(lineup_counts, n=500)

#teams with different starting five every game
print(lineup_counts %>% filter(unique_lineups==total_games & total_games >1)%>%select(team_location, total_games, unique_lineups), n=74)


# Teams with same starting five all season
lineup_counts %>%
  filter(unique_lineups == 1 & total_games >1)

#######################################################
# View Tennessee's starting lineups with player names
tennessee_lineups <- wbb_player_box2 %>%
  filter(starter == TRUE, team_location == "Tennessee") %>%
  group_by(game_id, game_date) %>%
  arrange(athlete_display_name) %>%
  summarize(
    starters = paste(athlete_display_name, collapse = ", "),
    lineup_id = paste(athlete_id, collapse = "_"),
    .groups = 'drop'
  ) %>%
  arrange(game_date)

# Display all lineups
print(tennessee_lineups, n = Inf)

# Check if all lineups are unique
cat("\nTotal games:", nrow(tennessee_lineups))
cat("\nUnique lineups:", n_distinct(tennessee_lineups$lineup_id))
cat("\nAll different?:", nrow(tennessee_lineups) == n_distinct(tennessee_lineups$lineup_id))
