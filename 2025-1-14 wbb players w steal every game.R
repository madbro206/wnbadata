pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR)

#load data
tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box(season=c(2016:2025))
})
tictoc::toc()


#find players who have a steal in every game they have played (minutes not NA)
#active players
active_players_2025 <- wbb_player_box %>%
  filter(season == 2025) %>% #at least one game in 2025 season
  distinct(athlete_id)

players_with_steals <- wbb_player_box %>%
  filter(athlete_id %in% active_players_2025$athlete_id) %>%
  filter(!is.na(minutes)) %>%
  group_by(athlete_id, athlete_display_name) %>%
  summarise(
    games_played = n(),
    games_with_steals = sum(steals > 0, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(games_played == games_with_steals) %>% #only players where number of games = number of games with a steal
  arrange(desc(games_played))

print(players_with_steals)



#same calculation for assists
players_with_assists <- wbb_player_box %>%
  filter(athlete_id %in% active_players_2025$athlete_id) %>%
  filter(!is.na(minutes)) %>%
  group_by(athlete_id, athlete_display_name) %>%
  summarise(
    games_played = n(),
    games_with_assists = sum(assists > 0, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(games_played == games_with_assists) %>% #only players where number of games = number of games with an assist
  arrange(desc(games_played))

print(players_with_assists)


#blocks
players_with_blocks <- wbb_player_box %>%
  filter(athlete_id %in% active_players_2025$athlete_id) %>%
  filter(!is.na(minutes)) %>%
  group_by(athlete_id, athlete_display_name) %>%
  summarise(
    games_played = n(),
    games_with_blocks = sum(blocks > 0, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(games_played == games_with_blocks) %>% #only players where number of games = number of games with block
  arrange(desc(games_played))

print(players_with_blocks)



#double digit points
players_with_points <- wbb_player_box %>%
  filter(athlete_id %in% active_players_2025$athlete_id) %>%
  filter(!is.na(minutes)) %>%
  group_by(athlete_id, athlete_display_name) %>% #Flau'Jae Johnson  comes up in this query but it's because her name gets spelled wrong sometimes
  summarise(
    games_played = n(),
    games_with_points = sum(points >= 10, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(games_played == games_with_points) %>% #only players where number of games = number of games with double digit points
  arrange(desc(games_played))

print(players_with_points)