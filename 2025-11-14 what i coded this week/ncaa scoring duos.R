#Use wehoop package to download WNBA data

#this loads all the packages I need at once, i could instead just do "library(wehoop)" for each package
pacman::p_load(wehoop, dplyr, tidyr, lubridate, glue, tictoc, progressr, lubridate, ggplot2)

#load data
#wbb player full box score
tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box(season=c(2025:2026))
})
tictoc::toc()


ggplot(wbb_player_box%>%filter(season==2026), aes(x = minutes, y = points)) +
  geom_point(alpha = 0.3, color = "black", size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "Player Minutes vs Points",
    x = "Minutes Played",
    y = "Points Scored"
  ) +
  scale_y_continuous(breaks = seq(0, 40, by = 5)) +
  theme_minimal()


ratio <- wbb_player_box %>%
  filter(minutes>10) %>%
  mutate(ppm=points/minutes) %>%
  select(game_date, athlete_display_name, team_location, points, minutes, ppm) %>%
  arrange(desc(ppm)) %>%
  head(10)

ratio

##############################################################
#top returning scoring duos
#Calculate PPG for last season (2024-25) for returning players
last_season_ppg <- wbb_player_box %>%
  filter(season == 2025 & did_not_play==FALSE) %>%
  group_by(athlete_id, athlete_display_name, team_location, team_name) %>%
  summarize(
    total_points = sum(points, na.rm = TRUE),
    games_played = n(),
    ppg = if_else(games_played > 0, total_points / games_played, 0),
    .groups = 'drop'
  ) %>%
  filter(games_played>3)

#Find players who played this season (returning)
current_season_players <- wbb_player_box %>%
  filter(season == 2026 & did_not_play==FALSE) %>%
  distinct(athlete_id, team_location)

#Filter to only returning players
returning_players <- last_season_ppg %>%
  semi_join(current_season_players, by = c("athlete_id", "team_location"))

#Find all possible pairs on the same team
duo_scores <- returning_players %>%
  inner_join(
    returning_players, 
    by = "team_location", 
    suffix = c("_1", "_2")
  ) %>%
  filter(athlete_id_1 < athlete_id_2) %>%  #avoid duplicate pairs
  mutate(combined_ppg = ppg_1 + ppg_2) %>%
  arrange(desc(combined_ppg))

#top duos
head(duo_scores%>%select(team_location, athlete_display_name_1, athlete_display_name_2, combined_ppg), 10)


# Get Iowa State returning players' PPG from last season
iowa_state_check <- last_season_ppg %>%
  filter(team_location == "Iowa State")

# Check if they played this season too
iowa_state_returning <- wbb_player_box %>%
  filter(season == 2026, team_location == "Iowa State") %>%
  distinct(athlete_display_name)

# Show their PPG
iowa_state_check %>%
  semi_join(iowa_state_returning, by = "athlete_display_name")