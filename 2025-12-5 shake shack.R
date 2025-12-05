library(tidyr)
library(dplyr)
library(ggplot2)
library(wehoop)
library(tictoc)
library(progressr)
library(stringr)

#load data
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp()
  wnba_team_box <- wehoop::load_wnba_team_box()
  wnba_player_box <- load_wnba_player_box()
})
tictoc::toc()

#miss twice get a slice
wnba_pbp_2025 <-wnba_pbp %>%filter(season==2025 & home_team_name=="New York") %>% select(athlete_id_1,id, sequence_number, game_id, game_date, type_text, text, away_score, home_score, period, clock_display_value, scoring_play, score_value)
wnba_pbp_2025$text


# 1) Tag each FT event with the shooter’s team (from player box)
pbp_with_team <- wnba_pbp_2025 %>%
  # keep only events with a shooter
  filter(str_detect(text, "free throw")) %>%
  left_join(
    wnba_player_box %>%
      select(game_id, athlete_id, team_name, home_away),
    by = c("game_id", "athlete_id_1" = "athlete_id")
  )

# Now pbp_with_team$team_name = shooter’s team; home_away = that team’s home/away flag

# 2) Restrict to lib opponents (shooters whose team is not the libs)
lib_opp_ft <- pbp_with_team %>%
  filter(team_name != "Liberty")   # these are all opponent FT events in Lib home games


lib_miss_2of2 <- lib_opp_ft |>
  # keep only away-team free throws
  filter(
    str_detect(text, "free throw"),
    # away team is shooting: away_score is the team that could change
    TRUE
  ) |>
  arrange(game_id, period, sequence_number) |>
  group_by(game_id) |>
  mutate(
    shooter   = str_extract(text, "^[^m]+"),  # crude shooter name grab if needed
    is_miss   = str_detect(text, "misses free throw"),
    ft_1of2   = str_detect(text, "free throw 1 of 2"),
    ft_2of2   = str_detect(text, "free throw 2 of 2"),
    next_miss = lead(is_miss),
    next_2of2 = lead(ft_2of2),
    next_text = lead(text),
    away_score_next = lead(away_score),
    home_score_next = lead(home_score)
  ) |>
  # look for 1-of-2 miss followed immediately by 2-of-2 miss
  filter(
    ft_1of2 & is_miss,          # this row is missed 1 of 2
    next_miss,                  # next row is a miss
    next_2of2,                  # next row is 2 of 2
    away_score_next == away_score,  # no points added for away
    home_score_next == home_score   # and no weird score changes
  ) |>
  ungroup()

#times liberty opponents missed 2 of 2 at barclays
lib_miss_2of2 %>%select(game_date,text,period, clock_display_value)

#times liberty opponents missed 2 of 2 at barclays in the FOURTH!
lib_miss_2of2 %>% filter(period==4) %>% select(game_date,text,period, clock_display_value)


lib_opp_ft_trips_2 <- lib_opp_ft |>
  filter(
    str_detect(text, "free throw"),
  ) |>
  arrange(game_id, period, sequence_number) |>
  group_by(game_id) |>
  mutate(
    ft_1of2 = str_detect(text, "free throw 1 of 2"),
    ft_2of2 = str_detect(text, "free throw 2 of 2"),
    next_2of2 = lead(ft_2of2),
    next_game_id = lead(game_id)
  ) |>
  # each trip of exactly two is identified by the 1-of-2 row whose next row is 2-of-2 in same game
  filter(
    ft_1of2,
    next_2of2,
    next_game_id == game_id
  ) |>
  ungroup()

# number of opponent trips to the line for exactly two
n_trips_2 <- nrow(lib_opp_ft_trips_2)
n_trips_2

#times liberty opponents went to the line for two in the FOURTH QUARTER
print(lib_opp_ft_trips_2%>%filter(period==4) %>% select(game_date,text, period, clock_display_value), n=48)





####################################################
#opponent FT% by home team

opp_ft_home <- wnba_team_box %>%
  filter(
    season == 2025,
    team_home_away == "away"      # this row = visiting team
  ) %>%
  group_by(opponent_team_name) %>%  # opponent_team_name = the home team
  summarise(
    opp_ft_made = sum(free_throws_made, na.rm = TRUE),
    opp_ft_att  = sum(free_throws_attempted, na.rm = TRUE),
    opp_ft_pct  = opp_ft_made / opp_ft_att,
    .groups = "drop"
  ) %>%
  rename(
    home_team = opponent_team_name  # clearer label
  ) %>%
  arrange(desc(opp_ft_pct))

opp_ft_home %>% filter(home_team !="TEAM CLARK")
####################################################
pbp_ft <- wnba_pbp %>%
  filter(season == 2025,
         str_detect(text, "free throw")) %>%
  left_join(
    wnba_player_box %>%
      select(game_id, athlete_id, team_name, home_away),
    by = c("game_id", "athlete_id_1" = "athlete_id")
  )


ft_trips_2 <- pbp_ft %>%
  arrange(game_id, period, sequence_number) %>%
  group_by(game_id, team_name) %>%  # shooter’s team
  mutate(
    ft_1of2      = str_detect(text, "free throw 1 of 2"),
    ft_2of2      = str_detect(text, "free throw 2 of 2"),
    next_2of2    = lead(ft_2of2),
    next_team    = lead(team_name)
  ) %>%
  filter(
    ft_1of2,
    next_2of2,
    next_team == team_name          # same shooting team
  ) %>%
  ungroup()

# all enemy (away) trips for 2, whole game
away_trips_game <- ft_trips_2 %>%
  filter(home_away == "away" &home_team_name !="Team Clark") %>%
  group_by(home_team_name) %>%
  summarise(
    trips_all = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(trips_all))

# enemy trips for 2 in the 4th
away_trips_q4 <- ft_trips_2 %>%
  filter(home_away == "away", period == 4) %>%
  group_by(home_team_name) %>%
  summarise(
    trips_q4 = n(),
    .groups = "drop"
  )

# join and compute ratio
enemy_ft_ratio <- away_trips_game %>%
  left_join(away_trips_q4, by = "home_team_name") %>%
  mutate(
    trips_q4 = replace_na(trips_q4, 0L),
    ratio_q4_all = trips_q4 / trips_all
  ) %>%
  arrange(desc(ratio_q4_all))

enemy_ft_ratio

