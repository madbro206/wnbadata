library(wehoop)
library(dplyr)
library(knitr)

#wnba team full box score
wnba_pbp <- wehoop::load_wnba_pbp(season=c(2006:2025))

wnba_player_box <- wehoop::load_wnba_player_box(season=c(2006:2025))

wnba_pbp_with_teams <- wnba_pbp %>%
  filter(!is.na(athlete_id_1)) %>%
  left_join(
    wnba_player_box %>% select(game_id, athlete_id, athlete_display_name, team_name),
    by = c("game_id" = "game_id", "athlete_id_1" = "athlete_id")
  )

wnba_pbp_with_teams %>% filter(is.na(team_name))


#for some reason i looked for quarters where there was only one scoring play, this would be a cheap answer
single_scoring_quarters <- wnba_pbp_with_teams %>%
  filter(scoring_play == TRUE) %>%
  filter(qtr <=4) %>%
  group_by(game_id, qtr, team_name) %>%
  mutate(n_scoring_plays = n()) %>%
  ungroup() %>%
  filter(n_scoring_plays == 1) %>%
  select(game_date, qtr, athlete_id_1, team_id, text)

single_scoring_quarters

#dataframe showing all the games where there was a quarter where a team had only one player score points
df <- wnba_pbp_with_teams %>%
  filter(scoring_play == TRUE) %>%
  filter(qtr <= 4) %>% #no overtime pls
  group_by(game_id, qtr, team_name) %>%
  summarise(
    n_scorers = n_distinct(athlete_id_1), # count unique scorers for this team/quarter/game
    game_date = first(game_date),
    season_type = first(season_type),
    athlete_display_name = first(athlete_display_name),
    points = sum(score_value), #note, some of the point values are wrong because it's counting the FTs wrong and idk how to fix it atm
    .groups = "drop"
  ) %>%
  filter(n_scorers == 1) %>%
  filter(!is.na(team_name)) %>%
  select(game_date, qtr, team_name, athlete_display_name, points, season_type)

print(df, n=26)

#playoff games
playoffs<- df %>% filter(season_type==3)

cat("Quarters with Single Scorer")
knitr::kable(df)


cat("Playoff Quarters with Single Scorer")
knitr::kable(playoffs)