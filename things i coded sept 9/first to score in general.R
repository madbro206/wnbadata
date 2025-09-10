#Use wehoop package to download WNBA data
pacman::p_load(wehoop, dplyr, glue, tictoc, progressr)
library(gt)

#load data
#wnba full play by play
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp(season=c(2003:2025))
})
tictoc::toc()

#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season=c(2003:2025))
})
tictoc::toc()


#home team data
home_teams <- wnba_team_box %>%
  filter(team_home_away == "home") %>%
  distinct(game_id, .keep_all = TRUE) %>%
  transmute( #transmute is like a mutate/select combo
    game_id,
    home_team_name = team_name,
    home_team_winner = team_winner
  )

#away team data
away_teams <- wnba_team_box %>%
  filter(team_home_away == "away") %>%
  distinct(game_id, .keep_all = TRUE) %>%
  transmute(
    game_id,
    away_team_name = team_name,
    away_team_winner = team_winner
  )

#figure out if home or away team won each game
winners <- home_teams %>%
  left_join(away_teams, by = "game_id") %>%
  mutate(
    winning_side = case_when(
      home_team_winner ~ "home",
      away_team_winner ~ "away",
      TRUE ~ NA_character_
    )
  ) %>%
  select(game_id, home_team_name, away_team_name, winning_side)

#################
# Build full df without dropping away rows
df_full <- wnba_pbp %>%
  select(game_id, season, game_date, game_play_number, sequence_number,
         period_number, home_score, away_score, scoring_play, score_value, team_id) %>%
  arrange(game_id, period_number, game_play_number) %>%
  group_by(game_id, period_number) %>%
  mutate(
    lead_val = sign(home_score - away_score),
    leader   = dplyr::case_when(
      lead_val > 0 ~ "home",
      lead_val < 0 ~ "away",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

# winners has columns: game_id, winning_side ("home"/"away")
first_scores_first_period <- df_full %>%
  filter(period_number == 1) %>%                                   #first quarter only
  filter(scoring_play == TRUE, score_value > 0) %>%                 # scoring events only
  arrange(game_id, game_play_number, sequence_number) %>%           # stable ordering
  group_by(game_id) %>%
  slice_head(n = 1) %>%                                             # first score
  ungroup() %>%
  mutate(first_scorer = leader) %>%
  left_join(winners, by = "game_id") %>%
  mutate(first_scorer_won = first_scorer == winning_side) %>%
  arrange(desc(game_date))

first_scores_first_period %>% select(game_date, home_team_name, away_team_name, first_scorer, winning_side, first_scorer_won)

#summary data
summary_overall <- first_scores_first_period %>%
  summarise(
    n_games = n(),
    n_first_scorer_wins = sum(first_scorer_won, na.rm = TRUE),
    pct_first_scorer_wins = mean(first_scorer_won, na.rm = TRUE) 
  )

summary_overall

#look pretty
summary_overall %>%
  gt() %>%
  tab_header(
    title = "Win% for first scoring team in the game", subtitle= "WNBA since 2006"
  ) %>%
  fmt_integer(columns = c(n_games, n_first_scorer_wins)) %>%
  fmt_percent(columns = pct_first_scorer_wins, decimals = 1) %>%
  cols_label(
    n_games = "Games",
    n_first_scorer_wins = "First-scorer wins",
    pct_first_scorer_wins = "Win rate"
  ) %>%
  tab_options(table.font.size = px(14))
  



#sandy's libs only                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            b
libs_only <- first_scores_first_period %>%
  filter((home_team_name == "Liberty" | away_team_name == "Liberty") & as.Date(game_date)>"2022-01-01") %>%
  summarise(
    n_games = n(),
    n_first_scorer_wins = sum(first_scorer_won, na.rm = TRUE),
    pct_first_scorer_wins = mean(first_scorer_won, na.rm = TRUE)
  ) 

libs_only

#look pretty
libs_only %>%
  gt() %>%
  tab_header(
    title = "Win% for first scoring team in the game", subtitle= "Sandy's Libs (since 2022)"
  ) %>%
  fmt_integer(columns = c(n_games, n_first_scorer_wins)) %>%
  fmt_percent(columns = pct_first_scorer_wins, decimals = 1) %>%
  cols_label(
    n_games = "Games",
    n_first_scorer_wins = "First-scorer wins",
    pct_first_scorer_wins = "Win rate"
  ) %>%
  tab_options(table.font.size = px(14))

