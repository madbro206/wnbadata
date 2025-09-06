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

############################### first attempt ##########################################
#didn't work because I thought the scoring play row would only be true for the scoring team, but it's not :/
#I would need to infer which team (home/away) scored based on score change

#join dataframes to add win column to pbp (why is it not there already?)
dataframe <- wnba_pbp %>%
  left_join(wnba_team_box %>% select(game_id, team_id, team_name, team_winner, team_home_away), by=c("game_id", "team_id")) %>%
  select(game_id, season, game_date, game_play_number, team_name, team_home_away, period_number, away_score, home_score, scoring_play, score_value, team_winner) %>%
  filter(team_home_away=="home") #only need one instance of each game, so use home team perspective

#identify OT periods and pick first scoring play in each OT (period >= 5)
first_scores_ot <- dataframe %>%
  filter(season == 2025) %>% #for testing
  filter(scoring_play == TRUE, score_value > 0, period_number >=5) %>% #scoring play for points scored (just in case)
  group_by(game_id) %>%
  filter(period_number ==max(period_number, na.rm = TRUE)) %>% #look at last OT period
  arrange(game_play_number, .by_group = TRUE) %>%
  slice(1) %>% #first scoring play
  ungroup()

first_scores_ot %>%
  summarise(
    n_games = n(),
    wins = sum(team_winner, na.rm = TRUE),
    win_pct = mean(team_winner, na.rm = TRUE) * 100
  )


########################## second attempt ##########################################
#help from my boy chat gpt

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
first_scores_last_period <- df_full %>%
  filter(period_number >= 5) %>%                                   # OT candidates
  group_by(game_id) %>%
  filter(period_number == max(period_number, na.rm = TRUE)) %>%    # last OT only
  ungroup() %>%
  filter(scoring_play == TRUE, score_value > 0) %>%                 # scoring events only
  arrange(game_id, game_play_number, sequence_number) %>%           # stable ordering
  group_by(game_id) %>%
  slice_head(n = 1) %>%                                             # first score in last OT
  ungroup() %>%
  mutate(first_scorer = leader) %>%
  left_join(winners, by = "game_id") %>%
  mutate(first_scorer_won = first_scorer == winning_side) %>%
  arrange(desc(game_date))

first_scores_last_period %>% select(game_date, home_team_name, away_team_name, first_scorer, winning_side, first_scorer_won)

#summary data
summary_overall <- first_scores_last_period %>%
  #filter(first_scorer_won != NA) %>%
  summarise(
    n_games = n(),
    n_first_scorer_wins = sum(first_scorer_won, na.rm = TRUE),
    pct_first_scorer_wins = mean(first_scorer_won, na.rm = TRUE) 
  )

summary_overall


summary_overall %>%
  gt() %>%
  tab_header(
    title = "Win% for first scoring team in overtime", subtitle= "WNBA since 2006"
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
libs_only <- first_scores_last_period %>%
  filter(home_team_name == "Liberty" | away_team_name == "Liberty" & as.Date(game_date)>"2022-01-01") %>%
  summarise(
    n_games = n(),
    n_first_scorer_wins = sum(first_scorer_won, na.rm = TRUE),
    pct_first_scorer_wins = mean(first_scorer_won, na.rm = TRUE)
  ) 

first_scores_last_period %>%
    filter(home_team_name == "Liberty" | away_team_name == "Liberty" & as.Date(game_date)>"2022-01-01") %>%
  select(game_date, home_team_name, away_team_name, first_scorer, winning_side, first_scorer_won)

libs_only



libs_only %>%
  gt() %>%
  tab_header(
    title = "Win% for first scoring team in overtime", subtitle= "Sandy's Libs (since 2022)"
  ) %>%
  fmt_integer(columns = c(n_games, n_first_scorer_wins)) %>%
  fmt_percent(columns = pct_first_scorer_wins, decimals = 1) %>%
  cols_label(
    n_games = "Games",
    n_first_scorer_wins = "First-scorer wins",
    pct_first_scorer_wins = "Win rate"
  ) %>%
  tab_options(table.font.size = px(14))


















########################### another bad attempt below
#it turns out using the home perspective only was filtering out important rows that i needed to determine who scored first

#create scoring data i need
df <- wnba_pbp %>%
  left_join(wnba_team_box %>% select(game_id, team_id, team_home_away),
            by = c("game_id","team_id")) %>%
  filter(team_home_away == "home") %>% #use home perspective rows only (only need one instance of each game)
  select(game_id, season, game_date, game_play_number, period_number,
         home_score, away_score, scoring_play, score_value) %>%
  arrange(game_id, period_number, game_play_number) %>%
  group_by(game_id, period_number) %>%
  mutate(
    start_home = dplyr::first(home_score),
    start_away = dplyr::first(away_score),
    d_home_start = home_score - start_home,
    d_away_start = away_score - start_away,
    scoring_side = dplyr::case_when(
      scoring_play & score_value > 0 & (d_home_start - dplyr::lag(d_home_start, default = 0)) > 
        (d_away_start - dplyr::lag(d_away_start, default = 0)) ~ "home",
      scoring_play & score_value > 0 & (d_away_start - dplyr::lag(d_away_start, default = 0)) > 
        (d_home_start - dplyr::lag(d_home_start, default = 0)) ~ "away",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()



subset(wnba_pbp, game_id==401736329 & period_number ==5)

# Take first scoring play from the last period of each game
first_scores_last_period <- df %>%
  filter(scoring_play == TRUE, score_value > 0, period_number >= 5) %>% #scoring rows only, overtime games only
  group_by(game_id) %>%
  filter(period_number == max(period_number, na.rm = TRUE)) %>% #filter to max period for that game (last overtime)
  arrange(game_play_number) %>%
  slice_min(game_play_number, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(winners, by = "game_id") %>%
  mutate(first_scorer_won = scoring_side == winning_side) %>%
  arrange(desc(game_date))

first_scores_last_period$first_scorer_won

#summary data
summary_overall <- first_scores_last_period %>%
  #filter(first_scorer_won != NA) %>%
  summarise(
    n_games = n(),
    n_first_scorer_wins = sum(first_scorer_won, na.rm = TRUE),
    pct_first_scorer_wins = mean(first_scorer_won, na.rm = TRUE) * 100
  )

summary_overall


first_scores_last_period %>% arrange(desc(game_date)) %>% select(game_date, scoring_side, winning_side, home_team_name, away_team_name) 


subset(wnba_pbp, game_id==401736329 & period_number==5)$game_play_number

subset(df, game_id==401736329 & period_number==5)
