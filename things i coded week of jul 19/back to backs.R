#Use wehoop package to download WNBA data

#this loads all the packages I need at once, i could instead just do "library(wehoop)" for each package
pacman::p_load(wehoop, dplyr, tidyr, lubridate, glue, tictoc, progressr, lubridate)

#load data
#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box()
})
tictoc::toc()

#convert date and arrange
wnba_team_box <- wnba_team_box %>%
  mutate(game_date = as.Date(game_date)) %>%
  arrange(team_name, game_date)

# update wnba_team_box with days since last game
wnba_team_box <- wnba_team_box %>%
  arrange(team_name, game_date) %>%
  group_by(team_name, season) %>%
  mutate(
    prev_game_date = lag(game_date),
    days_since_last_game = as.integer(game_date - prev_game_date),
    back_to_back = if_else(is.na(prev_game_date), FALSE, days_since_last_game == 1)
  ) %>%
  ungroup()

#keep second game
b2b_games <- wnba_team_box %>%
  filter(back_to_back == TRUE)

#summarize win/loss by home/away
b2b_summary <- b2b_games %>%
  mutate(result = if_else(team_winner, "Win", "Loss")) %>%
  group_by(team_home_away, result) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = result, values_from = n, values_fill = 0) %>%
  mutate(
    total = Win + Loss,
    win_rate = Win / total
  )

print(b2b_summary)

b2b_games %>% select("game_date", "team_name", "team_home_away", "team_winner")
