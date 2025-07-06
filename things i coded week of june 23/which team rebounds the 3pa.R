#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, stringr, progressr, BasketballAnalyzeR)

#load data
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp()
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box()
})
tictoc::toc()

wnba_pbp$text
colnames(wnba_pbp) 

wnba_pbp <- wnba_pbp %>%
  select(game_id, game_play_number, text, type_text, team_id)

wnba_pbp


missed3_reb <- wnba_pbp %>% 
  arrange(game_id, game_play_number) %>%  #make sure the log is in order
  group_by(game_id) %>% 
  mutate(
    next_type = lead(type_text),    #what happens one line later?
    next_team = lead(team_id)) %>% 
  ungroup() %>% 
  #keep only missed 3-point attempts
  filter(str_detect(text, "misses") & str_detect(text, regex("three", ignore_case = TRUE))) %>% 
  #make sure the very next event is a rebound
  filter(str_detect(next_type, "Rebound")) %>% 
  #check rebounding team
  mutate(reb_side = if_else(next_team == team_id, "Offensive", "Defensive"))

# Summary: how often does each side rebound a missed three?
missed3_reb %>% 
  count(reb_side, name = "n_rebounds") %>%               # counts with a condition[2]
  mutate(share = n_rebounds / sum(n_rebounds))           # convert to percentages


missed2_reb <- wnba_pbp %>% 
  arrange(game_id, game_play_number) %>%      # make sure events are in order
  group_by(game_id) %>% 
  mutate(
    next_type = lead(type_text),              # next event
    next_team = lead(team_id)
  ) %>% 
  ungroup() %>% 
  # 1. keep missed shots that are *not* threes
  filter(str_detect(text, "misses") & !str_detect(text, regex("three", ignore_case = TRUE))) %>% 
  filter(str_detect(next_type, "Rebound") & !str_detect(type_text, "Free Throw")) %>% 
  mutate(reb_side = if_else(next_team == team_id, "Offensive", "Defensive"))

missed2_reb %>% 
  count(reb_side, name = "n_rebounds") %>% 
  mutate(share = n_rebounds / sum(n_rebounds))
