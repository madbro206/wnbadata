#Use wehoop package to download WNBA or NCAAW data

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR)

#NCAAW
tictoc::tic()
progressr::with_progress({
  wbb_pbp <- wehoop::load_wbb_pbp()
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_team_box <- wehoop::load_wbb_team_box()
})
tictoc::toc()
  
#calculate game length to calculate the amount of minutes each team played
wbb_pbp <- wbb_pbp %>%
  group_by(game_id) %>%
  mutate(
    game_qtrs = max(qtr),
    game_length = case_when(
      game_qtrs == 4 ~ 40,
      game_qtrs == 5 ~ 45,
      game_qtrs == 6 ~ 50,
      game_qtrs == 7 ~ 55,
      game_qtrs == 8 ~ 60,
      TRUE ~ NA_integer_
    )
  )

#add these minute/quarter calculations to the wbb_team_box dataframe
wbb_team_box <- wbb_team_box %>%
  left_join(wbb_pbp %>% select(game_id, game_qtrs, game_length), by = "game_id") %>%
  distinct(game_id, team_location, .keep_all = TRUE)
  

#wbb_team_box contains only rows where consecutive games for each team had game_qtrs >= 6
wbb_team_box6 <- wbb_team_box %>%
  group_by(team_location) %>%
  arrange(game_date) %>% #make sure the games are in date order (they probably are but I just wanna make sure)
  mutate(consecutive_6_qtrs = game_qtrs >= 6,
         next_consecutive_6_qtrs = lead(consecutive_6_qtrs),
         has_consecutive_6_qtrs = consecutive_6_qtrs & next_consecutive_6_qtrs) %>%
  filter(has_consecutive_6_qtrs) %>%
  select(-next_consecutive_6_qtrs, -has_consecutive_6_qtrs)
  
#wbb_team_box contains only rows where consecutive games for each team had game_qtrs >= 5
wbb_team_box5 <- wbb_team_box %>%
  group_by(team_location) %>%
  arrange(game_date) %>% #make sure the games are in date order (they probably are but I just wanna make sure)
  mutate(consecutive_5_qtrs = game_qtrs >= 5,
         next_consecutive_5_qtrs = lead(consecutive_5_qtrs),
         has_consecutive_5_qtrs = consecutive_5_qtrs & next_consecutive_5_qtrs) %>%
  filter(has_consecutive_5_qtrs) %>%
  select(-next_consecutive_5_qtrs, -has_consecutive_5_qtrs)

wbb_team_box5