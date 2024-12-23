#Use wehoop package to download NCAAw data
pacman::p_load(wehoop, dplyr, stringr, glue, tictoc, progressr, BasketballAnalyzeR)

#equivalent in NCAAW:
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

tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box()
})
tictoc::toc()
  
#shooting plays from each game
game1 <- subset(wbb_pbp, game_date=="2024-12-20" & home_team_name=="Stanford" & shooting_play==TRUE)
game1 <- game1 %>%
  select(game_play_number, type_text, text, period_number, clock_display_value) %>%
  filter(str_detect(text, regex("Three", ignore_case = TRUE)))

game2 <- subset(wbb_pbp, game_date=="2024-12-20" & home_team_name=="UCLA" & shooting_play==TRUE)
game2 <- game2 %>%
  select(game_play_number, type_text, text, period_number, clock_display_value) %>%
  filter(str_detect(text, regex("Three", ignore_case = TRUE)))

#save to desktop as csv
write.csv(game1, "~/Desktop/ohiostate_stanford.csv", row.names = FALSE)
write.csv(game2, "~/Desktop/ucla_creighton.csv", row.names = FALSE)