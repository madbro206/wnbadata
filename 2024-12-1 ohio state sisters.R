#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, stringr)

#equivalent in NCAAW:
tictoc::tic()
progressr::with_progress({
  wbb_pbp <- wehoop::load_wbb_pbp()
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box()
})
tictoc::toc()

#######################################################################################################
#translate into player, team, and opponent box score

ohio_state_pbox <- subset(wbb_player_box, team_location=="Ohio State")

ohio_state_pbp <- subset(wbb_pbp, home_team_name=="Ohio State" | away_team_name=="Ohio State")

#sisters assist each other
filtered_df <- ohio_state_pbp %>%
  filter(str_detect(text, "Jaloni") & str_detect(text, "Kennedy")) %>%
  select('game_date', 'period_number', 'clock_display_value', 'text', 'home_team_name', 'away_team_name')
  
filtered_df
##############################################################################################################
#how often truongs assisted each other at gonzaga


tictoc::tic()
progressr::with_progress({
  wbb_pbp_19_24 <- wehoop::load_wbb_pbp(season=c(2019:2024))
})
tictoc::toc()

gonzaga_pbp <- subset(wbb_pbp_19_24, home_team_name=="Gonzaga" | away_team_name=="Gonzaga")

filtered_df2 <- gonzaga_pbp %>%
  filter(str_detect(text, "Kayleigh") & str_detect(text, "Kaylynne")) %>%
  select(game_date, text) %>%
  arrange(game_date)
  
print(filtered_df2, n=85)