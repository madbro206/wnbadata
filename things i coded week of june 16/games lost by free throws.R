#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package
#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR)

tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season=c(2024:2025))
})
tictoc::toc()

games <- wnba_team_box %>%
  filter(team_winner==FALSE)%>%
  filter(free_throws_attempted-free_throws_made >= opponent_team_score-team_score) %>%
  mutate(free_throws_missed=free_throws_attempted-free_throws_made) %>%
  mutate(margin=opponent_team_score-team_score) %>%
  select(season, game_date, team_name, free_throws_attempted, free_throws_missed, margin)
 
print(subset(games, season==2025), n=63)
