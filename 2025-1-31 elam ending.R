#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR)

#load data

#wnba full play by play
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp(season=2024)
})
tictoc::toc()

#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season=2024)
})
tictoc::toc()

example <- subset(wnba_pbp, game_id==401620335)

example <- example %>%
  filter(period_number==4)

"""
last_plays <- wnba_pbp %>%
  filter(period_number == 4) %>%
  group_by(game_id) %>%
  slice_max(order_by = desc(end_game_seconds_remaining), n = 2) %>%
  filter(end_game_seconds_remaining == 0 | is.na(end_game_seconds_remaining) | 
         row_number() == 1) %>%
  ungroup() %>%
  arrange(game_id) %>%
  select(game_id, game_date, home_team_name,  type_text, text, away_score, home_score) %>%
  filter(type_text != "End Period")

last_plays

#save as csv
write.csv(last_plays, "~/Desktop/last_plays_2024.csv", row.names = FALSE)
"""

last_plays_close_game <- wnba_pbp %>%
  filter(period_number == 4) %>%
  group_by(game_id) %>%
  filter(end_game_seconds_remaining <3 & shooting_play==TRUE & abs(away_score-home_score) <=3 & season_type==2 & game_date <= '2024-09-19') %>%
  ungroup() %>%
  arrange(game_id) %>%
  select(game_id, game_date, home_team_name, end_game_seconds_remaining, type_text, text, away_score, home_score) %>%
  filter(type_text != "End Period")

print(last_plays_close_game, n=57)
length(unique(last_plays_close_game$game_id))

last_plays_any_score <- wnba_pbp %>%
  filter(period_number == 4) %>%
  group_by(game_id) %>%
  filter(end_game_seconds_remaining <3 & shooting_play==TRUE & season_type==2 & game_date <= '2024-09-19') %>%
  ungroup() %>%
  arrange(game_id) %>%
  select(game_id, game_date, home_team_name, end_game_seconds_remaining, type_text, text, away_score, home_score) %>%
  filter(type_text != "End Period")

print(last_plays_any_score, n=163)
length(unique(last_plays_any_score$game_id))
