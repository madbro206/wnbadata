#how to get the WNBA data

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr)

#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box()
})
tictoc::toc()

players <- wnba_player_box %>%
  select(athlete_id, athlete_display_name, athlete_headshot_href) %>%
  distinct(athlete_id, .keep_all = TRUE) %>%
  arrange(athlete_id)
  
write.csv(players, file = "", row.names = FALSE, quote = TRUE)