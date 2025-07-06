#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, stringr)

#load data
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(season=2023)
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp(season=2023)
})
tictoc::toc()

players <- c("Nneka Ogwumike", "Elizabeth Williams", "Morgan Bertsch") 

game_ids_all_players <- wnba_player_box %>% 
  filter(athlete_display_name %in% players) %>% # keep only the three players
  distinct(game_id, athlete_display_name) %>%        
  count(game_id, name = "n_players") %>%            
  filter(n_players == length(players)) %>%       
  pull(game_id)                        

game_ids_all_players

away_game_dates <- wnba_player_box %>%                         # player box
  filter(team_display_name == "Los Angeles Sparks",            # Sparks rows
         game_id %in% game_ids_all_players,                    # games w/ all 3
         home_away == "away") %>%                              # road only
  distinct(game_id, game_date) %>%                             # keep both cols
  pull(game_date)                                              # return dates

away_game_dates

#2023-06-30
pbp <- wnba_pbp %>%
  filter(game_id==401507220)

rows_both <- pbp %>% 
  filter(
    str_detect(text, regex("Nneka Ogwumike", ignore_case = TRUE)) 
    & str_detect(text, regex("\\bdriving\\b", ignore_case = TRUE))
  ) %>%
  select("text", "period", "time", "game_date")

print(rows_both)
