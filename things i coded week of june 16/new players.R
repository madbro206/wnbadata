#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR)


tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(season=c(2024:2025))
})
tictoc::toc()
 
w2024 <- subset(wnba_player_box, season==2024)
w2025 <- subset(wnba_player_box, season==2025)

#unique player/team combos for each season
players_2024 <- w2024 %>%
  distinct(athlete_id, athlete_display_name, team_name)

players_2025 <- w2025 %>%
  distinct(athlete_id, athlete_display_name, team_name)

#players who are on the/a same team as they were on in 2024
same_team <- inner_join(players_2024, players_2025, by = c("athlete_id", "team_name"))
print(same_team, n=64)

# Players in 2025 who are new OR changed teams
changed_or_new_players <- anti_join(
  players_2025, 
  players_2024, 
  by = c("athlete_id", "team_name")
) %>% arrange(team_name)

print(changed_or_new_players, n=109)



dupes <- changed_or_new_players %>% 
  add_count(athlete_id) %>%   # creates column n = number of times id appears
  filter(n > 1) %>%           # keep only ids seen more than once
  select(-n)                  # drop helper column if you like
