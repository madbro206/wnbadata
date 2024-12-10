#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package
#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, tidyr, glue, tictoc, progressr)

#load data
tictoc::tic()
progressr::with_progress({
  wbb_team_box <- wehoop::load_wbb_team_box()
})
tictoc::toc()


#edit input to correct format
result <- wbb_team_box %>%
  select(game_id, game_date, team_location, team_home_away, team_score, 
         field_goals_attempted, offensive_rebounds, team_turnovers, free_throws_attempted) %>%
  pivot_wider(
    id_cols = c(game_id, game_date),
    names_from = team_home_away,
    values_from = c(team_location, team_score, field_goals_attempted, offensive_rebounds, team_turnovers, free_throws_attempted)) %>%
  transmute(
    team = team_location_home,
    opponent = team_location_away,
    date = as.Date(game_date),
    hca = 1,
    points = team_score_home,
    fga = field_goals_attempted_home,
    orb = offensive_rebounds_home,
    tov = team_turnovers_home,
    fta = free_throws_attempted_home,
    opp_points = team_score_away,
    opp_fga = field_goals_attempted_away,
    opp_orb = offensive_rebounds_away,
    opp_tov = team_turnovers_away,
    opp_fta = free_throws_attempted_away
  ) %>%
  bind_rows(
    transmute(., 
      team = opponent,
      opponent = team,
      date = date,
      hca = -1,
      points = opp_points,
      fga = opp_fga,
      orb = opp_orb,
      tov = opp_tov,
      fta = opp_fta,
      opp_points = points,
      opp_fga = fga,
      opp_orb = orb,
      opp_tov = tov,
      opp_fta = fta
    )
  ) %>%
  arrange(date, team)

# View the result
print(result)

#save as csv
write.csv(result, "~/Desktop/wehoop/2024-12-6 NET rankings/wbb_stats_input_dec6.csv", row.names = FALSE)
