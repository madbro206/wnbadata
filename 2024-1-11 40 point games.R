#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR)

#load data
#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(season=c(2021:2024))
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_team_box <- wehoop::load_wbb_team_box()
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box(season=c(2022:2025))
})
tictoc::toc()


games_40 <- subset(wbb_player_box, points>=40) %>%
  select("season", "game_date", "athlete_display_name", "team_location", "field_goals_made", "field_goals_attempted", "points") %>%
  arrange(desc(game_date))
games_40

wnba_games_40 <- subset(wnba_player_box, points>=40) %>%
  select("season", "game_date", "athlete_display_name", "team_name", "opponent_team_name", "field_goals_made", "field_goals_attempted", "points")
wnba_games_40


#stats from sports reference
ivy_league_stats <- read.csv("~/Desktop/ivy_league_stats.csv")

#label top players only
filtered_stats <- ivy_league_stats[(ivy_league_stats$Rk >= 1 & ivy_league_stats$Rk <= 6) | ivy_league_stats$PTS > 15, ]


ggplot(ivy_league_stats, aes(x=STL., y=PTS)) +
  geom_point() +
  geom_text(data = filtered_stats, 
            aes(label = Player), 
            hjust = .8, 
            vjust = -.2, 
            nudge_x = 0.1, 
            nudge_y = 0.1,
          size=3) +
    labs(
      title = "Ivy League wbb PPG & SPG Through 1/9/25",
      x = "Steals Per Game",
      y = "Points Per Game"
    ) +
  theme_minimal()
