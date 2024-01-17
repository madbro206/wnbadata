#how to get the data

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR)

tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box(c(2009:2024))
})
tictoc::toc()

#data frame with cumulative box score stats for every player
Pbox <- wbb_player_box %>%
  group_by(season, team_location, athlete_display_name) %>%
  summarise(GP=sum(minutes>0, na.rm=TRUE), MIN=sum(minutes, na.rm = TRUE), PTS=sum(points, na.rm = TRUE),
    P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), P2p=100*P2M/P2A,
    P3M=sum(three_point_field_goals_made, na.rm = TRUE), P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), P3p=100*P3M/P3A,
    FTM=sum(free_throws_made, na.rm = TRUE), FTA=sum(free_throws_attempted, na.rm = TRUE), FTp=100*FTM/FTA,
    OREB=sum(offensive_rebounds, na.rm = TRUE), DREB=sum(defensive_rebounds, na.rm = TRUE), AST=sum(assists, na.rm = TRUE),
    TOV=sum(turnovers, na.rm = TRUE), STL=sum(steals, na.rm = TRUE), BLK=sum(blocks, na.rm = TRUE),
    PF=sum(fouls, na.rm = TRUE)) %>%
  rename(Season=season, Team=team_location,
         Player=athlete_display_name) %>%
         filter(GP>5) %>%
  as.data.frame()
  
#find average box score stats for each player, taking only the ones I care about for this video :)
Pbox_avg <- Pbox %>%
  mutate(ppg = round(PTS/GP, 2),
         apg = round(AST/GP, 2),
         tpg = round(TOV/GP, 2)) %>%
  select(Season, Player, GP, Team, ppg, apg, tpg) %>%
  arrange(desc(ppg))

#players that meet the criteria that I saw in the post
filtered <- Pbox_avg %>%
	filter(ppg>=20, apg >=5, tpg <= 2.5, GP>10) #GP>10 is arbitrary

#players meeting criteria minus turnovers
filtered_wo_tov <- Pbox_avg %>%
	filter(ppg>=20, apg >=5, GP>10)%>%
	arrange(tpg)
	
#players close to meeting any of the criteria
close_players <- Pbox_avg %>%
	filter((ppg>=20, apg >=5, GP>10)