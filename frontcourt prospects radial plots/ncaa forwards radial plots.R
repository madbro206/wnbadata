#2.2.3 Radial Plots 

library(BasketballAnalyzeR)

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr)

tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box()
})
tictoc::toc()


Pbox <- wbb_player_box %>%
  group_by(season, team_location, athlete_display_name) %>%
  summarise(GP=n(), MIN=sum(minutes, na.rm = TRUE), PTS=sum(points, na.rm = TRUE),
    P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), P2p=100*P2M/P2A,
    P3M=sum(three_point_field_goals_made, na.rm = TRUE), P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), P3p=100*P3M/P3A,
    FTM=sum(free_throws_made, na.rm = TRUE), FTA=sum(free_throws_attempted, na.rm = TRUE), FTp=100*FTM/FTA,
    OREB=sum(offensive_rebounds, na.rm = TRUE), DREB=sum(defensive_rebounds, na.rm = TRUE), AST=sum(assists, na.rm = TRUE),
    TOV=sum(turnovers, na.rm = TRUE), STL=sum(steals, na.rm = TRUE), BLK=sum(blocks, na.rm = TRUE),
    PF=sum(fouls, na.rm = TRUE)) %>%
  rename(Season=season, Team=team_location,
         Player=athlete_display_name) %>%
  as.data.frame()
  

#use to visualize team and player profiles
#numeric values are plotted as distances from the center of a circular field
#criticised because it makes you focus on the area which doesn't actually mean anything
#use function radialprofile, flexible choice of variables




#NCAA frontcourt wnba prospects
Pbox.C <- subset(Pbox, Player=="Cameron Brink" | Player=="Angel Reese"| Player=="Aaliyah Edwards"| Player=="Kamilla Cardoso"| Player=="Rickea Jackson"| Player=="Elizabeth Kitley"| Player=="Mackenzie Holmes"| Player=="Alissa Pili"| Player=="Sedona Prince")
attach(Pbox.C)
X <- data.frame(P2M, P3M, FTM, REB=OREB+DREB, AST, STL, BLK)/MIN
detach(Pbox.C)
radialprofile(data=X, title=Pbox.C$Player, std=TRUE)