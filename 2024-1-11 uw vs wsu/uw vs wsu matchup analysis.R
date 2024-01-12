#how to get the data with wehoop package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html
# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr)


#equivalent in NCAAW:
tictoc::tic()
progressr::with_progress({
  wbb_pbp <- wehoop::load_wbb_pbp()
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_team_box <- wehoop::load_wbb_team_box()
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box()
})
tictoc::toc()

#put everything into the correct form for the BasketballAnalyzeR package
library(BasketballAnalyzeR)

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
  
  
#calculate game length to calculate the amount of minutes each team played
wbb_pbp <- wbb_pbp %>%
  group_by(game_id) %>%
  mutate(
    game_qtrs = max(qtr),
    game_length = case_when(
      game_qtrs == 4 ~ 40,
      game_qtrs == 5 ~ 45,
      game_qtrs == 6 ~ 50,
      game_qtrs == 7 ~ 55,
      game_qtrs == 8 ~ 60,
      TRUE ~ NA_integer_
    )
  )

#add these minute calculations to the wbb_team_box dataframe
wbb_team_box <- wbb_team_box %>%
  left_join(wbb_pbp %>% select(game_id, game_length), by = "game_id") %>%
  distinct(game_id, team_location, .keep_all = TRUE)

  
Tbox <- wbb_team_box %>%
  group_by(season, team_location) %>%
  summarise(GP=n(), 
  			MIN=sum(game_length, na.rm = TRUE), 
            PTS=sum(team_score, na.rm = TRUE),
            W=sum(team_winner==TRUE), 
            L=sum(team_winner==FALSE), 
            P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), 
            P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), 
            P2p=100*P2M/P2A, 
            P3M=sum(three_point_field_goals_made, na.rm = TRUE), 
            P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), 
            P3p=100*P3M/P3A, 
            FTM=sum(free_throws_made, na.rm = TRUE), 
            FTA=sum(free_throws_attempted, na.rm = TRUE), 
            FTp=100*FTM/FTA, 
            OREB=sum(offensive_rebounds, na.rm = TRUE), 
            DREB=sum(defensive_rebounds, na.rm = TRUE), 
            AST=sum(assists, na.rm = TRUE), 
            TOV=sum(turnovers, na.rm = TRUE), 
            STL=sum(steals, na.rm = TRUE), 
            BLK=sum(blocks, na.rm = TRUE), 
            PF=sum(fouls, na.rm = TRUE), 
            PM=sum(team_score-opponent_team_score, na.rm=TRUE)) %>%
  rename(Season=season, Team=team_location) %>%
  as.data.frame()
  

Obox <- wbb_team_box %>%
  group_by(season, opponent_team_location) %>%
  summarise(GP=n(), 
  			MIN=sum(game_length, na.rm = TRUE),
            PTS=sum(team_score, na.rm = TRUE),
            W=sum(team_winner==TRUE), 
            L=sum(team_winner==FALSE), 
            P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), 
            P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), 
            P2p=100*P2M/P2A, 
            P3M=sum(three_point_field_goals_made, na.rm = TRUE), 
            P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), 
            P3p=100*P3M/P3A, 
            FTM=sum(free_throws_made, na.rm = TRUE), 
            FTA=sum(free_throws_attempted, na.rm = TRUE), 
            FTp=100*FTM/FTA, 
            OREB=sum(offensive_rebounds, na.rm = TRUE), 
            DREB=sum(defensive_rebounds, na.rm = TRUE), 
            AST=sum(assists, na.rm = TRUE), 
            TOV=sum(turnovers, na.rm = TRUE), 
            STL=sum(steals, na.rm = TRUE), 
            BLK=sum(blocks, na.rm = TRUE), 
            PF=sum(fouls, na.rm = TRUE), 
            PM=sum(team_score-opponent_team_score, na.rm=TRUE)) %>%
  rename(Season=season, Team=opponent_team_location) %>%
  as.data.frame()
  
#visualizations from BasketballAnalyzeR package

#Four Factors
tm <- c("Washington", "Washington State", "Arizona", "Arizona State", "California", "Stanford", "Colorado", "Utah", "Oregon", "Oregon State", "UCLA", "USC")
selTeams <- which(Tbox$Team%in% tm)
FF.sel <- fourfactors(Tbox[selTeams,], Obox[selTeams,])
listplots <- plot(FF.sel)
library(gridExtra) 
grid.arrange(grobs=listplots[2:2], ncol=1)


#bubble plots
attach(Tbox[selTeams,])
X <- data.frame(T=Team, P2p, P3p, FTp, AS=P2A+P3A+FTA)
detach(Tbox[selTeams,])
labs <- c("2-point shots (% made)","3-point shots (% made)","free throws (% made)", "Total shots attempted")
bubbleplot(X, id="T", x="P2p", y="P3p", col="FTp", size="AS", labels=labs, title="Pac-12 WBB team stats through 1/10/2024")