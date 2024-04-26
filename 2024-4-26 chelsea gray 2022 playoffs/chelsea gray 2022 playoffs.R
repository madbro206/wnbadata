#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, ggplot2)


#wnba full play by play
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp(season=2022)
})
tictoc::toc()


#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season=2022)
})

tictoc::toc()


#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(season=2022)
})
tictoc::toc()


Pbox <- wnba_player_box %>%
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
  as.data.frame()
  
#calculate game length to calculate the amount of minutes each team played
wnba_pbp <- wnba_pbp %>%
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
wnba_team_box <- wnba_team_box %>%
  left_join(wnba_pbp %>% select(game_id, game_length), by = "game_id") %>%
  distinct(game_id, team_location, .keep_all = TRUE)

#make sure each game has a length 
wnba_team_box <- wnba_team_box %>%
  mutate(
    game_length = case_when(
      is.na(game_length) ~ 40,  # Set to 40 if game_length is NA
      TRUE ~ game_length  # Keep the original value if not NA
    )
  )

  
Tbox <- wnba_team_box %>%
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
  

Obox <- wnba_team_box %>%
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

##############################################################################################################
#create playoff player box scores only
player_playoffs <- subset(wnba_player_box, season_type==3)

Pbox_playoffs <- player_playoffs %>%
  group_by(team_location, athlete_display_name) %>%
  summarise(GP=sum(minutes>0, na.rm=TRUE), MIN=sum(minutes, na.rm = TRUE), PTS=sum(points, na.rm = TRUE),
    P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), P2p=100*P2M/P2A,
    P3M=sum(three_point_field_goals_made, na.rm = TRUE), P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), P3p=100*P3M/P3A,
    FTM=sum(free_throws_made, na.rm = TRUE), FTA=sum(free_throws_attempted, na.rm = TRUE), FTp=100*FTM/FTA,
    OREB=sum(offensive_rebounds, na.rm = TRUE), DREB=sum(defensive_rebounds, na.rm = TRUE), AST=sum(assists, na.rm = TRUE),
    TOV=sum(turnovers, na.rm = TRUE), STL=sum(steals, na.rm = TRUE), BLK=sum(blocks, na.rm = TRUE),
    PF=sum(fouls, na.rm = TRUE)) %>%
  rename(Team=team_location,
         Player=athlete_display_name) %>%
  as.data.frame()
  
Pbox_playoffs

chelsea<- subset(Pbox_playoffs, Player=="Chelsea Gray")
chelsea

Pbox_playoffs$ApM <- Pbox_playoffs$AST/Pbox_playoffs$MIN
Pbox_playoffs$FGpM <- (Pbox_playoffs$P2M+Pbox_playoffs$P3M)/Pbox_playoffs$MIN
Pbox_playoffs$MpG <- Pbox_playoffs$MIN/Pbox_playoffs$GP

sub<- subset(Pbox_playoffs, P2M+P3M>=10)
#2p% vs 3p%, at least 10 FGM
ggplot(sub, aes(x = P2p, y = P3p, label = Player, color=P2M+P3M)) +
  geom_point() +
  geom_text(vjust = -0.5, hjust = 0.6, size=3.5) + # Adjust position of team labels
  labs(x = "2p%", y = "3p%", subtitle="players with at least 10 FGM, via wehoop") + # Axis labels
  ggtitle("WNBA Playoffs 2022 Player Shooting Stats") + # Plot title
  theme_minimal() # Minimal theme
  
#only players who averaged at least 20 minutes per game
subset<- subset(Pbox_playoffs, MpG>=20)

#field goals made vs assists (both per minute), played at least 20 minutes per game
ggplot(subset, aes(x = ApM, y = FGpM, label = Player, color=GP)) +
  geom_point() +
  geom_text(vjust = -0.5, hjust = 0.35, size=3) + # Adjust position of team labels
  labs(x = "Assists per minute", y = "Field goals per minute") + # Axis labels
  ggtitle("WNBA Playoffs 2022 Player Stats",subtitle="players with at least 20 min per game, via wehoop") + 
  theme_minimal() # Minimal theme
  
  
#field goal percentage vs assists per game, at least 20 mins per game
ggplot(subset, aes(x = AST/GP, y = (P2M+P3M)/(P2A+P3A), label = Player, color=P2M+P3M)) +
  geom_point() +
  geom_text(vjust = -0.5, hjust = 0.35, size=3) + # Adjust position of team labels
  labs(x = "Assists per game", y = "Field goal percentage") + # Axis labels
  ggtitle("WNBA Playoffs 2022 Player Stats",subtitle="players with at least 20 min per game, via wehoop") + # Plot title
  theme_minimal() # Minimal theme
  
 
 
 
#field goal percentage vs ppg, at least 20 mins per game
ggplot(subset, aes(x = PTS/GP, y = (P2M+P3M)/(P2A+P3A), label = Player, color=GP)) +
  geom_point() +
  geom_text(vjust = -0.5, hjust = 0.55, size=3) + # Adjust position of team labels
  labs(x = "Points per game", y = "Field goal percentage") + # Axis labels
  ggtitle("WNBA Playoffs 2022 Player Stats",subtitle="players with at least 20 min per game, via wehoop") + # Plot title
  theme_minimal() # Minimal theme
  

subset$TS <- subset$PTS / (2 * (subset$P2A + subset$P3A + (0.44 * subset$FTA))) #calculate true shooting percentage
  
#field goal percentage vs true shooting percentage
ggplot(subset, aes(x = (P2M+P3M)/(P2A+P3A), y = TS, label = Player, color=GP)) +
  geom_point() +
  geom_text(vjust = -0.5, hjust = 0.5, size=3) + # Adjust position of team labels
  labs(x = "Field goal percentage", y = "True Shooting percentage") + # Axis labels
  ggtitle("WNBA Playoffs 2022 Player Stats",subtitle="players with at least 20 min per game, via wehoop") + # Plot title
  theme_minimal() # Minimal theme
  
#playoffs raw 2pm vs 3pm
ggplot(sub, aes(x = P2M, y = P3M, label = Player, color=P2M+P3M)) +
  geom_point() +
  geom_text(vjust = -0.3, hjust = 0.5, size=6) + # Adjust position of team labels
  labs(x = "2pm", y = "3pm") + # Axis labels
  ggtitle("WNBA Playoffs 2022 Player Shooting Stats",subtitle="players with at least 10 FGM, via wehoop") + # Plot title
  theme_minimal() # Minimal theme




  
  
  
#field goal percentage vs TS% for last 10 years playoffs, at least 50 FGA
#wnba player full box score
tictoc::tic()
progressr::with_progress({
  playoffs <- wehoop::load_wnba_player_box(season=c(2014:2023))
})
tictoc::toc()

player_playoffs2 <- subset(playoffs, season_type==3)

Pbox_playoffs2 <- player_playoffs2 %>%
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
  as.data.frame()
  
#only players who averaged at least 50 FGA
subset2<- subset(Pbox_playoffs2, P2A+P3A>=50)

subset2$TS <- subset2$PTS / (2 * (subset2$P2A + subset2$P3A + (0.44 * subset2$FTA))) #calculate true shooting percentage
  
#field goal percentage vs true shooting percentage
ggplot(subset2, aes(x = (P2M+P3M)/(P2A+P3A), y = TS, label = paste(Player, Season), color=P2M+P3M)) +
  geom_point() +
  geom_text(vjust = -0.5, hjust = 0.35, size=3) + # Adjust position of team labels
  labs(x = "Field goal percentage", y = "True Shooting percentage") + # Axis labels
  ggtitle("WNBA Playoffs 2014-2023 Player Stats", subtitle="players with at least 50 FGA, via wehoop") + # Plot title
  theme_minimal() # Minimal theme'
  
#field goal percentage vs true shooting percentage
ggplot(subset2, aes(x = AST/GP, y = TS, label = paste(Player, Season), color=P2M+P3M)) +
  geom_point() +
  geom_text(vjust = -0.5, hjust = 0.75, size=3) + # Adjust position of team labels
  labs(x = "Assists per game", y = "True Shooting percentage") + # Axis labels
  ggtitle("WNBA Playoffs 2014-2023 Player Stats", subtitle="players with at least 50 FGA, via wehoop") + # Plot title
  theme_minimal() # Minimal theme'
  
  
########################################################################################################
#2023 playoffs
#wnba player full box score
tictoc::tic()
progressr::with_progress({
  box2023 <- wehoop::load_wnba_player_box(season=2023)
})
tictoc::toc()

box2023 <- subset(box2023, season_type==3)

box2023 <- box2023 %>%
  group_by(team_location, athlete_display_name) %>%
  summarise(GP=sum(minutes>0, na.rm=TRUE), MIN=sum(minutes, na.rm = TRUE), PTS=sum(points, na.rm = TRUE),
    P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), P2p=100*P2M/P2A,
    P3M=sum(three_point_field_goals_made, na.rm = TRUE), P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), P3p=100*P3M/P3A,
    FTM=sum(free_throws_made, na.rm = TRUE), FTA=sum(free_throws_attempted, na.rm = TRUE), FTp=100*FTM/FTA,
    OREB=sum(offensive_rebounds, na.rm = TRUE), DREB=sum(defensive_rebounds, na.rm = TRUE), AST=sum(assists, na.rm = TRUE),
    TOV=sum(turnovers, na.rm = TRUE), STL=sum(steals, na.rm = TRUE), BLK=sum(blocks, na.rm = TRUE),
    PF=sum(fouls, na.rm = TRUE)) %>%
  rename(Team=team_location,
         Player=athlete_display_name) %>%
  as.data.frame()

box2023$TS <- box2023$PTS / (2 * (box2023$P2A + box2023$P3A + (0.44 * box2023$FTA))) #calculate true shooting percentage

fga50 <-subset(box2023, P2A+P3A>=50)
fga50

fga50 <- fga50%>%
	arrange(desc(TS))%>%
	select(Team, Player, GP, MIN, PTS, TS)