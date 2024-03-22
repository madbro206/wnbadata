#how to get the data

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

library(BasketballAnalyzeR)
library(ggplot2)

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
      TRUE ~ 40  # Set default value to 40 if none of the cases are satisfied
    )
  )

#add these minute calculations to the wbb_team_box dataframe
wbb_team_box <- wbb_team_box %>%
  left_join(wbb_pbp %>% select(game_id, game_length), by = "game_id") %>%
  distinct(game_id, team_location, .keep_all = TRUE) 
  
wbb_team_box <- wbb_team_box %>%
  mutate(
    game_length = case_when(
      is.na(game_length) ~ 40,  # Set to 40 if game_length is NA
      TRUE ~ game_length  # Keep the original value if not NA
    )
  )


  
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

#filter out d2 teams  
Tbox <- Tbox %>%
	filter(GP>5)
  

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


#filter out d2 teams
Obox <- Obox %>%
	filter(GP>5)

#four factors (the first is pace)
ff <- fourfactors(Obox, Tbox)

#pace on a per 40 minutes scale
ff$PACE.Off <-(ff$PACE.Off *40)
ff$PACE.Def <-(ff$PACE.Def *40)

listPlots <- plot(ff)

#plot pace only
library(gridExtra)
grid.arrange(grobs=listPlots[1:1], ncol=1)


#wbit round 1 georgetown vs washington
tm <- c("Washington","Georgetown")
selTeams <- which(Tbox$Team %in% tm)
FF.sel <- fourfactors(Tbox[selTeams,], Obox[selTeams,])
FF.sel$PACE.Off <-(FF.sel$PACE.Off *40)
FF.sel$PACE.Def <-(FF.sel$PACE.Def *40)

plots<- plot(FF.sel)
grid.arrange(grobs=plots[1:1], ncol=1)


#calculate pace myself:
attach(Tbox)
Tbox$poss <- P2A+P3A+(0.44*FTA)-OREB+TOV
Tbox$pace <- 40*(Tbox$poss/MIN)
detach(Tbox)

team_pace = Tbox[c("Season", "Team", "GP", "poss", "pace")]

attach(Obox)
Obox $poss <- P2A+P3A+(0.44*FTA)-OREB+TOV
Obox $pace <- 40*(Obox$poss/MIN)
detach(Obox)

opp_pace = Obox[c("Season", "Team", "GP", "poss", "pace")]









#plot pace

#merge team_pace and opp_pace based on Team names
merged_data <- merge(team_pace, opp_pace, by = "Team", suffixes = c("_team", "_opp"))

#filter only Georgetown and Washington
selected_teams <- merged_data$Team %in% c("Georgetown", "Washington")
selected_teams_df <- merged_data[selected_teams,]

ggplot(merged_data, aes(x = pace_team, y = pace_opp)) +
  geom_point(color = "blue") +
  geom_text(data = selected_teams_df, aes(label = Team), color="red", hjust = -0.2, vjust = 0.5, size = 7) +
  geom_point(data = selected_teams_df, aes(x = pace_team, y = pace_opp), color = "red", size = 3) +
  labs(title = "Team Pace vs Opponent Pace (per 40 min) NCAAW 2024",
       x = "Team Pace",
       y = "Opponent Pace") +
  theme_minimal()








#scorigami plot
#interactive and showing frequency
library(plotly)
#find scores for each game, larger score first
scores <- wbb_team_box %>%
  distinct(game_id, .keep_all = TRUE) %>%#record only 1 score for each game
  mutate(winning_score = pmax(team_score, opponent_team_score), losing_score = pmin(team_score, opponent_team_score)) %>%
  select(winning_score, losing_score, team_location, opponent_team_location, game_date) 
  
#Arrange the data by game_date within each group of winning_score and losing_score
scores1 <- scores %>%
  group_by(winning_score, losing_score) %>%
  arrange(desc(game_date)) %>%
  #Keep only the first row (most recent) within each group
  slice(1) %>%
  ungroup()
  
frequency <- scores %>%
  group_by(losing_score, winning_score) %>%
  summarize(frequency = n())
  
#add to show latest instance of each score
combined_data <- scores1 %>%
	left_join(frequency, by=c("losing_score"="losing_score", "winning_score"="winning_score"))


p <- ggplot(combined_data, aes(x = losing_score, y = winning_score, fill = frequency, text = paste("Score:", winning_score, "-", losing_score, "Frequency: ", frequency, "Most recent:", game_date, team_location, "vs", opponent_team_location))) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "Losing Score", y = "Winning Score", fill = "Frequency") +
  ggtitle("NCAA Scorigami 2023-24")+
  coord_equal()

ggplotly(p, tooltip = "text")




#points per game and allowed per game
#points per game (PPG) for each team and opponents
Tbox$PPG_team <- Tbox$PTS / Tbox$GP
Obox$PPG_opp <- Obox$PTS / Obox$GP

#merge Tbox and Obox based on team names
merged_data <- merge(Tbox, Obox, by = "Team", suffixes = c("_team", "_opp"))

#scatterplot
ggplot(merged_data, aes(x = PPG_team, y = PPG_opp)) +
  geom_point() +
  geom_text(data = subset(merged_data, Team %in% c("Georgetown", "Washington")), color="red",aes(label = Team), hjust = -0.2, vjust = 0.5, size = 7) +
    geom_point(data = subset(merged_data, Team %in% c("Georgetown", "Washington")), color = "red", size = 3) +
  labs(title = "Team Points per Game vs Opponent Points per Game NCAAW 2024",
       x = "Team Points per Game",
       y = "Opponent Points per Game") +
  theme_minimal()