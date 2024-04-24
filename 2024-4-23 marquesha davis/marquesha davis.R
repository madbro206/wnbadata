#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR)


#wnba full play by play
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp()
})
tictoc::toc()


#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box()
})

tictoc::toc()


#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box()
})
tictoc::toc()


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
 
#marquesha davis 2024 stats
marquesha <- subset(Pbox, Player=="Marquesha Davis")
marquesha
 
#find all SEC player box scores for 2024 season
sec_teams <- c("South Carolina","LSU","Ole Miss","Alabama","Tennessee","Vanderbilt","Mississippi State","Auburn","Texas A&M","Arkansas","Florida","Kentucky","Georgia","Missouri")

sec <- subset(Pbox, Team %in% sec_teams)
############################################################################################################## 
#plot defensive statistics for SEC
#for SEC players over 850 minutes
sec_500 <- subset(sec, MIN>=850)

attach(sec_500)
X<- data.frame(ID=Player, Team, V1=DREB/MIN, V2=STL/MIN, V3=PTS/MIN, V4=MIN)
detach(sec_500)
labs<- c("Defensive Rebounds per minute", "Steals per minute","Total minutes played", "Points per minute")
bubbleplot(X, id="ID", x="V1", y="V2", col="V4", size="V3", labels=labs, title="SEC Per-Minute Player Stats 2024 (players min>=850)", text.legend=TRUE, text.size=3, scale=FALSE)
 
 
#plot defensive statistics for liberty
wPbox <- wnba_player_box %>%
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
  
lib<-subset(wPbox, Team=="New York")
attach(lib)
X<- data.frame(ID=Player, Team, V1=DREB/MIN, V2=STL/MIN, V3=PTS/MIN, V4=MIN)
detach(lib)
labs<- c("Defensive Rebounds per minute", "Steals per minute","Total minutes played", "Points per minute")
bubbleplot(X, id="ID", x="V1", y="V2", col="V4", size="V3", labels=labs, title="Liberty Per-Minute Player Stats 2023", text.legend=TRUE, text.size=3, scale=FALSE)
##############################################################################################################  
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

#make sure each game has a length 
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

##############################################################################################################
#load advanced wnba stats from 2023 (sports reference)

data <- read.csv("~/Desktop/wehoop/2024-4-23 marquesha davis/wnba_data_2023.csv")
guards<- subset(data, Pos=="G" & MP/G>= 15)

NYL_guards <- subset(guards, Team == "NYL")

guards$DWSperG <- guards$DWS/guards$G

library(ggplot2)

# Create a vector of colors, with NYL guards highlighted in blue and others in gray
guards$team <- ifelse(guards$Team == "NYL", "NYL", "other")

# Plotting
ggplot(guards, aes(x = DRtg., y = DWSperG, color = team, label = ifelse(team == "NYL", Player, ""))) +
  geom_point() +
    geom_text(vjust=-.5, size=3) +
  labs(x = "Defensive rating (lower is better)", y = "Defensive win shares per game (higher is better)",
       title = "Comparison of DRtg vs DWS for WNBA (Pure) Guards in 2023", subtitle="guards with at least 15 mpg, stats via Sports Reference") +
  theme_minimal()
