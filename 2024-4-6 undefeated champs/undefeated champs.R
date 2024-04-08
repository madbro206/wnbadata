#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR)

library(ggplot2)


tictoc::tic()
progressr::with_progress({
  wbb_team_box16 <- wehoop::load_wbb_team_box(season=2016)
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_team_box <- wehoop::load_wbb_team_box()
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_player_box16 <- wehoop::load_wbb_player_box(season=2016)
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box()
})
tictoc::toc()

library(BasketballAnalyzeR)

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
uconn16 <- subset(wbb_team_box16, team_location=="UConn")
uconn16_p <- subset(wbb_player_box16, team_location=="UConn")

mean(uconn16$team_score-uconn16$opponent_team_score)

uconn16_players <- uconn16_p %>%
  group_by(athlete_display_name) %>%
  summarise(GP=sum(minutes>0, na.rm=TRUE), MIN=sum(minutes, na.rm = TRUE), PTS=sum(points, na.rm = TRUE),
    P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), P2p=100*P2M/P2A,
    P3M=sum(three_point_field_goals_made, na.rm = TRUE), P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), P3p=100*P3M/P3A,
    FTM=sum(free_throws_made, na.rm = TRUE), FTA=sum(free_throws_attempted, na.rm = TRUE), FTp=100*FTM/FTA,
    OREB=sum(offensive_rebounds, na.rm = TRUE), DREB=sum(defensive_rebounds, na.rm = TRUE), AST=sum(assists, na.rm = TRUE),
    TOV=sum(turnovers, na.rm = TRUE), STL=sum(steals, na.rm = TRUE), BLK=sum(blocks, na.rm = TRUE),
    PF=sum(fouls, na.rm = TRUE)) %>%
  rename(Player=athlete_display_name) 
  as.data.frame()

uconn16_players <- uconn16_players %>%
  arrange(desc(PTS))

# Create the bar chart
ggplot(uconn16_players, aes(x = reorder(Player, desc(PTS)), y = PTS)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Points Scored by UConn Players 2015-2016",
       x = "Player",
       y = "Points") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability




sc24 <- subset(wbb_team_box, team_location=="South Carolina")
sc24_p <- subset(wbb_player_box, team_location=="South Carolina")
mean(sc24$team_score-sc24$opponent_team_score)

sc24_players <- sc24_p %>%
  group_by(athlete_display_name) %>%
  summarise(GP=sum(minutes>0, na.rm=TRUE), MIN=sum(minutes, na.rm = TRUE), PTS=sum(points, na.rm = TRUE),
    P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), P2p=100*P2M/P2A,
    P3M=sum(three_point_field_goals_made, na.rm = TRUE), P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), P3p=100*P3M/P3A,
    FTM=sum(free_throws_made, na.rm = TRUE), FTA=sum(free_throws_attempted, na.rm = TRUE), FTp=100*FTM/FTA,
    OREB=sum(offensive_rebounds, na.rm = TRUE), DREB=sum(defensive_rebounds, na.rm = TRUE), AST=sum(assists, na.rm = TRUE),
    TOV=sum(turnovers, na.rm = TRUE), STL=sum(steals, na.rm = TRUE), BLK=sum(blocks, na.rm = TRUE),
    PF=sum(fouls, na.rm = TRUE)) %>%
  rename(Player=athlete_display_name)  
  as.data.frame()


sc24_players <- sc24_players %>%
  arrange(desc(PTS))

# Create the bar chart
ggplot(uconn16_players, aes(x = reorder(Player, desc(PTS)), y = PTS)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Points Scored by UConn Players 2015-2016",
       x = "Player",
       y = "Points") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability



# Create the bar chart
ggplot(sc24_players, aes(x = reorder(Player, desc(PTS)), y = PTS)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Points Scored by South Carolina Players 2023-24",
       x = "Player",
       y = "Points") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability




#combined bar chart:
# Assuming `uconn16_players` and `sc24_players` are your dataframes

# Load necessary library
library(ggplot2)

# Add a column to each dataframe to indicate the group
uconn16_players$Group <- "UConn 2015-2016"
sc24_players$Group <- "South Carolina 2023-24"

# Combine the dataframes
combined_data <- rbind(uconn16_players, sc24_players)

# Create the combined bar chart
ggplot(combined_data, aes(x = reorder(Player, desc(PTS)), y = PTS, fill = Group)) +
  geom_bar(stat = "identity", position = "identity") +
  labs(title = "Total Points Scored by Players in One Season", subtitle="South Carolina total 3160 points (37 games), UConn total 3346 points (38 games)",
       x = "Player",
       y = "Points",
       fill = "Team") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
