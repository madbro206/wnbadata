#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, ggplot2)

#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box() #wnba since 2010
})
tictoc::toc()

#wbb box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box()
})
tictoc::toc()

wnba_player_box <- subset(wnba_player_box, season_type==2)
wnba_team_box <- subset(wnba_team_box, season_type==2)

Tbox <- wnba_team_box %>%
  group_by(season, team_short_display_name) %>%
  summarise(OREB=sum(offensive_rebounds, na.rm = TRUE),
  FGM=sum(field_goals_made, na.rm=TRUE),
  FGA=sum(field_goals_attempted,na.rm=TRUE),
  FGp=100*FGM/FGA, 
  misses=FGA-FGM)%>%
  filter(FGA>150)%>%
  as.data.frame()
Tbox

Obox <- wnba_team_box %>%
  group_by(season, opponent_team_short_display_name) %>%
  summarise(DREB=sum(defensive_rebounds, na.rm = TRUE)) %>%
  filter(DREB>200)%>%
  as.data.frame()
Obox

data <- merge(Tbox, Obox, by.x="team_short_display_name", by.y="opponent_team_short_display_name")

data$OREBp <- data$OREB/(data$OREB+data$DREB)

cor(Tbox$FGp, Tbox$OREB)
cor(Tbox$misses, Tbox$OREB)
cor(data$FGp, data$OREBp)

##############################################################################################################
#FGp vs OREB

#scatter plot
ggplot(Tbox, aes(x = FGp, y = OREB, label = team_short_display_name)) +
  geom_point() +
  geom_text(vjust = -0.5, hjust = 0.5) + # Adjust position of team labels
  labs(x = "Team FG Percentage", y = "Team Offensive Rebounds", subtitle="WNBA 2023 Regular Season, data via wehoop R package") + # Axis labels
  ggtitle("FG Percentage vs. Offensive Rebounds") + # Plot title
  theme_minimal() # Minimal theme

#FG missed vs OREB
#scatter plot
ggplot(Tbox, aes(x = misses, y = OREB, label = team_short_display_name)) +
  geom_point() +
  geom_text(vjust = -0.5, hjust = 0.5) + # Adjust position of team labels
  labs(x = "Team FG Missed", y = "Team Offensive Rebounds", subtitle="WNBA 2023 Regular Season, data via wehoop R package") + # Axis labels
  ggtitle("FG Missed vs. Offensive Rebounds") + # Plot title
  theme_minimal() # Minimal theme
  
#FGp vs OREBp
ggplot(data, aes(x = FGp, y = OREBp, label = team_short_display_name)) +
  geom_point() +
  geom_text(vjust = -0.5, hjust = 0.5) + # Adjust position of team labels
  labs(x = "Team FG Percentage", y = "Team Offensive Rebound Percentage", subtitle="WNBA 2023 Regular Season, data via wehoop R package") + # Axis labels
  ggtitle("FG Percentage vs. Offensive Rebound Percentage") + # Plot title
  theme_minimal() # Minimal theme



##############################################################################################################
#bonus
#FG missed vs OREBp
ggplot(data, aes(x = misses, y = OREBp, label = team_short_display_name)) +
  geom_point() +
  geom_text(vjust = -0.5, hjust = 0.5) + # Adjust position of team labels
  labs(x = "Team FG Missed", y = "Team Offensive Rebound Percentage") + # Axis labels
  ggtitle("FG Percentage vs. Offensive Rebounds") + # Plot title
  theme_minimal() # Minimal theme