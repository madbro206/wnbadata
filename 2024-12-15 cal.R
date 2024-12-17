#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR)

#load data
#equivalent in NCAAW:
tictoc::tic()
progressr::with_progress({
  wbb_team_box <- wehoop::load_wbb_team_box()
})
tictoc::toc()

Tbox <- wbb_team_box %>%
  group_by(season, team_location) %>%
  summarise(GP=n(), 
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
  filter(GP >=5) %>%
  rename(Season=season, Team=team_location) %>%
  as.data.frame()

wbb_team_box <- wbb_team_box %>%
  mutate(highlight = ifelse(team_location == "California", "California", "Other")) %>%
  arrange(highlight)

ggplot(wbb_team_box, aes(x = fouls, y = turnovers, color = highlight)) +
  geom_point(data = subset(wbb_team_box, highlight == "Other"), alpha = 0.3) +
  geom_point(data = subset(wbb_team_box, highlight == "California"), alpha = 1) +
  scale_color_manual(values = c("California" = "red", "Other" = "black")) +
  labs(title = "Team Games PF vs TOV",
       x = "Team personal fouls",
       y = "Team turnovers") +
  theme_minimal()


ggplot(wbb_team_box, aes(x = three_point_field_goals_made, y = three_point_field_goals_attempted, color = highlight)) +
  geom_point(data = subset(wbb_team_box, highlight == "Other"), alpha = 0.3) +
  geom_point(data = subset(wbb_team_box, highlight == "California"), alpha = 1) +
  scale_color_manual(values = c("California" = "red", "Other" = "black")) +
  labs(title = "Team Games Threes",
       x = "3PM",
       y = "3PA") +
  theme_minimal()


Tbox <- Tbox %>%
  mutate(highlight = ifelse(Team == "California", "California", "Other")) %>%
  arrange(highlight)

ggplot(Tbox, aes(x = P3M, y = P3A, color = highlight)) +
  geom_point(data = subset(Tbox, highlight == "Other"), alpha = 0.3) +
  geom_point(data = subset(Tbox, highlight == "California"), alpha = 1) +
  scale_color_manual(values = c("California" = "blue", "Other" = "black")) +
  labs(title = "Team Threes",
       x = "3PM",
       y = "3PA") +
  theme_minimal()
