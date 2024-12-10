pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR)

tictoc::tic()
progressr::with_progress({
  wbb_team_box <- wehoop::load_wbb_team_box()
})
tictoc::toc()

options(max.print = 1000000)
#######################################################################################################
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
  rename(Season=season, Team=team_location) %>%
  as.data.frame()
  

Obox <- wbb_team_box %>%
  group_by(season, opponent_team_location) %>%
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
  rename(Season=season, Team=opponent_team_location) %>%
  as.data.frame()


ggplot(wbb_team_box, aes(x=field_goals_attempted, y=field_goals_made, shape=team_winner, color=team_winner)) +
  geom_point(size=2, alpha=0.5)+
  xlim(25,100) +
  ylim(0,60) +
  theme_minimal()

#how many teams had a game with 12 or fewer FGM
subset(wbb_team_box, field_goals_made <=12) %>% select("game_date", "team_location", "opponent_team_location", "field_goals_made")

#lowest FGM for team_winner=TRUE
winners <- wbb_team_box %>%
  filter(team_winner==TRUE) %>%
  arrange(field_goals_made) %>%
  select("game_date", "team_location", "opponent_team_location", "field_goals_made")

winners

#highest FGM for team_winner=FALSE
losers <- wbb_team_box %>%
  filter(team_winner==FALSE) %>%
  arrange(desc(field_goals_made)) %>%
  select("game_date", "team_location", "opponent_team_location", "field_goals_made", "field_goals_attempted")

losers

#combine both teams' box score
combined_box <- wbb_team_box %>%
  group_by(game_id) %>%
  mutate(total_FGM = sum(field_goals_made), total_FGA = sum(field_goals_attempted)) %>%
  slice(1) %>%
  select(game_date, team1 = team_location, team2 = opponent_team_location, total_FGM, total_FGA) %>%
  ungroup() %>%
  arrange((total_FGM))

combined_box

#find games where loser actually made more field goals than the winner
winners_less_fg <- wbb_team_box %>%
  inner_join(wbb_team_box, by = c("game_id" = "game_id", "opponent_team_id" = "team_id")) %>%
  filter(team_winner.x == 1 & field_goals_made.x < field_goals_made.y) %>%
  select(game_date = game_date.x,
         winning_team_location = team_location.x,
         #winning_team_score = team_score.x, 
         winning_team_fg = field_goals_made.x, 
         losing_team_location = team_location.y, 
         #losing_team_score = team_score.y,
         losing_team_fg = field_goals_made.y)

winners_less_fg

#calculate largest gap in fgs when the team with more field goals lost
winners_less_fg <- winners_less_fg %>%
  mutate(fg_diff=losing_team_fg-winning_team_fg) %>%
  arrange(desc(fg_diff))

149/length(unique(wbb_team_box$game_id))
