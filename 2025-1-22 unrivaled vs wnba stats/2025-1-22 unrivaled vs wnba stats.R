pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, tidyr)

#load data
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season=c(2024))
})
tictoc::toc()

Tbox <- wnba_team_box %>%
  group_by(team_name) %>%
  filter(team_name !="Team USA" & team_name !="Team WNBA") %>%
  filter(season_type==2) %>%
  summarise(GP=n(), 
            PTS=sum(team_score, na.rm = TRUE),
            W=sum(team_winner==TRUE), 
            L=sum(team_winner==FALSE), 
            X2P=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), 
            X2PA=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), 
            X3P=sum(three_point_field_goals_made, na.rm = TRUE), 
            X3PA=sum(three_point_field_goals_attempted, na.rm = TRUE), 
            FGM=X2P+X3P,
            FGA=X3PA+X2PA,
            FTM=sum(free_throws_made, na.rm = TRUE), 
            FTA=sum(free_throws_attempted, na.rm = TRUE), 
            OREB=sum(offensive_rebounds, na.rm = TRUE), 
            DREB=sum(defensive_rebounds, na.rm = TRUE), 
            AST=sum(assists, na.rm = TRUE), 
            TOV=sum(turnovers, na.rm = TRUE), 
            STL=sum(steals, na.rm = TRUE), 
            BLK=sum(blocks, na.rm = TRUE), 
            PF=sum(fouls, na.rm = TRUE), 
            PM=sum(team_score-opponent_team_score, na.rm=TRUE)) %>%
  rename(Team=team_name) %>%
  as.data.frame()

unrivaled <- read.csv("~/Desktop/unrivaled_wnba.csv", header = TRUE)


#league-wide per-game averages
unrivaled_league_avg <- unrivaled %>%
  summarise(across(c(FGM, FGA, X3P, X3PA, X2P, X2PA, FTM, FTA, OREB, DREB, AST, STL, BLK, TOV, PF, PTS), 
                   ~ sum(.) / sum(G))) %>%
  mutate(across(everything(), round, 2))%>%
    mutate(League = "Unrivaled")

wnba_league_avg <- Tbox %>%
  summarise(across(c(FGM, FGA, X3P, X3PA, X2P, X2PA, FTM, FTA, OREB, DREB, AST, STL, BLK, TOV, PF, PTS), 
                   ~ sum(.) / sum(GP))) %>%
  mutate(across(everything(), round, 2))%>%
    mutate(League = "WNBA")

#combine per game avg stats for both leagues
combined_df <- bind_rows(unrivaled_league_avg, wnba_league_avg)

colnames(combined_df) <- gsub("X", "", colnames(combined_df))

long_df <- combined_df %>%
  pivot_longer(-League, names_to = "Stat", values_to = "Value") %>%
  pivot_wider(names_from = League, values_from = Value)

#scatter plot
ggplot(long_df, aes(x = WNBA, y = Unrivaled)) +
  geom_point(color = "brown", size = 2.5) +
  geom_text(aes(label = Stat), 
            position = position_jitter(width = 0.3, height = 0.3, seed = 123),
            hjust = .2, vjust = -0.2, size = 4) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  annotate("text", x = max(long_df$WNBA) * 0.8, y = max(long_df$Unrivaled) * 0.8, 
           label = "line of equality", color = "red", angle = 40, hjust = -0.1, vjust=-1) +
  scale_x_continuous(breaks = seq(0, max(long_df$WNBA), by = 5)) +
  scale_y_continuous(breaks = seq(0, max(long_df$Unrivaled), by = 5)) +
  labs(x = "WNBA Stats", y = "Unrivaled Stats", 
       title = "WNBA vs Unrivaled Team Per-Game Stat Averages", 
       subtitle = "WNBA 2024 season + Unrivaled first 6 games") +
  theme_minimal()

