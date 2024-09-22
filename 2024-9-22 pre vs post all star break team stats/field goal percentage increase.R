#Use wehoop package to download WNBA data

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, ggplot2, tidyverse, ggrepel, patchwork)

#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box()
})

tictoc::toc()

#############################################################################################################
#calculate team overall box scores before/after all star break (will just set the cutoff as 8/1)

pre_AS <- wnba_team_box %>%
  filter(game_date < as.Date("2024-08-01"))%>%
  filter(team_location !="Team WNBA" & team_location != "Team USA") %>%
  group_by(team_location) %>%
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
  rename(Team=team_location) %>%
  as.data.frame()
  
post_AS <- wnba_team_box %>%
  filter(game_date > as.Date("2024-08-01"))%>%
  group_by(team_location) %>%
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
  rename(Team=team_location) %>%
  as.data.frame()

#per game stats
pre_AS_pg <- pre_AS %>%
  mutate(across(c(PTS, P2M, P2A, P3M, P3A, FTM, FTA, OREB, DREB, AST, TOV, STL, BLK, PF, PM), ~ . / GP),
         WinPct = W / (W + L) * 100,
         FGP = (P2M + P3M) / (P2A + P3A) * 100)

post_AS_pg <- post_AS %>%
  mutate(across(c(PTS, P2M, P2A, P3M, P3A, FTM, FTA, OREB, DREB, AST, TOV, STL, BLK, PF, PM), ~ . / GP),
         WinPct = W / (W + L) * 100,
         FGP = (P2M + P3M) / (P2A + P3A) * 100)

#join data
scatter_data <- inner_join(
  pre_AS_pg %>% select(Team, PTS, WinPct = W / (W + L)),
  post_AS_pg %>% select(Team, PTS, WinPct = W / (W + L)),
  by = "Team",
  suffix = c("_pre", "_post")
)

#############################################################################################################
# Create scatter plot for Points per Game
a <- ggplot(scatter_data, aes(x = PTS_pre, y = PTS_post)) +
  geom_point(color = "blue", size = 3) +
  geom_text_repel(aes(label = Team), box.padding = 0.5, size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "WNBA Teams: Points per Game",
       subtitle = "Pre vs Post All-Star Break",
       x = "Pre All-Star PPG",
       y = "Post All-Star PPG") +
  theme_minimal() +
  coord_fixed()

# Create scatter plot for Win Percentage
b <- ggplot(scatter_data, aes(x = WinPct_pre, y = WinPct_post)) +
  geom_point(color = "green", size = 3) +
  geom_text_repel(aes(label = Team), box.padding = 0.5, size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "WNBA Teams: Win Percentage",
       subtitle = "Pre vs Post All-Star Break",
       x = "Pre All-Star Win %",
       y = "Post All-Star Win %") +
  theme_minimal() +
  coord_fixed()
  
  # Create scatter plot for Field Goal Percentage
c <- ggplot(scatter_data, aes(x = FGP_pre, y = FGP_post)) +
  geom_point(color = "purple", size = 3) +
  geom_text_repel(aes(label = Team), box.padding = 0.5, size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "WNBA Teams: Field Goal Percentage",
       subtitle = "Pre vs Post All-Star Break",
       x = "Pre All-Star FG%",
       y = "Post All-Star FG%") +
  theme_minimal() +
  coord_fixed()
  
a+b+c

#############################################################################################################
#alternate code for scatter plots with more specific sizing
# Function to create consistent axis limits
get_axis_limits <- function(data, x_col, y_col) {
  min_val <- min(min(data[[x_col]]), min(data[[y_col]]))
  max_val <- max(max(data[[x_col]]), max(data[[y_col]]))
  return(c(min_val, max_val))
}

# Common theme
common_theme <- theme_minimal() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

# Points per Game plot
pts_limits <- get_axis_limits(scatter_data, "PTS_pre", "PTS_post")
a <- ggplot(scatter_data, aes(x = PTS_pre, y = PTS_post)) +
  geom_point(color = "blue", size = 3) +
  geom_text_repel(aes(label = Team), box.padding = 0.5, size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "WNBA Teams: Points per Game",
       subtitle = "Pre vs Post All-Star Break 2024",
       x = "Pre All-Star PPG",
       y = "Post All-Star PPG",
       caption="source: stats.wnba.com | graphic: @wnbadata") +
  scale_x_continuous(limits = pts_limits) +
  scale_y_continuous(limits = pts_limits) +
  coord_fixed(ratio = 1) +
  common_theme

# Win Percentage plot
win_limits <- get_axis_limits(scatter_data, "WinPct_pre", "WinPct_post")
b <- ggplot(scatter_data, aes(x = WinPct_pre, y = WinPct_post)) +
  geom_point(color = "green", size = 3) +
  geom_text_repel(aes(label = Team), box.padding = 0.5, size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "WNBA Teams: Win Percentage",
       subtitle = "Pre vs Post All-Star Break 2024",
       x = "Pre All-Star Win %",
       y = "Post All-Star Win %",
       caption="source: stats.wnba.com | graphic: @wnbadata") +
  scale_x_continuous(limits = win_limits) +
  scale_y_continuous(limits = win_limits) +
  coord_fixed(ratio = 1) +
  common_theme

# Field Goal Percentage plot
fgp_limits <- get_axis_limits(scatter_data, "FGP_pre", "FGP_post")
c <- ggplot(scatter_data, aes(x = FGP_pre, y = FGP_post)) +
  geom_point(color = "purple", size = 3) +
  geom_text_repel(aes(label = Team), box.padding = 0.5, size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "WNBA Teams: Field Goal Percentage",
       subtitle = "Pre vs Post All-Star Break 2024",
       x = "Pre All-Star FG%",
       y = "Post All-Star FG%",
       caption="source: stats.wnba.com | graphic: @wnbadata") +
  scale_x_continuous(limits = fgp_limits) +
  scale_y_continuous(limits = fgp_limits) +
  coord_fixed(ratio = 1) +
  common_theme

# Save plots with consistent size
ggsave("~/Desktop/wehoop/2024-9-22 pre vs post all star break team stats/points_per_game.png", plot = a, width = 5, height = 5, dpi = 300)
ggsave("~/Desktop/wehoop/2024-9-22 pre vs post all star break team stats/win_percentage.png", plot = b, width = 5, height = 5, dpi = 300)
ggsave("~/Desktop/wehoop/2024-9-22 pre vs post all star break team stats/field_goal_percentage.png", plot = c, width = 5, height = 5, dpi = 300)
