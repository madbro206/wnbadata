#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, ggplot2)

#load data
#equivalent in NCAAW:
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


#######################################################################################################
#translate into player, team, and opponent box score

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
  as.data.frame()

# Calculate total FG%
Pbox$FGM <- Pbox$P2M + Pbox$P3M
Pbox$FGA <- Pbox$P2A + Pbox$P3A
Pbox$FGp <- 100 * Pbox$FGM / Pbox$FGA

#minimum requirements
min_games <- ceiling(0.6 * max(Pbox$GP))  # 60% of maximum games played
min_fg_attempts <- pmax(4 * Pbox$GP, 60)  # 4 fg attempts per game or 60 total
min_3p_makes <- pmax(1 * Pbox$GP, 10)    # 1 3pt make per game or 30 total
min_ft_attempts <- pmax(2 * Pbox$GP, 25) # 2 ft attempts per game or 25 total

# Top 10 players in 3-point percentage (P3p)
top_10_3p <- Pbox %>%
  filter(GP >= min_games, P3M >= min_3p_makes) %>%
  arrange(desc(P3p)) %>%
  select(Player, P3p, P3M, P3A, GP) %>%
  head(10)

# Top 10 players in total field goal percentage (FGp)
top_10_fg <- Pbox %>%
  filter(GP >= min_games, FGA >= min_fg_attempts) %>%
  arrange(desc(FGp)) %>%
  select(Player, FGp, FGM, FGA, GP) %>%
  head(10)

# Top 10 players in free throw percentage (FTp)
top_10_ft <- Pbox %>%
  filter(GP >= min_games, FTA >= min_ft_attempts) %>%
  arrange(desc(FTp)) %>%
  select(Player, FTp, FTM, FTA, GP) %>%
  head(10)


cat("\nTop 10 players in total field goal percentage (FGp):\n")
print(top_10_fg)

cat("Top 10 players in 3-point percentage (P3p):\n")
print(top_10_3p)

cat("\nTop 10 players in free throw percentage (FTp):\n")
print(top_10_ft)
##############################################################################################################
#league total percentages
combined_P3M <- sum(Pbox$P3M, na.rm = TRUE)
combined_P3A <- sum(Pbox$P3A, na.rm = TRUE)
combined_P3p <- 100 * combined_P3M / combined_P3A

combined_FGM <- sum(Pbox$P2M, na.rm = TRUE) + sum(Pbox$P3M, na.rm = TRUE)
combined_FGA <- sum(Pbox$P2A, na.rm = TRUE) + sum(Pbox$P3A, na.rm = TRUE)
combined_FGp <- 100 * combined_FGM / combined_FGA

cat("\nTotal D1 field goal percentage:", round(combined_FGp, 2), "%\n")

cat("\nTotal D1 3-point percentage:", round(combined_P3p, 2), "%\n")

combined_FTM <- sum(Pbox$FTM, na.rm = TRUE)
combined_FTA <- sum(Pbox$FTA, na.rm = TRUE)
combined_FTp <- 100 * combined_FTM / combined_FTA

cat("\nTotal D1 free throw percentage:", round(combined_FTp, 2), "%\n")
########################### players hitting every minumum ###########################
qualified_all <- Pbox %>%
  filter(GP >= min_games,
         FGA >= min_fg_attempts,
         P3M >= min_3p_makes,
         FTA >= min_ft_attempts) %>%
  select(Player, Team, FGp, FGA, P3p, P3M, FTp, FTA) %>%
  mutate(
    FGp = round(FGp, 2),
    P3p = round(P3p, 2),
    FTp = round(FTp, 2)
  ) %>%
  arrange(desc(FGp))

print(qualified_all)
################################## visualization #####################################
library(tidyverse)
library(ggtext)
library(lemon)
library(extrafont)

# Filter players meeting the minimum requirements
qualified_players <- Pbox %>%
  filter(GP >= min_games,
         FGA >= min_fg_attempts,
         P3M >= min_3p_makes,
         FTA >= min_ft_attempts)

# Prepare data for plotting
shot_stats <- qualified_players %>%
  select(Player, Team, FGp, P3p, FTp) %>%
  gather("type", "percentage", FGp, P3p, FTp)

# Set factor levels for proper ordering
shot_stats$type <- factor(shot_stats$type, levels = c("FGp", "P3p", "FTp"))

# Create the plot
ggplot(shot_stats, aes(x = type, y = percentage, group = Player)) +
geom_pointline(data = shot_stats, 
                 aes(x = type, y = percentage, group = Player),
                 stroke = 0, linesize = 0.5, size = 2, 
                 linejoin = "mitre", alpha = 0.3, color = "grey60") +
  geom_pointline(data = league_averages,
                 aes(x = type, y = percentage, group = Player),
                 stroke = 0, linesize = 1.5, size = 4,
                 linejoin = "mitre", color = "red") +
  # Add league average labels
  geom_text(data = league_averages,
            aes(x = type, y = percentage, 
                label = sprintf("%.1f%%", percentage)),
            color = "red",
            vjust = -1,
            size = 3) +
  # Add player name labels (only for the first point to avoid clutter)
  geom_text(data = shot_stats %>% filter(type == "FGp"),
            aes(x = type, y = percentage, label = Player),
            hjust = 1,
            size = 2.5,
            alpha = 0.7,
            check_overlap = TRUE) +
  scale_y_continuous(limits = c(0, 100), labels = scales::percent_format(scale = 1)) +
  scale_x_discrete(labels = c("Field Goals", "Three-pointers", "Free Throws")) +
  labs(
    title = "NCAA D1 Women's Basketball Shooting Percentages",
    subtitle = "Players meeting minimum attempt requirements",
    caption = "Source: Your Data Source | Graphic: Your Name"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 30, 20, 20),
    plot.background = element_rect(fill = "grey95", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_markdown(size = 16, lineheight = 1.1, color = "grey20"),
    plot.subtitle = element_markdown(size = 14, lineheight = 1.1, color = "grey40", margin = margin(0, 0, 20, 0)),
    plot.caption = element_markdown(size = 8, color = "grey60", margin = margin(20, 0, 0, 0))
  )

#ggsave("ncaa_womens_basketball_shooting_stats.png", width = 10, height = 8, dpi = 300)

Pbox %>% 
  filter(FGp==100) %>% 
  select(Player, Team, FGM, FGA, FGp) %>%
  arrange(desc(FGA)) %>%
  head(10)
