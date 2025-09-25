#Use wehoop package to download WNBA data
pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, ggplot2, knitr, tidyr, ggrepel)

tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(season=c(2006:2025))
})
tictoc::toc()

wnba_player_box$minutes <- as.numeric(wnba_player_box$minutes)
wnba_player_box$points <- as.numeric(wnba_player_box$points)

#by team game
starter_points_pct <- wnba_player_box %>%
  filter(!is.na(points) & season_type==2) %>%
  group_by(season, game_id, game_date, team_id, team_name, team_score) %>%
  summarise(
    starter_points = sum(points[starter == TRUE], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_starter_points = starter_points / team_score * 100,
    game_team = paste(team_name, format(game_date, "%m-%d-%y"))
  ) %>%
  arrange(desc(pct_starter_points)) %>%
  select(game_team, team_score, starter_points, pct_starter_points, team_name, game_date, season)

starter_points_pct <- starter_points_pct %>%
  filter(team_name !="TEAM COLLIER" & team_name != "TEAM CLARK") 

print(starter_points_pct, n=20)


teams_to_exclude <- c("Team Collier", "Team Clark", "Team USA", "Team Usa", "West", "East", "Team WNBA", "Team Wilson", "Team Stewart", "Team Parker", "Team Delle Donne")

#team/season avgs
team_avg_starter_pct <- starter_points_pct %>%
  filter(!team_name %in% teams_to_exclude) %>%
  group_by(team_name, season) %>%
  summarise(
    avg_pct_starter_points = mean(pct_starter_points, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_pct_starter_points))

unique(team_avg_starter_pct$team_name)

team_avg_starter_pct 

kable(team_avg_starter_pct %>%select(team_name, season, avg_pct_starter_points, playoff_wins, made_playoffs) %>%head(15), digits = 1, col.names = c("Team", "Season","Starter %", "Playoff Wins", "Made Playoffs?"))

###############################################################################
#visualization of starter percentage of points over time
starter_points_pct_ts <- starter_points_pct %>%
  group_by(team_name) %>%
  arrange(game_date, .by_group = TRUE) %>%
  mutate(game_number = row_number()) %>%
  ungroup()

ggplot(starter_points_pct_ts,
  aes(x = game_number, y = pct_starter_points)) +
geom_line(alpha = 0.4) +
geom_point(size = 0.8) +
geom_smooth(method = "loess", se = TRUE, span = 0.4) +
facet_wrap(~ team_name, scales = "free_y") +
labs(x = "Game number", y = "Starter points (%)",
  title = "Starter-points share over time") +
theme_minimal()

#all on the same scale
ggplot(starter_points_pct_ts,
  aes(game_number, pct_starter_points)) +
geom_line(alpha = 0.4) +
geom_point(size = 0.8) +
geom_smooth(method = "loess", se = TRUE, span = 0.4) +
facet_wrap(~ team_name, scales = "fixed") +
scale_y_continuous(limits = c(50, 90)) + #scale
labs(x = "Game number", y = "Starter points (%)",
  title = "Starter-points share over time") +
theme_minimal()

#semifinals teams only
ggplot(dplyr::filter(starter_points_pct_ts,
  team_name %in% c("Mercury", "Lynx", "Fever", "Aces"), season %in% c(2021:2025)),
aes(game_number, pct_starter_points)) +
geom_line(alpha = 0.4) +
geom_point(size = 1) +
geom_smooth(method = "loess", se = TRUE, span = 0.4) +
facet_wrap(~ team_name, scales = "fixed") +
scale_y_continuous(limits = c(50, 90)) +
labs(x = "Game number", y = "Starter points (%)",
title = "Starter-points share over time") +
theme_minimal()

###############################################################################
# Get playoff history data (whether team made playoffs, and how many playoff games they played as proxy for going far)
playoff_data <- load_wnba_team_box(season=c(2006:2025)) %>%
  filter(season_type == 3) %>%  # Playoff games only
  group_by(team_name, season) %>%
  summarise(
    playoff_games = n(),
    playoff_wins = sum(team_winner, na.rm = TRUE),  # Count wins
    made_playoffs = TRUE,
    .groups = "drop"
  ) %>%
  mutate(
    playoff_win_pct = playoff_wins / playoff_games,
    playoff_losses = playoff_games - playoff_wins
  )

#normalize playoff wins (to account for format changes)
#calculate the actual maximum wins achieved each season
season_max_wins <- playoff_data %>%
  group_by(season) %>%
  summarise(max_wins_that_season = max(playoff_wins, na.rm = TRUE))

# Join back and normalize
playoff_data <- playoff_data %>%
  left_join(season_max_wins, by = "season") %>%
  mutate(normalized_playoff_wins = playoff_wins / max_wins_that_season)

#join starter point pct data
team_avg_starter_pct<- team_avg_starter_pct %>%
  left_join(playoff_data, by = c("team_name", "season")) %>%
  mutate(made_playoffs = replace_na(made_playoffs, FALSE))

#change NA to 0 for teams that didn't have any playoff action
team_avg_starter_pct$playoff_games[is.na(team_avg_starter_pct$playoff_games)] <- 0
team_avg_starter_pct$playoff_wins[is.na(team_avg_starter_pct$playoff_wins)] <- 0
team_avg_starter_pct$playoff_win_pct[is.na(team_avg_starter_pct$playoff_win_pct)] <- 0
team_avg_starter_pct$playoff_losses[is.na(team_avg_starter_pct$playoff_losses)] <- 0
team_avg_starter_pct$normalized_playoff_wins[is.na(team_avg_starter_pct$normalized_playoff_wins)] <- 0

team_avg_starter_pct %>%filter(season==2019) %>%arrange(desc(avg_pct_starter_points))
kable(team_avg_starter_pct%>%filter(season==2019) %>%select(team_name, season, avg_pct_starter_points, playoff_wins, made_playoffs), digits = 1, col.names = c("Team", "Season","Starter %", "Playoff Wins", "Made Playoffs?"))
###################################################################
#correlation tests
cor(team_avg_starter_pct$avg_pct_starter_points, team_avg_starter_pct$made_playoffs)

cor.test(team_avg_starter_pct$avg_pct_starter_points, as.numeric(team_avg_starter_pct$made_playoffs))

cor.test(team_avg_starter_pct$avg_pct_starter_points, team_avg_starter_pct$playoff_games)

cor.test(team_avg_starter_pct$avg_pct_starter_points, team_avg_starter_pct$playoff_wins)

cor.test(team_avg_starter_pct$avg_pct_starter_points, team_avg_starter_pct$playoff_win_pct)

cor.test(team_avg_starter_pct$avg_pct_starter_points, team_avg_starter_pct$normalized_playoff_wins)

#just for teams who made the playoffs at all
# Filter to only teams that made playoffs
playoff_teams_only <- team_avg_starter_pct %>%
  filter(playoff_games > 0)

# Then run correlations on the filtered data
cor.test(playoff_teams_only$avg_pct_starter_points, playoff_teams_only$playoff_win_pct)

cor.test(playoff_teams_only$avg_pct_starter_points, playoff_teams_only$normalized_playoff_wins)

#2025 example
team_avg_starter_pct %>% filter(season==2025) %>% arrange(desc(avg_pct_starter_points)) %>% select(team_name, season, avg_pct_starter_points, made_playoffs)
kable(team_avg_starter_pct%>%filter(season==2025) %>%select(team_name, season, avg_pct_starter_points, made_playoffs), digits = 1, col.names = c("Team", "Season","Starter %", "Made Playoffs?"))
###################################################################
#visualizations
ggplot(team_avg_starter_pct, aes(x = avg_pct_starter_points, y = playoff_win_pct)) +
  geom_point() +
  labs(
    title = "Starter Point % vs Playoff Win %",
    x = "Average % of Points from Starters",
    y = "Playoff Win Percentage",
    caption = "Each point represents a team-season"
  ) +
  theme_minimal()

#with trend line
ggplot(team_avg_starter_pct, aes(x = avg_pct_starter_points, y = playoff_win_pct)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Team Starter Point Production and Playoff Success",
    x = "Average % of Points from Starters",
    y = "Playoff Win Percentage",
    subtitle = paste0("r = ", round(cor(team_avg_starter_pct$avg_pct_starter_points, 
                                       team_avg_starter_pct$playoff_win_pct, 
                                       use = "complete.obs"), 3)),
    caption = "Each point represents a team-season combination"
  ) +
  theme_minimal()

ggplot(team_avg_starter_pct, aes(x = "", y = avg_pct_starter_points)) +
  geom_violin(fill = "lightblue", alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.7) +
  labs(
    title = "Distribution of Average Starter Point Percentage",
    y = "Average % of Points from Starters",
    x = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

ggplot(team_avg_starter_pct, aes(x = made_playoffs, y = avg_pct_starter_points, fill = made_playoffs)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.7) +
  labs(
    title = "Starter Point % Distribution: Playoff vs Non-Playoff Teams",
    x = "Made Playoffs",
    y = "Average % of Points from Starters",
    fill = "Made Playoffs"
  ) +
  theme_minimal()

ggplot(team_avg_starter_pct, aes(x = avg_pct_starter_points)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "lightblue", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  labs(
    title = "Distribution of Average Starter Point Percentage",
    x = "Average % of Points from Starters",
    y = "Density"
  ) +
  theme_minimal()


# Create your championship winners lookup
champions <- tribble(
  ~team_name, ~season,
  "Shock", 2006,
  "Mercury", 2007,
  "Shock", 2008,
  "Mercury", 2009,
  "Storm", 2010,
  "Lynx", 2011,
  "Fever", 2012,
  "Lynx", 2013,
  "Mercury", 2014,
  "Lynx", 2015,
  "Sparks", 2016,
  "Lynx", 2017,
  "Storm", 2018,
  "Mystics", 2019,
  "Storm", 2020,
  "Sky", 2021,
  "Aces", 2022,
  "Aces", 2023,
  "Liberty", 2024,
)
# Add a champion indicator to the champions dataset
champions <- champions %>%
  mutate(is_champion = TRUE)

# Join and create plot data
plot_data <- team_avg_starter_pct %>%
  left_join(champions, by = c("team_name", "season")) %>%
  mutate(is_champion = coalesce(is_champion, FALSE))


ggplot(plot_data, aes(x = "", y = avg_pct_starter_points)) +
  geom_boxplot(width = 0.1, fill = "pink", alpha = 0.7) +
  geom_point(data = filter(plot_data, is_champion), aes(y = avg_pct_starter_points), color = "blue", size = 3, shape = 16) +
  geom_text_repel(data = filter(plot_data, is_champion),aes(y = avg_pct_starter_points, label = paste0(substr(team_name, 1, 10), "'", substr(season, 3, 4))),size = 3.5, color = "black", nudge_x = 0.1,max.overlaps = 20) +
  labs(
    title = "Distribution of WNBA Team\nReg Season Starter Points Percentage",
    subtitle = "Blue dots show eventual finals winners,\nsince 2006",
    y = "Average % of Points from Starters",
    x = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank())


###################################################################
# Regular season starter percentages
regular_season_starter_pct <- wnba_player_box %>%
  filter(!is.na(points) & season_type == 2) %>%  # Regular season only
  group_by(season, game_id, game_date, team_id, team_name, team_score) %>%
  summarise(starter_points = sum(points[starter == TRUE], na.rm = TRUE), .groups = "drop") %>%
  mutate(pct_starter_points = starter_points / team_score * 100) %>%
  group_by(team_name, season) %>%
  summarise(reg_season_avg_pct_starter = mean(pct_starter_points, na.rm = TRUE), .groups = "drop")

# Playoff starter percentages  
playoff_starter_pct <- wnba_player_box %>%
  filter(!is.na(points) & season_type == 3) %>%  # Playoffs only
  group_by(season, game_id, game_date, team_id, team_name, team_score) %>%
  summarise(starter_points = sum(points[starter == TRUE], na.rm = TRUE), .groups = "drop") %>%
  mutate(pct_starter_points = starter_points / team_score * 100) %>%
  group_by(team_name, season) %>%
  summarise(playoff_avg_pct_starter = mean(pct_starter_points, na.rm = TRUE), .groups = "drop")

# Join regular season and playoff data
comparison_data <- regular_season_starter_pct %>%
  inner_join(playoff_starter_pct, by = c("team_name", "season"))

# Calculate correlation
cor.test(comparison_data$reg_season_avg_pct_starter, comparison_data$playoff_avg_pct_starter)

# View the data
comparison_data

ggplot(comparison_data, aes(x = reg_season_avg_pct_starter, y = playoff_avg_pct_starter)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Perfect correlation line
  labs(
    title = "Regular Season vs Playoff Starter Point Percentage",
    x = "Regular Season Avg % Points from Starters",
    y = "Playoff Avg % Points from Starters",
    subtitle = "Dashed red line shows perfect correlation"
  ) +
  theme_minimal()

cor.test(comparison_data$reg_season_avg_pct_starter, comparison_data$playoff_avg_pct_starter)
