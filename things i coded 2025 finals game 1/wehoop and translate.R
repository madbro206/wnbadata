pacman::p_load(wehoop, dplyr, ggplot2, tictoc, progressr, BasketballAnalyzeR)

#load data
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

###################################################
# Find all Mercury games by abbreviation
mercury_ids <- unique(
  c(
    wnba_pbp$home_team_id[wnba_pbp$home_team_abbrev == "PHX"],
    wnba_pbp$away_team_id[wnba_pbp$away_team_abbrev == "PHX"]
  )
)
mercury_id <- mercury_ids[!is.na(mercury_ids)][1]

#make sure data types match
wnba_pbp$team_id <- as.character(wnba_pbp$team_id)
mercury_id <- as.character(mercury_id)

#add flag for Mercury scoring events
wnba_pbp$mercury_scored <- wnba_pbp$scoring_play & wnba_pbp$team_id == mercury_id

#summarize for each game and period
mercury_period_points <- wnba_pbp %>%
  filter(mercury_scored) %>%
  group_by(game_date, period_number) %>%
  summarise(period_points = sum(score_value, na.rm = TRUE), .groups = "drop")

#compute average points by period across all games
avg_mercury_points_by_period <- mercury_period_points %>%
  group_by(period_number) %>%
  summarise(mean_period_points = mean(period_points), n_games = n(), .groups = "drop")


#sum points by game (regular time only, 4 quarters)
mercury_quarters_points <- mercury_period_points %>%
  filter(period_number %in% 1:5) %>%
  group_by(game_date) %>%
  summarise(points_sum = sum(period_points))


#compare to team box total
mercury_box_points <- wnba_team_box %>%
  filter(team_id == mercury_id) %>%
  select(game_date, team_score)

#check that the total number of points counted is correct
comparison <- left_join(mercury_quarters_points, mercury_box_points, by = "game_date")
comparison <- comparison %>% mutate(match = points_sum == team_score)

print(comparison, n=52)


#######################################################################
#plots
#whole season

ggplot(avg_mercury_points_by_period, aes(x = factor(period_number), y = mean_period_points)) +
  geom_bar(stat = "identity", fill = "purple") +
  geom_text(aes(label = round(mean_period_points, 1)), 
            vjust = 2.5, 
            color = "black", 
            size = 4) +
  labs(
    title = "Phoenix Mercury Average Points by Period",
    x = "Period Number",
    y = "Average Points"
  ) +
  theme_minimal()

#######################################################################
#avg net points per quarter
# Aggregate points per team per game and period
points_by_team_game_period <- wnba_pbp %>%
  filter(scoring_play) %>%
  group_by(game_id, period_number, team_id) %>%
  summarise(period_points = sum(score_value, na.rm = TRUE), .groups = "drop")

# Get Mercury's points
mercury_points <- points_by_team_game_period %>%
  filter(team_id == mercury_id) %>%
  rename(mercury_points = period_points)

# Get opponent's points by anti-joining Mercury's team_id
opponent_points <- points_by_team_game_period %>%
  filter(team_id != mercury_id) %>%
  rename(opponent_id = team_id, opponent_points = period_points)

# Merge both by game and period
net_points_by_period <- mercury_points %>%
  left_join(opponent_points, by = c("game_id", "period_number")) %>%
  mutate(net_points = mercury_points - opponent_points)

# Now average net points by period across all games
avg_net_points_by_period <- net_points_by_period %>%
  group_by(period_number) %>%
  summarise(avg_net_points = mean(net_points, na.rm = TRUE), n_games = n(), .groups = "drop")


#check if count is correct
pbp_team_points <- wnba_pbp %>%
  filter(scoring_play) %>%
  group_by(game_id, team_id) %>%
  summarise(pbp_points = sum(score_value, na.rm = TRUE), .groups = "drop")

# Make sure team_id columns are the same type
wnba_team_box$team_id <- as.character(wnba_team_box$team_id)
pbp_team_points$team_id <- as.character(pbp_team_points$team_id)

box_team_points <- wnba_team_box %>%
  select(game_id, team_id, team_score)

points_comparison <- pbp_team_points %>%
  left_join(box_team_points, by = c("game_id", "team_id")) %>%
  mutate(points_match = pbp_points == team_score)

# View mismatches
points_comparison %>% filter(!points_match)


ggplot(avg_net_points_by_period, aes(x = factor(period_number), y = avg_net_points)) +
  geom_bar(stat = "identity", fill = "purple") +
  geom_text(aes(label = round(avg_net_points, 1)), 
            vjust = 2.5, 
            color = "black", 
            size = 4) +
  labs(
    title = "Phoenix Mercury Average Net Points by Period",
    x = "Period Number",
    y = "Average Net Points"
  ) +
  theme_minimal()
#######################################################################
# Filter for playoff games and Mercury scoring events
mercury_playoff_points_by_period <- wnba_pbp %>%
  filter(season_type == 3, team_id == mercury_id, scoring_play) %>%
  group_by(game_id, period_number) %>%
  summarise(period_points = sum(score_value, na.rm = TRUE), .groups = "drop")

# Calculate average points per period in playoffs
avg_mercury_playoff_points <- mercury_playoff_points_by_period %>%
  group_by(period_number) %>%
  summarise(mean_period_points = mean(period_points), n_games = n(), .groups = "drop")

ggplot(avg_mercury_playoff_points, aes(x = factor(period_number), y = mean_period_points)) +
  geom_bar(stat = "identity", fill = "purple") +
  geom_text(aes(label = round(mean_period_points, 1)), 
            vjust = 2.5, 
            color = "black", 
            size = 4) +
  labs(
    title = "Phoenix Mercury Average Points by Period",
    subtitle="Playoffs",
    x = "Period Number",
    y = "Average Points"
  ) +
  theme_minimal()

# Summarize points for all teams by game and period (playoffs only)
points_playoffs <- wnba_pbp %>%
  filter(season_type == 3, scoring_play) %>%
  group_by(game_id, period_number, team_id) %>%
  summarise(period_points = sum(score_value, na.rm = TRUE), .groups = "drop")

# Mercury's period points
mercury_points <- points_playoffs %>%
  filter(team_id == mercury_id) %>%
  rename(mercury_points = period_points)

# Opponent's period points (not Mercury team_id)
opponent_points <- points_playoffs %>%
  filter(team_id != mercury_id) %>%
  rename(opponent_id = team_id, opponent_points = period_points)

# Join Mercury and opponent points per period and game
net_points_playoffs <- mercury_points %>%
  left_join(opponent_points, by = c("game_id", "period_number")) %>%
  mutate(net_points = mercury_points - opponent_points)

avg_net_points_by_period_playoffs <- net_points_playoffs %>%
  group_by(period_number) %>%
  summarise(avg_net_points = mean(net_points, na.rm = TRUE), n_games = n(), .groups = "drop")

# Add vjust column (outside of ggplot)
avg_net_points_by_period_playoffs <- avg_net_points_by_period_playoffs %>%
  mutate(vjust_text = ifelse(avg_net_points >= 0, -0.5, 1.5))

# Plot using the new vjust column
ggplot(avg_net_points_by_period_playoffs, aes(x = factor(period_number), y = avg_net_points)) +
  geom_bar(stat = "identity", fill = "purple") +
  geom_text(aes(label = round(avg_net_points, 1), vjust = vjust_text),
            color = "black",
            size = 4) +
  labs(
    title = "Phoenix Mercury Net Points by Period",
    subtitle = "Playoffs",
    x = "Period Number",
    y = "Average Net Points"
  ) +
  theme_minimal()

