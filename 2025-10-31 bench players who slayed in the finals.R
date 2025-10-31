library(dplyr)
library(lubridate)
library(wehoop)

#championship teams
finals_winners <- tribble(
  ~season, ~champion,
  2025, "Aces",
  2024, "Liberty",
  2023, "Aces",
  2022, "Aces",
  2021, "Sky",
  2020, "Storm",
  2019, "Mystics",
  2018, "Storm",
  2017, "Lynx",
  2016, "Sparks",
  2015, "Lynx",
  2014, "Mercury",
  2013, "Lynx",
  2012, "Fever",
  2011, "Lynx",
  2010, "Storm",
  2009, "Mercury",
  2008, "Shock",
  2007, "Mercury",
  2006, "Shock"
)

#Use game dates to identify Finals games
all_playoff_games <- wnba_player_box %>%
  filter(season_type == 3) %>%
  group_by(season, game_id) %>%
  slice(1) %>%  # Get one row per game
  arrange(season, game_date) %>%
  select(season, game_id, game_date) %>%
  ungroup()

#for each season, get the last 3-7 games (Finals games)
finals_game_ids <- all_playoff_games %>%
  group_by(season) %>%
  arrange(desc(game_date)) %>%
  mutate(
    games_from_end = row_number(),
    is_finals = case_when(
      season >= 2025 ~ games_from_end <= 4,  # Best-of-7 format but only had 4
      season >= 2006 ~ games_from_end <= 5  # Best-of-5 format
    )
  ) %>%
  filter(is_finals) %>%
  pull(game_id)

# Now filter to Finals games only (still a rough code)
finals_player_box <- wnba_player_box %>%
  filter(game_id %in% finals_game_ids)


# === FIND THE "HOTTEST" BENCH PLAYER FOR EACH CHAMPIONSHIP TEAM ===

# Step 1: Regular season stats (same as before)
regular_season_stats <- wnba_player_box %>%
  filter(season_type == 2) %>%
  group_by(season, athlete_id, athlete_display_name, team_name) %>%
  summarise(
    reg_games = n(),
    reg_minutes_avg = mean(minutes, na.rm = TRUE),
    reg_pts_avg = mean(points, na.rm = TRUE),
    reg_pts_sd = sd(points, na.rm = TRUE),
    reg_started = sum(starter == TRUE, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    is_bench_player = reg_started / reg_games < 0.5,
    is_role_player = reg_pts_avg < 15
  )

# Step 2: Finals games with context
finals_with_context <- finals_player_box %>%
  left_join(regular_season_stats,by = c('season', 'athlete_id', 'athlete_display_name', 'team_name'))

# Step 3: Calculate multiplier for ALL bench/role players in Finals
all_bench_finals_performances <- finals_with_context %>%
  filter(
    !is.na(reg_pts_avg), 
    reg_games >= 8,
    (is_role_player | is_bench_player)  # Only bench/role players
  ) %>%
  mutate(
    pts_above_avg = points - reg_pts_avg,
    pts_multiplier = points / pmax(reg_pts_avg, 0.1)  # Avoid division issues
  )

# Step 4: Filter to championship teams ONLY
championship_performances <- all_bench_finals_performances %>%
  inner_join(finals_winners, by = c('season', 'team_name' = 'champion'))

# Step 5: Find the HIGHEST multiplier game by bench player for each championship team
hottest_by_champion <- championship_performances %>%
  filter(is_bench_player==TRUE) %>% #generally a bench player 
  #filter(starter==FALSE) %>% #and came off bench in this game
  filter(game_date != as.Date('2018-09-04'))%>% #not a finals game
  group_by(season, team_name) %>%
  slice_max(order_by = pts_multiplier, n = 1, with_ties = FALSE) %>%
  select(
    season, 
    team_name, 
    athlete_display_name, 
    game_date,
    points, 
    reg_pts_avg, 
    pts_above_avg,
    pts_multiplier,
    field_goals_made,
    field_goals_attempted,
    three_point_field_goals_made,
    is_bench_player,
    is_role_player,
    reg_games,
    reg_started
  ) %>%
  arrange(desc(pts_multiplier))  # Sort by highest multiplier

# View the results
print(hottest_by_champion)

# Optional: Create a clean summary table
summary_table <- hottest_by_champion %>%
  mutate(
    pts_multiplier = round(pts_multiplier, 2),
    reg_pts_avg = round(reg_pts_avg, 1)
  ) %>%
  select(
    season,
    game_date,
    team_name,
    athlete_display_name,
    pts_multiplier,
    points,
    reg_pts_avg
  ) %>%
  arrange(season)

print("Hottest Bench Player Performance for Each Champion")
print(summary_table)

# Additional analysis: Distribution of multipliers
cat("\n=== MULTIPLIER STATISTICS ===\n")
cat(sprintf("Highest multiplier: %.2fx (by %s in %d)\n", 
            max(hottest_by_champion$pts_multiplier),
            hottest_by_champion$athlete_display_name[which.max(hottest_by_champion$pts_multiplier)],
            hottest_by_champion$season[which.max(hottest_by_champion$pts_multiplier)]))
cat(sprintf("Average multiplier: %.2fx\n", mean(hottest_by_champion$pts_multiplier)))
cat(sprintf("Median multiplier: %.2fx\n", median(hottest_by_champion$pts_multiplier)))
cat(sprintf("Lowest multiplier: %.2fx\n", min(hottest_by_champion$pts_multiplier)))

# How many champions had someone go 2x+ their average?
champions_2x <- sum(hottest_by_champion$pts_multiplier >= 2)
cat(sprintf("\nChampions with 2x+ performance: %d/%d (%.1f%%)\n",
            champions_2x, nrow(hottest_by_champion),
            (champions_2x/nrow(hottest_by_champion))*100))

# How many had 3x+?
champions_3x <- sum(hottest_by_champion$pts_multiplier >= 3)
cat(sprintf("Champions with 3x+ performance: %d/%d (%.1f%%)\n",
            champions_3x, nrow(hottest_by_champion),
            (champions_3x/nrow(hottest_by_champion))*100))



#Find the HIGHEST pts game by bench player for each championship team
hottest_by_champion2 <- championship_performances %>%
  filter(is_bench_player==TRUE) %>% #generally a bench player 
  #filter(starter==FALSE) %>% #and came off bench in this game
  filter(game_date != as.Date('2018-09-04'))%>% #not a finals game
  group_by(season, team_name) %>%
  slice_max(order_by = points, n = 1, with_ties = FALSE) %>%
  select(
    season, 
    team_name, 
    athlete_display_name, 
    game_date,
    points, 
    reg_pts_avg, 
    pts_above_avg,
    pts_multiplier,
    field_goals_made,
    field_goals_attempted,
    three_point_field_goals_made,
    is_bench_player,
    is_role_player,
    reg_games,
    reg_started
  ) %>%
  arrange(desc(pts_multiplier)) 

#clean summary table
summary_table2 <- hottest_by_champion2 %>%
  mutate(
    pts_multiplier = round(pts_multiplier, 2),
    reg_pts_avg = round(reg_pts_avg, 1)
  ) %>%
  select(
    season,
    game_date,
    team_name,
    athlete_display_name,
    pts_multiplier,
    points,
    reg_pts_avg
  ) %>%
  arrange(season)

summary_table2
