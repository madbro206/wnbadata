#win percentage by largest lead
pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, ggplot2)

#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(seasons=c(2006:2025))
})
tictoc::toc()

#convert largest_lead to integer and calculate the empirical win percentage
empirical_probs <- wnba_team_box %>%
  #filter(team_winner==FALSE) %>% #find teams who lost lead only
  filter(!is.na(largest_lead)) %>%  #remove rows where largest_lead is NA
  mutate(largest_lead = as.integer(largest_lead)) %>%  #convert to integer
  group_by(largest_lead) %>%
  summarize(
    wins = sum(team_winner == TRUE),
    losses = sum(team_winner == FALSE),
    total_games = n(),
    empirical_win_percentage = mean(team_winner) * 100
  ) %>%
  arrange(largest_lead)

print(result, n=50)

#games with lead lost
games <- wnba_team_box %>% 
  mutate(largest_lead = as.numeric(largest_lead)) %>%
  filter(largest_lead >=20 & team_winner == FALSE) %>%
  select(game_date, team_name, opponent_team_name, largest_lead, season_type) %>%
  arrange(desc(game_date))

games

playoff_games <- games %>%
  filter(season_type==3)

playoff_games

#create a scatter plot with smooth curve
ggplot(empirical_probs, aes(x = largest_lead, y = empirical_win_percentage)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +  # Add smooth curve
  scale_x_continuous(breaks = seq(0, max(empirical_probs$largest_lead, na.rm = TRUE), by = 5)) +
  scale_y_continuous(breaks = seq(0, max(empirical_probs$empirical_win_percentage, na.rm = TRUE), by = 5)) +  # Set breaks for every 5th x-axis number
  labs(title = "Win Percentage vs Largest Lead in WNBA 2016-2025",
       x = "Largest Lead",
       y = "Win Percentage") +
  theme_minimal()

######################################################################################################
#the above code is great byt the largest_lead column is NA for everything before 2016, so I had chatgpt help me calculate the largest_lead for each game using the pbp data

# Function to calculate largest lead for each team in each game from play-by-play data
calculate_largest_leads <- function(seasons_to_process) {
  
  # Load play-by-play data for specified seasons
  pbp_data <- load_wnba_pbp(seasons = seasons_to_process)
  
  # Calculate largest lead for each team in each game
  largest_leads <- pbp_data %>%
    filter(!is.na(away_score) & !is.na(home_score)) %>%
    mutate(
      home_lead = home_score - away_score,
      away_lead = away_score - home_score
    ) %>%
    group_by(game_id) %>%
    summarise(
      home_team_name = first(home_team_name),
      away_team_name = first(away_team_name),
      home_team_id = first(home_team_id),
      away_team_id = first(away_team_id),
      
      # Calculate largest lead for each team
      home_largest_lead = max(home_lead, na.rm = TRUE),
      away_largest_lead = max(away_lead, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    # Convert to long format for each team
    pivot_longer(
      cols = c(home_team_name, away_team_name),
      names_to = "location",
      values_to = "team_name"
    ) %>%
    mutate(
      team_id = ifelse(location == "home_team_name", home_team_id, away_team_id),
      calculated_largest_lead = ifelse(location == "home_team_name", 
                                     home_largest_lead, away_largest_lead)
    ) %>%
    select(game_id, team_id, team_name, calculated_largest_lead)
  
  return(largest_leads)
}

# Calculate for all available seasons (you can adjust this range)
# Start with a few seasons to test, then expand
seasons_range <- 2006:2025  # Adjust based on available data

calculated_leads <- calculate_largest_leads(seasons_range)

# Join with your existing wnba_team_box data
wnba_team_box_enhanced <- wnba_team_box %>%
  left_join(calculated_leads, by = c("game_id", "team_id")) %>%
  mutate(
    # Create a combined largest_lead column that uses existing data when available,
    # otherwise uses calculated data
    largest_lead_combined = case_when(
      !is.na(largest_lead) ~ as.numeric(largest_lead),  # Use existing data
      !is.na(calculated_largest_lead) ~ calculated_largest_lead,  # Use calculated
      TRUE ~ NA_real_  # Keep as NA if neither available
    )
  )

# Verification: Compare existing vs calculated for 2016+ games
verification <- wnba_team_box_enhanced %>%
  filter(season >= 2016, !is.na(largest_lead), !is.na(calculated_largest_lead)) %>%
  mutate(
    existing_lead = as.numeric(largest_lead),
    difference = existing_lead - calculated_largest_lead,
    match = abs(difference) <= 1  # Allow for small rounding differences
  ) %>%
  select(game_date, team_name.x, season, existing_lead, calculated_largest_lead, difference, match)

# Check verification results
cat("Verification Results:\n")
cat("Total comparisons:", nrow(verification), "\n")
cat("Exact matches:", sum(verification$match, na.rm = TRUE), "\n")
cat("Match percentage:", round(mean(verification$match, na.rm = TRUE) * 100, 1), "%\n")

# Show some examples of differences if any
if(any(!verification$match, na.rm = TRUE)) {
  cat("\nSample of non-matching cases:\n")
  print(head(verification[!verification$match & !is.na(verification$match), ]))
}

# Now use the enhanced dataset for your analysis
empirical_probs_enhanced <- wnba_team_box_enhanced %>%
  filter(!is.na(largest_lead_combined)) %>%
  mutate(largest_lead_combined = as.integer(largest_lead_combined)) %>%
  group_by(largest_lead_combined) %>%
  summarize(
    wins = sum(team_winner == TRUE),
    losses = sum(team_winner == FALSE),
    total_games = n(),
    empirical_win_percentage = mean(team_winner) * 100
  ) %>%
  arrange(largest_lead_combined)

games2 <- wnba_team_box_enhanced %>% 
  mutate(largest_lead_combined = as.numeric(largest_lead_combined)) %>%
  filter(largest_lead_combined >=15 & team_winner == FALSE) %>%
  select(game_date, team_name.x, opponent_team_name, largest_lead_combined, season_type) %>%
  arrange(desc(game_date))

print(games2, n=29)

playoff_games2 <- games2 %>%
  filter(season_type==3)

playoff_games2 %>% mutate(largest_lead=largest_lead_combined) %>% select(game_date,team_name.x,opponent_team_name,largest_lead) %>%arrange(desc(largest_lead))


wnba_team_box %>% 
  mutate(largest_lead = as.numeric(largest_lead)) %>%
  filter(season_type==3 & team_winner == FALSE) %>%
  select(game_date, team_name, opponent_team_name, largest_lead, season_type) %>%
  arrange(desc(largest_lead))



