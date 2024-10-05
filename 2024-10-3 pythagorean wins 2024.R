#how to get the WNBA data
#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, ggplot2, gt)


#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season=c(2023,2024)) #will use the two 40 game seasons to find pythagorean exponent
})
tictoc::toc()

Tbox <- wnba_team_box %>%
  filter(season_type == 2) %>% #regular season
  filter(team_name != "Team USA" & team_name != "Team WNBA" & team_name != "Team Stewart" & team_name != "Team Wilson") %>% #no all star games
  filter(as.Date(game_date) != as.Date("2023-08-15") & as.Date(game_date) != as.Date("2024-06-25")) %>% #no commissioners cup games
  group_by(season, team_name) %>%
  summarise(GP = n(), 
            PTS = sum(team_score, na.rm = TRUE),
            PTS_allowed = sum(opponent_team_score, na.rm=TRUE),
            W = sum(team_winner == TRUE), 
            L = sum(team_winner == FALSE)) %>%
  mutate(Win_Percentage = W / GP) %>%
  rename(Season = season, Team = team_name) %>%
  as.data.frame()
  
  
  
#find pythagorean exponent experimentally
  
pythagorean_expectation <- function(pts_for, pts_against, exponent) {
  (pts_for^exponent) / (pts_for^exponent + pts_against^exponent)
}

calculate_error <- function(exponent, pts_for, pts_against, actual_win_pct) {
  expected_win_pct <- pythagorean_expectation(pts_for, pts_against, exponent)
  mean(abs(expected_win_pct - actual_win_pct))
}


optimal_exponent <- optim(
  par = 2,  # Starting value for the exponent
  fn = calculate_error,
  pts_for = Tbox$PTS,
  pts_against = Tbox$PTS_allowed,
  actual_win_pct = Tbox$Win_Percentage,
  method = "Brent",
  lower = 1,
  upper = 25
)

best_exponent <- optimal_exponent$par

###############################################################################################################


#visualize exponents errors
calculate_error_range <- function(exponents, pts_for, pts_against, actual_win_pct) {
  sapply(exponents, function(x) {
    expected_win_pct <- (pts_for^x) / (pts_for^x + pts_against^x)
    mean(abs(expected_win_pct - actual_win_pct))
  })
}

exponents <- seq(1, 25, by = 0.1)

errors <- calculate_error_range(exponents, Tbox$PTS, Tbox$PTS_allowed, Tbox$Win_Percentage)

error_df <- data.frame(Exponent = exponents, Error = errors)

ggplot(error_df, aes(x = Exponent, y = Error)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = best_exponent, color = "red", linetype = "dashed") +
  geom_text(aes(x = best_exponent, y = max(Error), label = paste("Best Exponent:", round(best_exponent, 2))),
            vjust = -0.5, hjust = 1.1, color = "red") +
  labs(title = "Error in Pythagorean Win Percentage by Exponent",
       x = "Exponent",
       y = "Mean Absolute Error") +
  theme_minimal()
  
  
###############################################################################################################
#calculate expected wins  

Tbox <- Tbox %>%
  mutate(
    Team_Season = paste(Team, Season, sep = "_"),
    pythagorean_win_pct = pythagorean_expectation(PTS, PTS_allowed, best_exponent),
    win_pct_difference = Win_Percentage - pythagorean_win_pct
  ) %>%
  arrange(win_pct_difference)

ggplot(Tbox, aes(x = reorder(Team_Season, win_pct_difference), y = win_pct_difference, fill = win_pct_difference)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  labs(
    title = "2023-2024 WNBA Overachievers and Underachievers",
    subtitle ="Difference in Actual Win% vs Pythagorean Expectation",
    x="",
    y = "Win Percentage Difference",
    caption="source: stats.wnba | graphic: @wnbadata"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate("text", x = Inf, y = max(Tbox$win_pct_difference) * 0.9, 
           label = "Overachievers", hjust = 1.5, vjust = 1, color = "darkblue", fontface = "bold") +
  annotate("text", x = -Inf, y = min(Tbox$win_pct_difference) * 0.9, 
           label = "Underachievers", hjust = 0, vjust = 0, color = "darkred", fontface = "bold")


###############################################################################################################
#create nice table
Tbox_selected <- Tbox %>%
  select(Team_Season, PTS, PTS_allowed, Win_Percentage, pythagorean_win_pct, win_pct_difference) %>%  
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  arrange(win_pct_difference)


gt_table <- gt(Tbox_selected)

gt_table <- gt_table %>%
  tab_header(
    title = md("Pythagorean Wins"),
    subtitle = "Expected vs actual wins for 2023-2024 WNBA teams"
  )%>%
  cols_label(
    Team_Season = "Team",
    PTS = "Points Scored",
    PTS_allowed = "Points Allowed",
    Win_Percentage = "Actual Win%",
    pythagorean_win_pct = "Expected Win%",
    win_pct_difference="Difference" 
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "orange"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_source_note(
    source_note = "source: stats.wnba | table: @wnbadata"
  )

gt_table


  
###############################################################################################################
#find pythagorean exponent theoretically (didn't work well for me)
#https://en.wikipedia.org/wiki/Pythagorean_expectation
# Calculate the standard deviation of runs scored
std_dev_runs <- sd(Tbox$PTS)

# Calculate the average number of runs scored
average_runs <- mean(Tbox$PTS)

# Compute sigma (standard deviation divided by average)
sigma <- std_dev_runs / average_runs

# Calculate 2/(σ√π)
result <- 2 / (sigma * sqrt(pi))

# Print the result
print(paste("2/(σ√π) =", result))