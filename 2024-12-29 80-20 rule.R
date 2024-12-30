#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, ggplot2)

#load data
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(season=c(2024))
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box()
})
tictoc::toc()

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
#translate into player totals
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

#wnba
wnba_Pbox <- wnba_player_box %>%
  group_by(season, athlete_display_name) %>%
  summarise(GP=sum(minutes>0, na.rm=TRUE), MIN=sum(minutes, na.rm = TRUE), PTS=sum(points, na.rm = TRUE),
    P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), P2p=100*P2M/P2A,
    P3M=sum(three_point_field_goals_made, na.rm = TRUE), P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), P3p=100*P3M/P3A,
    FTM=sum(free_throws_made, na.rm = TRUE), FTA=sum(free_throws_attempted, na.rm = TRUE), FTp=100*FTM/FTA,
    OREB=sum(offensive_rebounds, na.rm = TRUE), DREB=sum(defensive_rebounds, na.rm = TRUE), AST=sum(assists, na.rm = TRUE),
    TOV=sum(turnovers, na.rm = TRUE), STL=sum(steals, na.rm = TRUE), BLK=sum(blocks, na.rm = TRUE),
    PF=sum(fouls, na.rm = TRUE)) %>%
  rename(Season=season, Player=athlete_display_name) %>%
  as.data.frame()


##############################################################################################################
pareto_analysis <- function(df, column) {
  # Sort the dataframe by the specified column in descending order
  sorted_df <- df[order(df[[column]], decreasing = TRUE), ]
  # Calculate cumulative sum and total sum
  cumulative_sum <- cumsum(sorted_df[[column]])
  total_sum <- sum(sorted_df[[column]])
  # Calculate cumulative percentage
  cumulative_percentage <- cumulative_sum / total_sum
  # Find the index where cumulative percentage exceeds 80%
  pareto_index <- which(cumulative_percentage >= 0.8)[1]
  # Calculate the percentage of players contributing to 80% of the stat
  player_percentage <- pareto_index / nrow(df)
  return(player_percentage)
}

stats <- c("MIN", "PTS", "AST", "DREB", "OREB", "STL", "BLK", "PF", "TOV", "P2A", "P2M", "P3A", "P3M", "FTA", "FTM")

#analysis for ncaa
pareto_results <- sapply(stats, function(stat) pareto_analysis(Pbox, stat))
print(pareto_results)

#analysis for wnba
pareto_results_wnba <- sapply(stats, function(stat) pareto_analysis(wnba_Pbox, stat))
print(pareto_results_wnba)


#include players in output
pareto_analysis2 <- function(df, column) {
  # Sort the dataframe by the specified column in descending order
  sorted_df <- df[order(df[[column]], decreasing = TRUE), ]
  
  # Calculate cumulative sum and total sum
  cumulative_sum <- cumsum(sorted_df[[column]])
  total_sum <- sum(sorted_df[[column]])
  
  # Calculate cumulative percentage
  cumulative_percentage <- cumulative_sum / total_sum
  
  # Find the index where cumulative percentage exceeds 80%
  pareto_index <- which(cumulative_percentage > 0.8)[1]
  
  # Calculate the percentage of players contributing to 80% of the stat
  player_percentage <- pareto_index / nrow(df)
  
  # Get the list of players contributing to 80% of the stat
  top_players <- sorted_df$Player[1:pareto_index]
  
  # Return both the percentage and the list of players
  return(list(percentage = player_percentage, players = top_players))
}

pareto_results2 <- lapply(stats, function(stat) pareto_analysis2(Pbox, stat))

for (i in seq_along(stats)) {
  cat("\nStat:", stats[i], "\n")
  cat("Percentage:", pareto_results2[[i]]$percentage * 100, "%\n")
  cat("Top players:\n")
  print(pareto_results2[[i]]$players)
}


##############################################################################################################
pareto_data <- data.frame(
  Stat = names(pareto_results),
  Percentage = pareto_results * 100  # Convert to percentage
)

#sort data 
pareto_data <- pareto_data %>%
  arrange(Percentage)

#convert Stat to a factor with levels in the sorted order
pareto_data$Stat <- factor(pareto_data$Stat, levels = pareto_data$Stat)

#bar chart
ggplot(pareto_data, aes(x = Stat, y = Percentage)) +
  geom_bar(stat = "identity", fill = "#2591ea") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5, size = 2.5) +
  labs(
    title = "Percentage of Players Accounting for 80% of Each Stat",
    subtitle = "NCAAW 2025 season through 12/28",
    x = "Stat category",
    y = "Percentage of Players"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, max(pareto_data$Percentage) * 1.1) 





#WNBA bar chart
pareto_data2 <- data.frame(
  Stat = names(pareto_results_wnba),
  Percentage = pareto_results_wnba * 100  # Convert to percentage
)

#sort data 
pareto_data2 <- pareto_data2 %>%
  arrange(Percentage)

#convert Stat to a factor with levels in the sorted order
pareto_data2$Stat <- factor(pareto_data2$Stat, levels = pareto_data2$Stat)

#bar chart
ggplot(pareto_data2, aes(x = Stat, y = Percentage)) +
  geom_bar(stat = "identity", fill = "#ea9125") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5, size = 2.5) +
  labs(
    title = "Percentage of Players Accounting for 80% of Each Stat",
    subtitle = "WNBA 2024 Season",
    x = "Stat category",
    y = "Percentage of Players"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, max(pareto_data2$Percentage) * 1.1) 
