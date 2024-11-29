#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, tidyr, glue, tictoc, progressr, BasketballAnalyzeR, ggplot2, gt)

#load data
#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box()
})
tictoc::toc()

##############################################################################################################
#overall starter vs bench minutes by team
#group by team, then calculate minutes by players when starter==TRUE and when starter==FALSE (bench)

data <- wbb_player_box %>%
	group_by(team_location, starter) %>%
	summarize(min=sum(minutes, na.rm=TRUE))

data_summary <- data %>%
  pivot_wider(names_from = starter, values_from = min, names_prefix = "min_") %>%
  rename(starter_minutes = min_TRUE, bench_minutes = min_FALSE)

data_summary$total <- data_summary$bench_minutes + data_summary$starter_minutes

#make chart

# Prepare the data
data_long <- data_summary %>%
  pivot_longer(
    cols = c(bench_minutes, starter_minutes),
    names_to = "minute_type",
    values_to = "minutes"
  ) %>%
  group_by(team_location) %>%
  mutate(total_minutes = sum(minutes),
         percentage = (minutes / total_minutes) * 100) %>%
  ungroup()

# Calculate the maximum percentage of starter minutes for each team
data_summary_reordered <- data_long %>%
  filter(minute_type == "starter_minutes") %>%
  arrange(desc(percentage)) %>%
  mutate(team_location = factor(team_location, levels = unique(team_location)))

# Join the reordered factor levels back to the full dataset
data_long <- data_long %>%
  mutate(team_name = factor(team_location, levels = levels(data_summary_reordered$team_location)))
  
custom_colors <- c("starter_minutes" = "#f88158", "bench_minutes" = "#fceecb")  
 
gt(data_summary_reordered)
  
# Plot with horizontal bars and percentage labels
ggplot(data_long, aes(x = team_name, y = percentage, fill = minute_type)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(percentage / 100, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 4) +
  coord_flip() +  # Flip the coordinates for horizontal bars
  labs(
    title = "Which NCAA team uses their bench the most?",
    subtitle = "2025 season through Nov 19",
    x = NULL,
    y = "Percentage of Total Team Playing Time",
    caption = "Source: wehoop | Graphic: @wnbadata",
    fill = "Type"
  ) +
  scale_fill_manual(values = custom_colors) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0),
    plot.caption= element_text(hjust=1.5, vjust=3),
    axis.text.x = element_text(angle = 0, hjust = 0),
    axis.text.y = element_text(size = 12)  # Adjust the size of the team labels here
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))
##############################################################################################################
#starter vs bench minutes for a team vs game scoring margin
by_game <- wbb_player_box %>%
  group_by(game_date, team_location) %>%
  summarize(
    total_minutes = sum(minutes, na.rm = TRUE),
    starter_minutes = sum(minutes[starter == TRUE], na.rm = TRUE),
    margin = first(team_score) - first(opponent_team_score),
    .groups = "drop"
  ) %>%
  mutate(
    starter_percentage = (starter_minutes / total_minutes) * 100,
    abs_margin = abs(margin)
  )
  
  cor(by_game$starter_percentage, by_game$abs_margin)
  
ggplot(by_game, aes(x = abs_margin, y = starter_percentage/100)) +
  geom_point(color = "blue", size=1) +
  labs(x = "Final Score Margin", y = "Starter Minutes (percent of total)", 
       title = "Starter percentage of team minutes vs final score margin", subtitle="NCAA wbb through 11/19") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()


  
#d1 percentage of starter minutes out of total  
sum(by_game$starter_minutes)/sum(by_game$total_minutes)  
  
##############################################################################################################
#starters vs bench for current top 25 teams' games only

top_25_teams <- c("Louisville", "Alabama", "Oregon", "Illinois", "Nebraska", "NC State", "TCU", "Baylor", "Ole Miss", "North Carolina", "Kentucky", "Duke", "West Virginia", "Ohio State", "Maryland", "Kansas State", "Oklahoma", "Iowa State", "LSU", "Notre Dame", "UCLA", "Texas", "USC", "UConn", "South Carolina")

filtered_data <- wbb_player_box %>%
  filter(team_location %in% top_25_teams)

by_game_25 <- filtered_data %>%
  group_by(game_date, team_location) %>%
  summarize(
    total_minutes = sum(minutes, na.rm = TRUE),
    starter_minutes = sum(minutes[starter == TRUE], na.rm = TRUE),
    margin = first(team_score) - first(opponent_team_score),
    .groups = "drop"
  ) %>%
  mutate(
    starter_percentage = (starter_minutes / total_minutes) * 100,
    abs_margin = abs(margin)
  )
  
  cor(by_game_25$starter_percentage, by_game_25$abs_margin)
  
ggplot(by_game_25, aes(x = abs_margin, y = starter_percentage)) +
  geom_point(color = "blue") +
  labs(x = "Absolute Margin", y = "Starter Percentage", title="Starter percentage of team minutes by final score margin") +
  ylim(0, 100) +
  theme_minimal()
  
data_25 <- filtered_data %>%
	group_by(team_location, starter) %>%
	summarize(min=sum(minutes, na.rm=TRUE))

data_summary_25 <- data_25 %>%
  pivot_wider(names_from = starter, values_from = min, names_prefix = "min_") %>%
  rename(starter_minutes = min_TRUE, bench_minutes = min_FALSE)

data_summary_25 $total <- data_summary_25 $bench_minutes + data_summary_25 $starter_minutes

#make chart

# Prepare the data
data_long_25 <- data_summary_25 %>%
  pivot_longer(
    cols = c(bench_minutes, starter_minutes),
    names_to = "minute_type",
    values_to = "minutes"
  ) %>%
  group_by(team_location) %>%
  mutate(total_minutes = sum(minutes),
         percentage = (minutes / total_minutes) * 100) %>%
  ungroup()

# Calculate the maximum percentage of starter minutes for each team
data_summary_reordered_25 <- data_long_25 %>%
  filter(minute_type == "starter_minutes") %>%
  arrange(percentage) %>%
  mutate(team_location = factor(team_location, levels = unique(team_location)))

# Join the reordered factor levels back to the full dataset
data_long_25 <- data_long_25 %>%
  mutate(team_name = factor(team_location, levels = levels(data_summary_reordered$team_location)))
  
custom_colors <- c("starter_minutes" = "#f88158", "bench_minutes" = "#fceecb")  
 
gt(data_summary_reordered_25)
  
# Plot with horizontal bars and percentage labels
ggplot(data_long_25, aes(x = team_name, y = percentage, fill = minute_type)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(percentage / 100, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 4) +
  coord_flip() +  # Flip the coordinates for horizontal bars
  labs(
    title = "Which Top-25 NCAA team uses their bench the most?",
    subtitle = "2025 season through Nov 19",
    x = NULL,
    y = "Percentage of Total Team Playing Time",
    caption = "Source: wehoop | Graphic: @wnbadata",
    fill = "Type"
  ) +
  scale_fill_manual(values = custom_colors) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0),
    plot.caption= element_text(hjust=1.5, vjust=3),
    axis.text.x = element_text(angle = 0, hjust = 0),
    axis.text.y = element_text(size = 12)  # Adjust the size of the team labels here
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))
 
