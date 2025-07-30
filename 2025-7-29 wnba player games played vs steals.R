#this loads all the packages I need at once, i could instead just do "library(wehoop)" for each package
pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, ggplot2)

#load data
#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box()
})
tictoc::toc()


#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box()
})
tictoc::toc()


#######################################################################################################
player_summary <- wnba_player_box %>%
  filter(!is.na(minutes) & minutes > 0) %>%
  group_by(athlete_display_name, team_name) %>%
  summarise(
    total_games = n(),
    games_with_steal = sum(steals > 0, na.rm = TRUE)
  ) %>%
  select(athlete_display_name, total_games, games_with_steal, team_name)

player_summary$highlight <- player_summary$athlete_display_name == "Gabby Williams"


# Add the highlight and color columns
player_summary$highlight <- player_summary$athlete_display_name == "Gabby Williams"
player_summary$dot_color <- ifelse(player_summary$team_name == "Storm", "green", "black")


ggplot(player_summary, aes(x = total_games, y = games_with_steal)) +
  # Plot all dots, using dot_color for Seattle green, others black
  geom_point(aes(color = dot_color), size = 3) +
  # Label only Gabby Williams
  geom_text(
    data = subset(player_summary, athlete_display_name == "Gabby Williams"),
    aes(label = athlete_display_name), 
    hjust = -2.8, vjust = -21, color = "red", fontface = "bold", size = 5
  ) +
  scale_color_identity() + # Honors manual color mapping
  labs(
    x = "Total Games Played",
    y = "No. games with 1+ steals",
    title = "WNBA: Games Played vs 1+ Steal Games (per Player)",
    subtitle = "WNBA 2025 Season through July 28"
  ) +
  scale_x_continuous(breaks = seq(0, max(player_summary$total_games), by = 5))+
  theme_minimal()


###########################################################################################
# Summarize total steals by player for the Seattle Storm
storm_steals <- wnba_player_box %>%
  filter(team_name == "Storm") %>%
  group_by(athlete_display_name) %>%
  summarise(total_steals = sum(steals, na.rm = TRUE)) %>%
  arrange(desc(total_steals))

# Create a bar chart
ggplot(storm_steals, aes(x = reorder(athlete_display_name, total_steals), y = total_steals)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  coord_flip() + # so names are readable
  labs(
    x = "Player",
    y = "Total Steals",
    title = "Total Steals by Seattle Storm Players",
    subtitle = "WNBA 2025 Season to Date"
  ) +
  theme_minimal()
