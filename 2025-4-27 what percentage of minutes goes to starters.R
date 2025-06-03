#Use wehoop package to download WNBA data
pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, ggplot2, knitr)

tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box()
})
tictoc::toc()

wnba_player_box$minutes <- as.numeric(wnba_player_box$minutes)

#by team game
starter_minutes_pct <- wnba_player_box %>%
  filter(!is.na(minutes) & season_type==2) %>%
  group_by(game_id, game_date, team_id, team_name, team_score, opponent_team_score) %>%
  summarise(
    total_team_minutes = sum(minutes, na.rm = TRUE),
    starter_minutes = sum(minutes[starter == TRUE], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_starter_minutes = starter_minutes / total_team_minutes * 100,
    game_team = paste(team_name, format(game_date, "%m-%d")),
    margin = abs(team_score - opponent_team_score)
  ) %>%
  arrange(pct_starter_minutes)


print(starter_minutes_pct, n=88)

ggplot(starter_minutes_pct, aes(x=reorder(game_team, pct_starter_minutes), y=pct_starter_minutes)) + 
  geom_bar(stat = "identity")


#margin vs pct starter minutes
ggplot(starter_minutes_pct, aes(x = pct_starter_minutes, y = margin)) +
  geom_point() +
  labs(
    x = "Percent Starter Minutes",
    y = "Game Margin",
    title = "Game Margin vs. Percent of Team Minutes Played by Starters"
  ) +
  theme_minimal()

ggplot(starter_minutes_pct, aes(x = pct_starter_minutes, y = margin)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Percent Starter Minutes",
    y = "Game Margin",
    title = "Game Margin vs. Percent of Team Minutes Played by Starters",
    subtitle="WNBA 2025 Season Through 6/1"
  ) +
  theme_minimal()


cor(starter_minutes_pct$margin, starter_minutes_pct$pct_starter_minutes)



#team avgs
team_avg_starter_pct <- starter_minutes_pct %>%
  group_by(team_name) %>%
  summarise(
    avg_pct_starter_minutes = mean(pct_starter_minutes, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_pct_starter_minutes))

team_avg_starter_pct

kable(team_avg_starter_pct, digits = 1, col.names = c("Team", "Starter %"))
