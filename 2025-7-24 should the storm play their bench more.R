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
  group_by(game_id, game_date, team_id, team_name) %>%
  summarise(
    total_team_minutes = sum(minutes, na.rm = TRUE),
    starter_minutes = sum(minutes[starter == TRUE], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_starter_minutes = starter_minutes / total_team_minutes * 100,
    game_team = paste(team_name, format(game_date, "%m-%d"))
  ) %>%
  arrange(pct_starter_minutes)


print(starter_minutes_pct, n=350)

subset(starter_minutes_pct, game_date=='2025-07-24')

#team avgs
team_avg_starter_pct <- starter_minutes_pct %>%
  filter(team_name != "TEAM COLLIER" & team_name !="TEAM CLARK") %>%
  group_by(team_name) %>%
  summarise(
    avg_pct_starter_minutes = mean(pct_starter_minutes, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_pct_starter_minutes))

team_avg_starter_pct

kable(team_avg_starter_pct, digits = 1, col.names = c("Team", "Starter %"))


ggplot(team_avg_starter_pct, aes(x=team_name, y=avg_pct_starter_minutes)) +
  geom_col() +
  theme_minimal()




wnba_player_box %>%
  filter(!is.na(minutes) & season_type==2) %>%
  group_by(team_id, team_name) %>%
  summarise(
    total_team_minutes = sum(minutes, na.rm = TRUE),
    starter_minutes = sum(minutes[starter == TRUE], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_starter_minutes = starter_minutes / total_team_minutes * 100
  ) %>%
  arrange(desc(pct_starter_minutes))