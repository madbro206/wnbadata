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

print(starter_minutes_pct, n=476)

starter_minutes_pct <- starter_minutes_pct %>%
  filter(team_name !="TEAM COLLIER" & team_name != "TEAM CLARK") 

#team avgs
team_avg_starter_pct <- starter_minutes_pct %>%
  filter(team_name !="TEAM COLLIER" & team_name != "TEAM CLARK") %>%
  group_by(team_name) %>%
  summarise(
    avg_pct_starter_minutes = mean(pct_starter_minutes, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_pct_starter_minutes))

team_avg_starter_pct

kable(team_avg_starter_pct, digits = 1, col.names = c("Team", "Starter %"))



###############################################################################
#visualization of starter percentage of minutes over time
starter_minutes_pct_ts <- starter_minutes_pct %>%
  group_by(team_id) %>%
  arrange(game_date, .by_group = TRUE) %>%
  mutate(game_number = row_number()) %>%
  ungroup()

ggplot(starter_minutes_pct_ts,
  aes(x = game_number, y = pct_starter_minutes)) +
geom_line(alpha = 0.4) +
geom_point(size = 0.8) +
geom_smooth(method = "loess", se = TRUE, span = 0.4) +
facet_wrap(~ team_name, scales = "free_y") +
labs(x = "Game number", y = "Starter minutes (%)",
  title = "Starter-minute share over time") +
theme_minimal()

#all on the same scale
ggplot(starter_minutes_pct_ts,
  aes(game_number, pct_starter_minutes)) +
geom_line(alpha = 0.4) +
geom_point(size = 0.8) +
geom_smooth(method = "loess", se = TRUE, span = 0.4) +
facet_wrap(~ team_name, scales = "fixed") +
scale_y_continuous(limits = c(50, 90)) + #scale
labs(x = "Game number", y = "Starter minutes (%)",
  title = "Starter-minute share over time") +
theme_minimal()

#sparks and storm only
ggplot(dplyr::filter(starter_minutes_pct_ts,
  team_name %in% c("Storm","Sparks")),
aes(game_number, pct_starter_minutes)) +
geom_line(alpha = 0.4) +
geom_point(size = 1) +
geom_smooth(method = "loess", se = TRUE, span = 0.4) +
facet_wrap(~ team_name, scales = "fixed") +
scale_y_continuous(limits = c(50, 90)) +
labs(x = "Game number", y = "Starter minutes (%)",
title = "Starter-minute share over time") +
theme_minimal()

###############################################################################
#idk this business
library(slider)

starter_minutes_roll <- starter_minutes_pct_ts %>%
  group_by(team_id, team_name) %>%
  arrange(game_number, .by_group = TRUE) %>%
  mutate(pct_roll5 = slide_dbl(pct_starter_minutes, mean,
                               .before = 4, .complete = TRUE, .na_rm = TRUE)) %>%
  ungroup()

# N-game blocks
by_blocks <- starter_minutes_pct_ts %>%
  mutate(block = (game_number - 1) %/% 5 + 1) %>%
  group_by(team_name, block) %>%
  summarise(avg_pct = mean(pct_starter_minutes, na.rm = TRUE), .groups = "drop")

# Monthly
by_month <- starter_minutes_pct %>%
  mutate(month = format(game_date, "%Y-%m")) %>%
  group_by(team_name, month) %>%
  summarise(avg_pct = mean(pct_starter_minutes, na.rm = TRUE), .groups = "drop")

print(by_blocks, n=120)
