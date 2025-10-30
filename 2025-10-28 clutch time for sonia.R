library(wehoop)
library(dplyr)
library(ggplot2)

#load data
wnba_pbp <- wehoop::load_wnba_pbp()
wnba_team_box <- wehoop::load_wnba_team_box()

#filter to regular season only
wnba_pbp <- wnba_pbp %>%
  filter(season_type==2 & !grepl("team", home_team_name, ignore.case = TRUE))
wnba_team_box <- wnba_team_box %>%
  filter(season_type==2 & !grepl("team", team_name, ignore.case = TRUE))

#add game winner info to pbp
wnba_pbp_with_winner <- wnba_pbp %>%
  left_join(
    wnba_team_box %>% 
      filter(team_home_away == "home") %>%
      select(game_id, team_winner) %>%
      rename(home_team_winner = team_winner),
    by = "game_id"
  ) %>%
  left_join(
    wnba_team_box %>% 
      filter(team_home_away == "away") %>%
      select(game_id, team_winner) %>%
      rename(away_team_winner = team_winner),
    by = "game_id"
  )


#identify clutch games (within 5 points in last 5 minutes of Q4)
clutch_games <- wnba_pbp_with_winner %>%
  #Filter to last 5 minutes of 4th quarter (300 seconds)
  filter(
    qtr == 4,
    end_quarter_seconds_remaining <= 300
  ) %>%
  #Calculate score margin for each play
  mutate(score_margin = abs(home_score - away_score)) %>%
  #Group by game
  group_by(game_id) %>%
  #Check if game was ever within 5 points
  summarize(
    was_clutch = any(score_margin <= 5),
    .groups = "drop"
  ) %>%
  #Keep only clutch games
  filter(was_clutch == TRUE)

#get team results for clutch games only
clutch_team_results <- wnba_team_box %>%
  # Keep only clutch games
  semi_join(clutch_games, by = "game_id") %>%
  select(game_id, team_id, team_abbreviation, team_winner)

#calculate clutch win percentage by team
clutch_win_pct <- clutch_team_results %>%
  group_by(team_id, team_abbreviation) %>%
  summarize(
    clutch_games = n(),
    clutch_wins = sum(team_winner),
    clutch_losses = sum(!team_winner),
    clutch_win_pct = mean(team_winner),
    .groups = "drop"
  ) %>%
  arrange(desc(clutch_win_pct))

clutch_win_pct$team <- clutch_win_pct$team_abbreviation
clutch_win_pct <- clutch_win_pct %>% select(team, clutch_wins, clutch_losses, clutch_win_pct)

print(clutch_win_pct)

#viz
clutch_long <- clutch_win_pct %>%
  mutate(clutch_games = clutch_wins + clutch_losses) %>%
  pivot_longer(cols = c(clutch_wins, clutch_losses),
               names_to = "result",
               values_to = "count") %>%
  mutate(
    result = factor(result, levels = c("clutch_losses", "clutch_wins")),
    team = reorder(team, clutch_win_pct)
  )

#bar chart
ggplot(clutch_long, aes(x = team, y = count, fill = result)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_text(data = clutch_win_pct %>% 
              mutate(clutch_games = clutch_wins + clutch_losses,
                     team = reorder(team, clutch_win_pct)),
            aes(x = team, y = clutch_games, label = paste0(clutch_games, "g")), 
            hjust = -0.3, size = 3.5, inherit.aes = FALSE) +
  geom_text(data = clutch_win_pct %>%
              mutate(clutch_games = clutch_wins + clutch_losses,
                     team = reorder(team, clutch_win_pct)),
            aes(x = team, y = clutch_games/2, 
                label = paste0(round(clutch_win_pct * 100, 1), "%")), 
            color = "white", fontface = "bold", size = 4, inherit.aes = FALSE) +
  scale_fill_manual(
    values = c("clutch_wins" = "#2c51a0", "clutch_losses" = "#d62728"),
    guide = "none"
  ) +
  coord_flip() +
  labs(
    title = "WNBA Clutch Performance by Team",
    subtitle = "Games within 5 points in final 5 minutes of 4th quarter",
    x = NULL,
    y = "Clutch Games",
    caption = "Blue = Wins, Red = Losses"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

###################################################################
#offensive efficiency in close games
close_game_offense <- wnba_pbp_with_winner %>%
  #filter to clutch moments
  filter(qtr == 4 & abs(home_score - away_score) <= 5 & season_type == 2) %>%
  #group by team
  group_by(team_id) %>%
  summarize(
    #calculate FG$
    fg_pct = sum(grepl("makes", text, ignore.case = TRUE)) / sum(shooting_play == TRUE),
    #count turnovers
    turnovers = sum(grepl("turnover", type_text, ignore.case = TRUE))
  )

#add in team names
close_game_offense <- close_game_offense %>%
left_join(
  wnba_team_box %>%
    distinct(team_id, team_abbreviation),
  by = "team_id"
) %>%
select(team_abbreviation, everything(), -team_id) %>%
arrange(desc(fg_pct))

print(close_game_offense)

#viz scatterplot
ggplot(close_game_offense, aes(x = turnovers, y = fg_pct)) +
  geom_point(aes(color = fg_pct), size = 5, alpha = 0.7) +
  geom_text(aes(label = team_abbreviation), 
            vjust = -1, size = 3.5, fontface = "bold") +
  scale_color_gradient2(low = "#d62728", mid = "#ff7f0e", high = "#2ca02c",
                        midpoint = 0.52, labels = scales::percent) +
  scale_y_continuous(labels = scales::percent, limits = c(0.45, 0.60)) +
  labs(
    title = "Close Game Offense: Efficiency vs Ball Security",
    subtitle = "4th quarter when within 5 points",
    x = "Turnovers",
    y = "Field Goal %",
    color = "FG%"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

###########################################################################
#clutch assist rate
clutch_assist_rate <- wnba_pbp_with_winner %>%
  filter(qtr == 4, abs(home_score - away_score) <= 5& grepl("makes", text, ignore.case = TRUE)& season_type == 2) %>%
  mutate(assisted = grepl("assist", text, ignore.case = TRUE)) %>%
  group_by(team_id) %>%
  summarize(assist_rate = mean(assisted), .groups = "drop")

#add in team names
clutch_assist_rate <- clutch_assist_rate %>%
  left_join(
    wnba_team_box %>%
      distinct(team_id, team_abbreviation),
    by = "team_id"
  ) %>%
  select(team_abbreviation, everything(), -team_id) %>%
  arrange(desc(assist_rate))


clutch_assist_rate

#################################################
#clutch efg%
clutch_efg <- wnba_pbp_with_winner %>%
  filter(qtr == 4, abs(home_score - away_score) <= 5, shooting_play == TRUE, season_type == 2) %>%
  mutate(
    is_three = grepl("three point", text, ignore.case = TRUE),
    is_made = grepl("makes", text, ignore.case = TRUE),
    points = case_when(
      is_made & is_three ~ 3,
      is_made & !is_three ~ 2,
      TRUE ~ 0
    )
  ) %>%
  group_by(team_id) %>%
  summarize(
    efg_pct = sum(points) / (2 * n()),
    .groups = "drop"
  )

#add in team names
clutch_efg <- clutch_efg %>%
  left_join(
    wnba_team_box %>%
      distinct(team_id, team_abbreviation),
    by = "team_id"
  ) %>%
  select(team_abbreviation, everything(), -team_id) %>%
  arrange(desc(efg_pct))

clutch_efg


#####################################################
#subway lines

# Create data
transit_data <- data.frame(
  city = c("New York City", "Seattle"),
  lines = c(28, 1)
)


ggplot(transit_data, aes(x = reorder(city, -lines), y = lines, fill = city)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = lines), vjust = -0, size = 8, fontface = "bold") +
  scale_fill_manual(values = c("New York City" = "#0039A6", "Seattle" = "#00843D")) +
  labs(
    title = "Sonia Raman's train options ",
    subtitle = "hope this helps! the 2 line isn't in seattle",
    x = NULL,
    y = "Number of Rail Lines",
    caption = "NYC Subway vs Seattle Link Light Rail (within city limits)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 11)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), 
                     breaks = seq(0, 30, 5))
