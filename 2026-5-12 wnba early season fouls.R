library(wehoop)
library(dplyr)
library(ggplot2)

team_box <- load_wnba_team_box(seasons = c(2016:2026))

fouls_count <- team_box %>%
  filter(season_type == 2) %>%
  group_by(season) %>%         
  summarise(
    tot_games = n(),                    
    tot_fouls = sum(fouls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(fouls_per_game = tot_fouls / tot_games)

fouls_count$foul_per_game <- fouls_count$fouls_per_game*2
fouls_count$tot_games <- fouls_count$tot_games/2
fouls_count %>% select(season, tot_games, tot_fouls, foul_per_game)



fouls_first11 <- team_box %>%
  filter(season_type == 2) %>%
  group_by(season, game_id) %>%              
  summarise(
    game_fouls = sum(fouls, na.rm = TRUE),    
    .groups = "drop"
  ) %>%
  group_by(season) %>%
  arrange(season, game_id) %>%                
  mutate(game_number = row_number()) %>%     
  filter(game_number <= 11) %>%            
  summarise(
    games_11 = n(),                     
    fouls_11 = sum(game_fouls, na.rm = TRUE),
    fouls_per_game_11 = fouls_11 / games_11,
    .groups = "drop"
  )

fouls_first11 %>% select(season, fouls_11, fouls_per_game_11)

first11_2026 <- fouls_first11 %>%
  filter(season == 2026)

first11_prior <- fouls_first11 %>%
  filter(season < 2026) %>%
  summarise(
    mean_fouls_per_game_11 = mean(fouls_per_game_11),
    sd_fouls_per_game_11   = sd(fouls_per_game_11)
  )

first11_2026
first11_prior


avg_11 <- first11_prior$mean_fouls_per_game_11

ggplot(fouls_first11, aes(x = season, y = fouls_per_game_11)) +
  geom_line(color = "#ffc85a") +
  geom_point(color = "#ff7c01", size = 2) +
  geom_hline(
    data = first11_prior,
    aes(yintercept = mean_fouls_per_game_11),
    linetype = "dashed", color = "#c4d1fe"
  ) +
  geom_text(aes(label = sprintf("%.1f", fouls_per_game_11)), vjust = -0.7, size=5) +
  geom_point(
    data = subset(fouls_first11, season == 2026),
    color = "#ff4b8b",
    shape = 8,
    size = 5
  ) +
  scale_x_continuous(breaks = 2016:2026) +
  scale_y_continuous(limits = c(0, 50)) + 
  annotate(
    "label",
    x = 2018.2,
    y = avg_11 - 15,   # move text lower
    label = paste0("2016-25 avg through 11 games: ", round(avg_11, 1)),
    color = "grey30",
    fill = "#FEEA80",
    hjust = 0,
    size = 5
  ) +
  annotate(
    "segment",
    x = 2018,
    xend = 2015.5,
    y = avg_11 - 14.5,
    yend = avg_11 - 1.4,
    color = "#f87e7e",
    arrow = arrow(length = unit(0.15, "cm"))
  ) +
  labs(
    title = "WNBA fouls per game through the first 11 games",
    subtitle = "2026 has about 5 fouls per game more than a typical season start",
    x = "season",
    y = "fouls per game (league-wide)",
    caption = "data: wehoop | chart: @wnbadata"
  ) +
  theme_minimal()+ 
  theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )