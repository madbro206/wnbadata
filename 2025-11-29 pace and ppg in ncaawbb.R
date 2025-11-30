library(dplyr)
library(stringr)
library(ggplot2)
library(wehoop)
library(lubridate)

df <- read.csv("/Users/maddy/Desktop/wehoop/pace_stats.csv")

df <- df %>%
  mutate(ppg = points / g)

season_summary <- df %>%
  group_by(season) %>%
  summarise(
    n_teams   = n(),
    avg_ppg   = mean(ppg,  na.rm = TRUE),
    avg_pace  = mean(pace, na.rm = TRUE),
    med_ppg   = median(ppg,  na.rm = TRUE),
    med_pace  = median(pace, na.rm = TRUE)
  )
season_summary



wbb_team_box <- wehoop::load_wbb_team_box(season=c(2020:2025))

wbb_team_box_flagged <- wbb_team_box %>%
  mutate(
    period = if_else(
      game_date < make_date(season, 1, 1),  # before Jan 1 of that season year
      "pre_new_year",
      "post_new_year"
    )
  )

team_ppg_split <- wbb_team_box_flagged %>%
  group_by(season, team_id, period) %>%
  summarise(
    g   = n(),
    pts = sum(team_score, na.rm = TRUE),
    ppg = pts / g,
    .groups = "drop"
  )
  
ppg_split_summary <- team_ppg_split %>%
  group_by(season, period) %>%
  summarise(
    n_teams = n(),
    avg_ppg = mean(ppg, na.rm = TRUE),
    med_ppg = median(ppg, na.rm = TRUE),
    .groups = "drop"
  )

ppg_split_summary

df_2526 <- df %>% filter(season %in% c(2025, 2026))

t.test(ppg ~ season, data = df_2526)   # scoring change
t.test(pace ~ season, data = df_2526)  # possessions change


season_summary <- season_summary %>%
  mutate(season = as.integer(season))

gg_ppg <- ggplot(season_summary, aes(x = season, y = avg_ppg)) +
  geom_line(color = "#1f77b4") +
  geom_point(color = "#1f77b4", size = 2) +
  geom_text(
    aes(label = round(avg_ppg, 1)),
    vjust = -0.7,
    size = 3
  ) +
  scale_x_continuous(breaks = season_summary$season) +
  labs(
    title = "Average Team Points Per Game by Season",
    x = "Season",
    y = "Average PPG"
  ) +
  theme_minimal()+
    coord_cartesian(ylim = c(62, 70))

gg_ppg

gg_pace <- ggplot(season_summary, aes(x = season, y = avg_pace)) +
  geom_line(color = "#d62728") +
  geom_point(color = "#d62728", size = 2) +
  geom_text(
    aes(label = round(avg_pace, 1)),
    vjust = -0.7,
    size = 3
  ) +
  scale_x_continuous(breaks = season_summary$season) +
  labs(
    title = "Average Team Pace by Season",
    x = "Season",
    y = "Average Possessions per 40 Minutes"
  ) +
  theme_minimal()+
    coord_cartesian(ylim = c(68, 75))

gg_pace
