# valkyries vs the 2013-14 spurs
# remaking ian levy's charts from chasing perfection (andy glockner) for the 2026 wnba
# levy used box plus-minus, which isn't published for the wnba, so this uses regular plus-minus

library(tidyverse)
library(wehoop)

VALKS <- "#6F4BB2"
DREAM <- "#E31837"
GRAY  <- "#8A8A86"
INK   <- "#010101"

# drop the commissioner's cup final and all-star game
drop_games <- load_wnba_schedule(seasons = 2026) |>
  filter(type_abbreviation %in% c("CC", "ALLSTAR")) |>
  pull(game_id)

box <- load_wnba_player_box(seasons = 2026) |>
  filter(season_type == 2, !game_id %in% drop_games, !is.na(plus_minus)) |>
  mutate(plus_minus = as.numeric(plus_minus),
         minutes = replace_na(minutes, 0))

# check: player plus-minus should add up to 5x the team's margin
margins <- box |>
  distinct(game_id, team = team_abbreviation, team_score, opponent_team_score) |>
  group_by(team) |>
  summarise(games = n(), margin = sum(team_score - opponent_team_score))

stopifnot(nrow(margins) == 15, all(margins$games == 44),
          all(box |> group_by(team = team_abbreviation) |> summarise(pm = sum(plus_minus)) |>
                left_join(margins, by = "team") |> mutate(ok = pm == 5 * margin) |> pull(ok)))

# roster spot = rank in minutes played for that team
players <- box |>
  group_by(team = team_abbreviation, player = athlete_display_name) |>
  summarise(games = n(), minutes = sum(minutes), plus_minus = sum(plus_minus), .groups = "drop") |>
  filter(minutes > 0) |>
  mutate(pm40 = plus_minus / minutes * 40) |>
  group_by(team) |>
  arrange(desc(minutes), .by_group = TRUE) |>
  mutate(spot = row_number(), share = minutes / sum(minutes)) |>
  ungroup()

average <- players |>
  filter(spot <= 12) |>
  group_by(spot) |>
  summarise(teams = n(), pm40 = weighted.mean(pm40, minutes), minutes = mean(minutes), share = mean(share))

stopifnot(all(average$teams == 15))

valks <- players |> filter(team == "GS", spot <= 12)
dream <- players |> filter(team == "ATL", spot <= 12)

# what if each team split its minutes like the league average?
levy <- players |>
  filter(spot <= 12) |>
  left_join(select(average, spot, avg_share = share), by = "spot") |>
  group_by(team) |>
  summarise(actual       = sum(share / sum(share) * pm40),
            with_average = sum(avg_share / sum(avg_share) * pm40)) |>
  mutate(change = with_average - actual)

stopifnot(abs(levy$change[levy$team == "GS"]) < 0.5)

# charts

theme_wnbadata <- function() {
  theme_minimal(base_size = 15) +
    theme(plot.title.position = "plot", plot.caption.position = "plot",
          plot.title    = element_text(face = "bold", color = INK, size = 19),
          plot.subtitle = element_text(color = GRAY, size = 12, margin = margin(b = 14)),
          plot.caption  = element_text(color = GRAY, size = 11, hjust = 0, margin = margin(t = 14)),
          axis.text     = element_text(color = INK, size = 12),
          axis.title    = element_text(color = GRAY, size = 12),
          panel.grid.minor = element_blank(),
          plot.margin   = margin(18, 18, 14, 10))
}
CAPTION <- "roster spot = rank in minutes played for that team  |  chart: @wnbadata\ndata: wehoop, 2026 regular season"

both <- bind_rows(transmute(average, who = "wnba average", spot, minutes, pm40),
                  transmute(valks, who = "valkyries", spot, minutes, pm40))
both_dream <- bind_rows(transmute(average, who = "wnba average", spot, minutes, pm40),
                        transmute(dream, who = "dream", spot, minutes, pm40))

levy_lines <- function(y, labels, d = both, limits = NULL) {
  ggplot(d, aes(x = spot, y = {{ y }}, color = who)) +
    geom_line(linewidth = 1.6) +
    geom_point(size = 3.5) +
    geom_text(data = filter(d, spot == 12), aes(label = who), hjust = -0.15, size = 4.5,
              fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = c(valkyries = VALKS, dream = DREAM, `wnba average` = GRAY), guide = "none") +
    scale_x_continuous(breaks = 1:12, expand = expansion(add = c(0.4, 3.2))) +
    scale_y_continuous(labels = labels, limits = limits) +
    coord_cartesian(clip = "off") +
    theme_wnbadata() +
    theme(panel.grid.major.x = element_blank())
}

MINUTES_LIMITS <- c(0, 1500)

p_minutes <- levy_lines(minutes, scales::comma, limits = MINUTES_LIMITS) +
  labs(title = "valkyries minutes by roster spot",
       subtitle = "total minutes, 2026 regular season",
       caption = CAPTION, x = "roster spot", y = "minutes")

p_plus_minus <- levy_lines(pm40, \(x) if_else(x > 0, paste0("+", x), as.character(x))) +
  geom_hline(yintercept = 0, color = INK, linewidth = 0.4) +
  labs(title = "valkyries plus-minus by roster spot",
       subtitle = "per 40 minutes on the court, 2026 regular season",
       caption = paste0("wnba average = every team's player at that spot, combined\n", CAPTION), x = "roster spot", y = NULL)

p_minutes_dream <- levy_lines(minutes, scales::comma, d = both_dream, limits = MINUTES_LIMITS) +
  labs(title = "dream minutes by roster spot",
       subtitle = "total minutes, 2026 regular season",
       caption = CAPTION, x = "roster spot", y = "minutes")

p_minutes
p_plus_minus
p_minutes_dream
