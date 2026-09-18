# charts for the washington mystics "weird 3/15" video
#
# records, margins, close games and true shooting all compute live from wehoop (espn)
# and were cross checked against her hoop stats.
#
# AGES ARE HARDCODED from her hoop stats. wehoop's box scores carry no birthdate, so
# the youngest team claim cannot be computed from wehoop at all. figures are minutes
# weighted as of 9/5.
#
# VERIFIED 9/5 that the weighting attributes minutes to the correct team. 16 players
# changed teams midseason, so this matters. two checks:
#   - every team totals 8000-8150 minutes over 40 games, ie 200-204 player minutes per
#     game, which is exactly right for a 40 minute game with 5 on the floor. any double
#     counting would roughly double these
#   - washington totals 8150, matching basketball reference exactly, and betnijah
#     laney-hamilton's 146 washington minutes match too. she played 17 more games for
#     new york that are correctly excluded here
#
# the earlier bad number (18 washington games for laney-hamilton instead of 9) came from
# a query built on a season level roster table, which has duplicate rows per player and doubled
# her games. for per team work, always attribute players to teams game by game.
#
# two mandatory wehoop filters or the season is wrong:
#   - the commissioner's cup final sits inside season_type == 2 but does not count
#   - the all star rosters appear as their own "teams"
#
# axis rule: anything with a length starts at zero. the win/margin chart is a scatter,
# where both axes encode position, so it does not need a zero baseline.

library(wehoop)
library(dplyr)
library(ggplot2)

ALLSTAR   <- c("Team Spoon", "Team Coop")
CUP_FINAL <- as.Date("2026-06-30")
WAS       <- "Washington"

HI     <- "#E03A3E"   # mystics red. swap here if you want a different hex
GRAY    <- "#8A8A86"  # everyone else
INK     <- "#15171D"
CREDIT  <- "2026 season through the fiba break  ·  data: her hoop stats and wehoop  |  chart: @wnbadata"

team_box <- load_wnba_team_box(seasons = 2026) |>
  filter(season_type == 2, !team_location %in% ALLSTAR, game_date != CUP_FINAL)

player_box <- load_wnba_player_box(seasons = 2026) |>
  filter(season_type == 2, !team_location %in% ALLSTAR, game_date != CUP_FINAL)

# minutes weighted team age, her hoop stats, as of 9/5
team_age <- tibble::tribble(
  ~team,          ~age,
  "Washington",   24.30,
  "Connecticut",  25.27,
  "Seattle",      25.48,
  "Portland",     26.44,
  "Atlanta",      27.10,
  "Indiana",      27.62,
  "Dallas",       27.85,
  "Toronto",      29.15,
  "Golden State", 29.38,
  "Chicago",      29.63,
  "New York",     30.16,
  "Minnesota",    30.31,
  "Las Vegas",    30.32,
  "Los Angeles",  30.42,
  "Phoenix",      31.39
)

# washington rotation, 10 or more games, her hoop stats ages
mystics_roster <- tibble::tribble(
  ~player,              ~age,  ~mpg,
  "Sonia Citron",        22.9, 33.2,
  "Shakira Austin",      26.1, 29.6,
  "Kiki Iriafen",        23.0, 28.4,
  "Michaela Onyenwere",  27.1, 25.2,
  "Georgia Amoore",      25.4, 23.0,
  "Cotie McMahon",       22.3, 19.7,
  "Lauren Betts",        22.9, 17.1,
  "Alicia Florez",       22.4, 12.1,
  "Cassandre Prosper",   21.2, 10.0,
  "Angela Dugalic",      24.7,  9.4,
  "Lucy Olsen",          23.3,  7.3,
  "Rori Harmon",         23.6,  5.8
)

base_theme <- function() {
  theme_minimal(base_size = 15) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_text(face = "bold", colour = INK, size = 19),
      plot.subtitle = element_text(colour = GRAY, size = 13, margin = margin(b = 14)),
      plot.caption  = element_text(colour = GRAY, size = 11, hjust = 0, margin = margin(t = 14)),
      axis.text     = element_text(colour = INK, size = 13),
      axis.title    = element_text(colour = GRAY, size = 12),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#E3E3E0", linewidth = 0.3),
      plot.margin   = margin(18, 18, 14, 10)
    )
}

lollipop <- function(df, value, label_fmt, title, subtitle, ascending = TRUE) {
  d <- df |> mutate(v = {{ value }}, is_was = team == WAS, lab = sprintf(label_fmt, v))
  ggplot(d, aes(x = v, y = reorder(team, if (ascending) -v else v))) +
    geom_segment(aes(x = 0, xend = v, yend = team, colour = is_was),
                 linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
    geom_point(aes(colour = is_was, size = is_was)) +
    geom_text(aes(label = lab, colour = is_was), hjust = -0.4, size = 4.2, show.legend = FALSE) +
    scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
    scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.18))) +
    labs(title = title, subtitle = subtitle, caption = CREDIT, x = NULL, y = NULL) +
    base_theme()
}

# 1. THE LEAD. youngest team in the league, a full year clear
p_age <- lollipop(team_age, age, "%.1f",
                  "washington is the youngest team in the wnba",
                  "average age weighted by minutes played")

# 2. the roster behind it. every rotation player 27 or younger
p_roster <- ggplot(mystics_roster, aes(x = age, y = reorder(player, -age))) +
  geom_segment(aes(x = 0, xend = age, yend = player), colour = HI, linewidth = 1.6, alpha = 0.55) +
  geom_point(colour = HI, size = 4) +
  geom_text(aes(label = sprintf("%.1f", age)), hjust = -0.4, size = 4.2, colour = INK) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
  labs(title = "every player in washington's rotation is 27 or younger",
       subtitle = "age as of september 5, players with 10 or more games",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# 3. THE TWIST. a scatter, so no zero baseline needed. both axes encode position and
# the zero margin line is drawn because it is the meaningful reference here.
record <- team_box |>
  mutate(m = team_score - opponent_team_score) |>
  group_by(team = team_location) |>
  summarise(wins = sum(m > 0), margin = mean(m), .groups = "drop")

# fitted league trend, then shade above it as lucky and below it as unlucky.
# the ribbons are decoration for the eye, the dots are still the data.
fit  <- lm(wins ~ margin, data = record)
band <- data.frame(margin = seq(min(record$margin) - 1.2, max(record$margin) + 1.2, length.out = 200))
band$pred <- predict(fit, band)
Y_LO <- 4
Y_HI <- 35

p_luck <- ggplot(record, aes(x = margin, y = wins)) +
  geom_ribbon(data = band, inherit.aes = FALSE,
              aes(x = margin, ymin = pred, ymax = Y_HI), fill = HI, alpha = 0.07) +
  geom_ribbon(data = band, inherit.aes = FALSE,
              aes(x = margin, ymin = Y_LO, ymax = pred), fill = GRAY, alpha = 0.09) +
  geom_line(data = band, inherit.aes = FALSE, aes(x = margin, y = pred),
            colour = "#C9C8C3", linewidth = 0.6, linetype = "22") +
  annotate("text", x = min(record$margin) - 0.6, y = Y_HI - 1.5, label = "lucky",
           hjust = 0, colour = HI, size = 5.4, fontface = "bold", alpha = 0.75) +
  annotate("text", x = max(record$margin) + 0.6, y = Y_LO + 1.2, label = "unlucky",
           hjust = 1, colour = GRAY, size = 5.4, fontface = "bold", alpha = 0.85) +
  geom_point(aes(colour = team == WAS, size = team == WAS)) +
  ggrepel::geom_text_repel(aes(label = team, colour = team == WAS),
                           size = 4, seed = 1, show.legend = FALSE, max.overlaps = 20) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3, `TRUE` = 5.4), guide = "none") +
  scale_y_continuous(limits = c(Y_LO, Y_HI)) +
  labs(title = "washington wins far more than their scoring says they should",
       subtitle = "wins against average point differential, dashed line is the league trend",
       caption = CREDIT, x = "point differential per game", y = "wins") +
  base_theme() +
  theme(panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 4. the mechanism. close game record, washington 12-5 in 17
close_games <- team_box |>
  mutate(m = team_score - opponent_team_score) |>
  group_by(team = team_location) |>
  summarise(close_w = sum(abs(m) <= 5 & m > 0),
            close_l = sum(abs(m) <= 5 & m < 0), .groups = "drop") |>
  tidyr::pivot_longer(c(close_w, close_l), names_to = "result", values_to = "n") |>
  mutate(result = factor(if_else(result == "close_w", "won", "lost"), levels = c("lost", "won")))

p_close <- ggplot(close_games, aes(x = n, y = reorder(team, n), fill = result)) +
  geom_col(width = 0.66) +
  scale_fill_manual(values = c(won = HI, lost = "#D6D5D0"), name = NULL) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
  labs(title = "mystics have the most close games and win almost all of them",
       subtitle = "games decided by <= 5 points",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top")

# 5. the player. citron is the most efficient scorer on the roster
citron <- player_box |>
  filter(team_location == WAS, !did_not_play) |>
  group_by(player = athlete_display_name) |>
  summarise(gp = n(),
            ts = 100 * sum(points) /
              (2 * (sum(field_goals_attempted) + 0.44 * sum(free_throws_attempted, na.rm = TRUE))),
            .groups = "drop") |>
  filter(gp >= 15)

p_citron <- ggplot(citron, aes(x = ts, y = reorder(player, ts))) +
  geom_segment(aes(x = 0, xend = ts, yend = player, colour = player == "Sonia Citron"),
               linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
  geom_point(aes(colour = player == "Sonia Citron", size = player == "Sonia Citron")) +
  geom_text(aes(label = sprintf("%.1f", ts), colour = player == "Sonia Citron"),
            hjust = -0.4, size = 4.2, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
  labs(title = "sonia citron is 22 and the most efficient scorer washington has",
       subtitle = "true shooting percentage, 15 or more games",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# render one at a time in positron. for 9:16 size the plot pane tall and narrow.
p_age
p_roster
p_luck
p_close
p_citron
