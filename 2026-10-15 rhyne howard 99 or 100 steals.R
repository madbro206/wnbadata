# charts for the rhyne howard 100 steals video
#
# all numbers are hardcoded from her hoop stats (2026 regular season). wehoop isn't
# used here because the whole video is about a single steal, and the source that
# matches the official game book is the one that counts.
#   - howard: 100 steals in 43 games. she was rostered for 44 and sat out 5/17 vs chi.
#     basketball reference has 99, because it scores the 6/18 at ind play at q1 9:38
#     as a howard offensive rebound and not a billings turnover.
#   - record: teresa weatherspoon, 100 in 1998.
#   - atlanta's 400 team steals are the most by any team in a season. that's a total:
#     2009 indiana averaged more per game (11.0 vs 9.1), so don't say "per game".
#
# axis rule: anything with a length starts at zero.

library(dplyr)
library(ggplot2)

HI     <- "#C8102E"   # atlanta
GRAY   <- "#8A8A86"   # everyone else
INK    <- "#15171D"
CREDIT <- "2026 regular season  ·  data: her hoop stats  |  chart: @wnbadata"

base_theme <- function() {
  theme_minimal(base_size = 15) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_text(face = "bold", colour = INK, size = 19),
      plot.subtitle = element_text(colour = GRAY, size = 13, margin = margin(b = 14)),
      plot.caption  = element_text(colour = GRAY, size = 11, hjust = 0, margin = margin(t = 14)),
      axis.text     = element_text(colour = INK, size = 13),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#E3E3E0", linewidth = 0.3),
      plot.margin   = margin(18, 18, 14, 10)
    )
}

# 1. CUMULATIVE STEALS BY GAME PLAYED, which reaches 100 in the final game (9/23 at nyl)
howard_games <- tibble::tribble(
  ~date,        ~opp,  ~stl,
  "2026-05-09", "MIN", 3,
  "2026-05-12", "DAL", 1,
  "2026-05-22", "DAL", 4,
  "2026-05-24", "PHO", 1,
  "2026-05-27", "MIN", 1,
  "2026-05-29", "POR", 6,
  "2026-06-02", "CON", 3,
  "2026-06-04", "IND", 3,
  "2026-06-06", "WAS", 6,
  "2026-06-09", "CHI", 3,
  "2026-06-11", "NYL", 4,
  "2026-06-14", "TOR", 1,
  "2026-06-18", "IND", 3,   # the disputed game. basketball reference has 2
  "2026-06-20", "IND", 1,
  "2026-06-22", "TOR", 0,
  "2026-06-24", "GSV", 1,
  "2026-06-26", "GSV", 3,
  "2026-06-27", "SEA", 1,
  "2026-07-02", "WAS", 5,
  "2026-07-04", "GSV", 0,
  "2026-07-09", "SEA", 4,
  "2026-07-11", "POR", 4,
  "2026-07-13", "LAS", 2,
  "2026-07-17", "TOR", 1,
  "2026-07-19", "CHI", 0,
  "2026-07-29", "DAL", 3,
  "2026-07-31", "SEA", 4,
  "2026-08-03", "LVA", 1,
  "2026-08-05", "PHO", 2,
  "2026-08-07", "WAS", 0,
  "2026-08-10", "TOR", 3,
  "2026-08-13", "CON", 3,
  "2026-08-16", "IND", 1,
  "2026-08-18", "LVA", 4,
  "2026-08-20", "LAS", 3,
  "2026-08-22", "PHO", 2,
  "2026-08-24", "LAS", 1,
  "2026-08-28", "POR", 2,
  "2026-08-30", "MIN", 2,
  "2026-09-17", "CON", 4,
  "2026-09-19", "CHI", 1,
  "2026-09-21", "NYL", 1,
  "2026-09-23", "NYL", 2
) |>
  mutate(game = row_number(), total = cumsum(stl))

stopifnot(nrow(howard_games) == 43, max(howard_games$total) == 100)

# a zero row so the line starts at the origin
howard_line <- bind_rows(tibble(game = 0, total = 0), select(howard_games, game, total))
disputed    <- filter(howard_games, date == "2026-06-18")
last_game   <- slice_tail(howard_games, n = 1)

p_cumulative <- ggplot(howard_line, aes(x = game, y = total)) +
  geom_hline(yintercept = 100, colour = GRAY, linewidth = 0.5, linetype = "dashed") +
  annotate("text", x = 1, y = 100, label = "wnba record: 100, teresa weatherspoon (1998)",
           hjust = 0, vjust = -0.7, colour = GRAY, size = 4.2) +
  geom_step(colour = HI, linewidth = 1.3) +
  # optional: delete these two layers if the chart shouldn't point at the 6/18 game
  geom_point(data = disputed, colour = INK, size = 3) +
  annotate("text", x = disputed$game + 1, y = disputed$total,
           label = "6/18 at indiana", hjust = 0, vjust = 1.6, colour = INK, size = 4) +
  geom_point(data = last_game, colour = HI, size = 4.5) +
  annotate("text", x = last_game$game, y = last_game$total, label = "100",
           hjust = 0.5, vjust = 2.2, colour = HI, fontface = "bold", size = 5) +
  scale_x_continuous(limits = c(0, 44), breaks = c(1, 10, 20, 30, 43),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 25),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "rhyne howard got to 100 steals in her last game",
       subtitle = "cumulative steals by game played",
       caption = CREDIT, x = "game", y = NULL) +
  base_theme() +
  theme(panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3),
        axis.title.x = element_text(colour = GRAY, size = 12, margin = margin(t = 8)))


# 2. TEAM STEALS. atlanta's 400 is the most by any team in a season (total, not per game)
team_steals <- tibble::tribble(
  ~team,          ~stl,
  "Atlanta",      400,
  "Minnesota",    384,
  "Connecticut",  346,
  "Seattle",      344,
  "Golden State", 344,
  "Los Angeles",  340,
  "Portland",     335,
  "Phoenix",      328,
  "Las Vegas",    318,
  "Indiana",      310,
  "Chicago",      308,
  "Toronto",      305,
  "Dallas",       300,
  "New York",     267,
  "Washington",   244
)

p_teams <- ggplot(team_steals, aes(x = stl, y = reorder(team, stl))) +
  geom_segment(aes(x = 0, xend = stl, yend = team, colour = team == "Atlanta"),
               linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
  geom_point(aes(colour = team == "Atlanta", size = team == "Atlanta")) +
  geom_text(aes(label = stl, colour = team == "Atlanta"),
            hjust = -0.45, size = 4.2, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "the dream stole the ball 400 times this season",
       subtitle = "total team steals",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()


# 3. THE LAST STRETCH, zoomed into her final 10 games so the 98 -> 100 step is visible.
# the y axis starts at 75 on purpose. it's a running total line, not a bar, so the
# zero rule doesn't apply. both of the last 2 games were at new york (9/21 and 9/23)
stretch     <- filter(howard_line, game >= 33)
second_last <- filter(howard_games, game == 42)

p_last_stretch <- ggplot(stretch, aes(x = game, y = total)) +
  geom_hline(yintercept = 100, colour = GRAY, linewidth = 0.5, linetype = "dashed") +
  annotate("text", x = 33.2, y = 100, label = "wnba record: 100",
           hjust = 0, vjust = -0.7, colour = GRAY, size = 4.2) +
  geom_step(colour = HI, linewidth = 1.3) +
  geom_point(data = second_last, colour = INK, size = 4) +
  annotate("text", x = second_last$game, y = second_last$total, label = "98",
           hjust = 1.6, vjust = 0.4, colour = INK, fontface = "bold", size = 5) +
  geom_point(data = last_game, colour = HI, size = 5) +
  annotate("text", x = last_game$game, y = last_game$total, label = "100",
           hjust = 0.5, vjust = -0.9, colour = HI, fontface = "bold", size = 5) +
  scale_x_continuous(limits = c(33, 43.4), breaks = 34:43,
                     expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(limits = c(75, 104), breaks = seq(75, 100, 5),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "rhyne howard had 98 steals going into her last game",
       subtitle = "cumulative steals, her last 10 games",
       caption = CREDIT, x = "game", y = NULL) +
  base_theme() +
  theme(panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3),
        axis.title.x = element_text(colour = GRAY, size = 12, margin = margin(t = 8)))


p_cumulative
p_teams
p_last_stretch
