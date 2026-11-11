# charts for the unrivaled elam ending video with the realest (2026-11-10)
#
# wnba numbers are hardcoded from wnba_endings.R (wehoop pbp, regular season + playoffs, all-star excluded).
#   - fouling: a non-shooting foul by the trailing team in the last 60 seconds of the final period, 2018-2026
#   - slow: real time (wallclock) from the 2:00 mark of the 4th to the end of regulation, 2026 regulation games
#   - game winners: the winner took the lead for good on a field goal in the last 5 seconds, 2026
# unrivaled numbers are hardcoded from unrivaled.basketball (stats page and game recaps, november 2026).
#   - the 9 minute elam quarter is the average from the feb 2025 "fourth quarter actual length" video (early 2025 season,
#     real time, range 4 to 17). unrivaled.basketball play by play has no timestamps, so there is no 2026 version
#
# axis rule: bars start at zero. the waffles are counts, one square per game.

library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)

ACCENT <- "#6A3FB5"   # the thing the beat is about
GRAY   <- "#8A8A86"   # text that should sit back
LIGHT  <- "#D9D9D4"   # every other game
INK    <- "#15171D"
CREDIT_WNBA <- "data: wehoop  |  chart: @wnbadata"

base_theme <- function() {
  theme_minimal(base_size = 15) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_text(face = "bold", color = INK, size = 19),
      plot.subtitle = element_text(color = GRAY, size = 13, margin = margin(b = 14)),
      plot.caption  = element_text(color = GRAY, size = 11, hjust = 0, margin = margin(t = 14)),
      axis.text     = element_text(color = INK, size = 13),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#E3E3E0", linewidth = 0.3),
      plot.margin   = margin(18, 18, 14, 10)
    )
}

# one square per game, highlighted squares first so they read as a block in the top-left
waffle <- function(total, highlighted, ncol, title, subtitle, hl_label, rest_label, caption) {
  squares <- tibble(i = seq_len(total) - 1) |>
    mutate(col = i %% ncol, row = i %/% ncol,
           group = if_else(i < highlighted, hl_label, rest_label),
           group = factor(group, levels = c(hl_label, rest_label)))
  ggplot(squares, aes(x = col, y = -row, fill = group)) +
    geom_tile(width = 0.82, height = 0.82) +
    scale_fill_manual(values = setNames(c(ACCENT, LIGHT), c(hl_label, rest_label)), name = NULL) +
    coord_equal() +
    labs(title = title, subtitle = subtitle, caption = caption, x = NULL, y = NULL) +
    base_theme() +
    theme(axis.text = element_blank(), panel.grid.major.x = element_blank(),
          legend.position = "top", legend.justification = "left",
          legend.text = element_text(color = INK, size = 13))
}


# 0. WHAT UNRIVALED IS (script: "3 on 3, on a mini full court, with an 18 second shot clock")
# 72 by 49.2 is what unrivaled bills the court as, wnba is the standard 94 by 50. both drawn to scale
courts <- tibble::tribble(
  ~league,     ~length, ~width,
  "WNBA",       94,      50,
  "unrivaled",  72,      49.2
)

p_court <- ggplot() +
  geom_rect(data = filter(courts, league == "WNBA"),
            aes(xmin = -length / 2, xmax = length / 2, ymin = -width / 2, ymax = width / 2),
            fill = LIGHT) +
  geom_rect(data = filter(courts, league == "unrivaled"),
            aes(xmin = -length / 2, xmax = length / 2, ymin = -width / 2, ymax = width / 2),
            fill = ACCENT) +
  annotate("text", x = 0, y = 0, label = "unrivaled\n72 by 49.2 feet", color = "white",
           size = 6, fontface = "bold", lineheight = 0.95) +
  annotate("text", x = 0, y = 29, label = "WNBA: 94 by 50 feet", color = INK, size = 5) +
  coord_equal(ylim = c(-27, 32)) +
  labs(title = "court size, drawn to scale",
       subtitle = "unrivaled: 3 on 3, 18 second shot clock\nWNBA: 5 on 5, 24 second shot clock",
       x = NULL, y = NULL, caption = "chart: @wnbadata") +
  base_theme() +
  theme(axis.text = element_blank(), panel.grid.major.x = element_blank())


# 0b. HOW THE ELAM ENDING WORKS (script: "after three quarters / the clock turns off / whoever is winning gets 11
#     added to their score, and that's the target / first team to hit the target score wins")
# vinyl @ phantom, 2/9/26, one of the 4 jersey games. scores are every scoring play of the 4th from the
# unrivaled.basketball play by play. the elam clock counts up. plum scored 13 of phantom's 15 in the 4th
elam_game <- tibble::tribble(
  ~clock, ~Vinyl, ~Phantom,
    0,    65,     61,
    9,    65,     63,
   32,    65,     65,
   39,    67,     65,
   58,    69,     65,
   72,    69,     68,
   87,    71,     68,
  109,    71,     71,
  132,    73,     71,
  215,    73,     74,
  235,    73,     76
)
elam_target <- max(elam_game$Vinyl[1], elam_game$Phantom[1]) + 11

stopifnot(elam_target == 76, max(elam_game$Phantom) == elam_target, max(elam_game$Vinyl) == 73)

# the explainer: a race to a finish line, one chart per line of the script. uses the real scores from the
# game above but keeps the game out of the title, because the script never mentions it. the team that was
# losing after 3 wins in step 3, which also shows the rule leaves room for a comeback
elam_bars <- tibble::tribble(
  ~team,                        ~pos, ~after_3, ~final,
  "winning after\n3 quarters",   2,    65,       73,
  "losing after\n3 quarters",    1,    61,       76
)
CREDIT_ELAM <- "example from a 2026 unrivaled game  |  data: unrivaled.basketball  |  chart: @wnbadata"

elam_step <- function(step) {
  bars <- elam_bars |> mutate(score = if (step == 3) final else after_3)
  p <- ggplot(bars) +
    geom_rect(aes(xmin = 0, xmax = score, ymin = pos - 0.3, ymax = pos + 0.3, fill = team)) +
    geom_text(aes(x = score - 1.5, y = pos, label = score), hjust = 1, color = "white",
              size = 8, fontface = "bold")
  if (step == 2) {
    p <- p +
      annotate("rect", xmin = 65, xmax = elam_target, ymin = 1.7, ymax = 2.3, fill = NA,
               color = INK, linetype = "22", linewidth = 0.7) +
      annotate("text", x = 70.5, y = 2, label = "+11", color = INK, size = 7, fontface = "bold")
  }
  if (step >= 2) {
    p <- p +
      annotate("segment", x = elam_target, xend = elam_target, y = 0.55, yend = 2.5,
               color = INK, linewidth = 1.1) +
      annotate("text", x = elam_target, y = 2.62, label = "target: 76", color = INK, size = 6,
               fontface = "bold")
  }
  p +
    scale_fill_manual(values = setNames(c(GRAY, ACCENT), elam_bars$team), guide = "none") +
    scale_x_continuous(limits = c(0, 86), expand = c(0, 0)) +
    scale_y_continuous(breaks = elam_bars$pos, labels = elam_bars$team, limits = c(0.5, 2.8)) +
    labs(title = "how the elam ending works",
         subtitle = c("after 3 quarters, the clock turns off",
                      "add 11 to the leading score, and that's the target for both teams",
                      "no clock to run out, the game ends when someone hits 76")[step],
         x = NULL, y = NULL, caption = CREDIT_ELAM) +
    base_theme() +
    theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank(),
          plot.subtitle = element_text(color = INK, size = 15, margin = margin(b = 14)))
}

p_elam_1 <- elam_step(1)
p_elam_2 <- elam_step(2)
p_elam_3 <- elam_step(3)


# 1. THE FOULING (script: "in 46% of them, the losing team started fouling in the last minute /
#    and the team doing the fouling won just 17 times out of a thousand")
# from fouling in wnba_endings.R
fouling <- tibble::tribble(
  ~season, ~games, ~foul_games, ~fouler_won,
  2018,    218,     77,          1,
  2019,    218,    109,          2,
  2020,    146,     66,          2,
  2021,    210,     96,          1,
  2022,    240,    110,          1,
  2023,    261,    118,          2,
  2024,    263,    133,          1,
  2025,    311,    141,          0,
  2026,    353,    168,          7
)
fouling_total <- fouling |>
  summarise(games = sum(games), foul_games = sum(foul_games), fouler_won = sum(fouler_won)) |>
  mutate(pct_fouled = round(100 * foul_games / games), pct_won = round(100 * fouler_won / foul_games, 1))

stopifnot(fouling_total$foul_games == 1018, fouling_total$fouler_won == 17)

p_fouling <- waffle(
  total = 1018, highlighted = 17, ncol = 34,
  title = "when the losing team fouled in the last minute",
  subtitle = "WNBA, 2018-2026, one square per game",
  hl_label = "fouling team won (17)", rest_label = "fouling team lost (1,001)",
  caption = CREDIT_WNBA
)

# 1b. how often teams foul at all (script: "in 46% of them")
# the every-game version: stacked bars by final margin, late fouling on the bottom
# counts come from fouling_by_margin in wnba_endings.R. everything 30+ shares a bar
by_margin <- tibble(
  margin = c(1:41, 43, 44, 48, 53),
  fouled = c(37, 52, 91, 110, 117, 111, 96, 96, 78, 58, 45, 23, 20, 12, 7, 9, 5, 6, 9, 7, 2, 2, 7, 2, 0, 0,
             1, 2, 3, 2, 2, 0, 1, 0, 2, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0),
  none   = c(38, 53, 36, 15, 16, 21, 35, 44, 45, 66, 66, 52, 64, 60, 63, 68, 60, 52, 35, 32, 34, 25, 35, 27,
             18, 17, 18, 17, 15, 7, 11, 11, 8, 5, 7, 5, 7, 4, 1, 2, 2, 1, 1, 2, 1)
)
stopifnot(sum(by_margin$fouled) == 1018, sum(by_margin$fouled) + sum(by_margin$none) == 2220)

margin_bars <- by_margin |>
  mutate(bin = pmin(margin, 30)) |>
  group_by(bin) |>
  summarise(fouled = sum(fouled), none = sum(none), .groups = "drop") |>
  pivot_longer(c(fouled, none), names_to = "group", values_to = "n") |>
  mutate(group = factor(group, levels = c("none", "fouled")))

p_fouled_margin <- ggplot(margin_bars, aes(x = bin, y = n, fill = group)) +
  geom_col(width = 0.82) +
  scale_fill_manual(values = c(fouled = ACCENT, none = LIGHT), name = NULL,
                    breaks = c("fouled", "none"),
                    labels = c(fouled = "losing team fouled in the last minute (46%)",
                               none = "no late fouling (54%)")) +
  guides(fill = guide_legend(ncol = 1)) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30), labels = c(1, 5, 10, 15, 20, 25, "30+")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.04))) +
  labs(title = "every WNBA game by final margin",
       subtitle = "2018-2026, 2,220 games",
       x = "final margin (points)", y = "games", caption = CREDIT_WNBA) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#E3E3E0", linewidth = 0.3),
        legend.position = "top", legend.justification = "left",
        legend.text = element_text(color = INK, size = 13))

# the one-bar version, for when the beat is too short to read the dots
fouled_split <- tibble(
  what = fct_inorder(c("losing team fouled", "no late fouling")),
  pct  = c(46, 54)
)

p_fouled_bar <- ggplot(fouled_split, aes(x = pct, y = 1, fill = fct_rev(what))) +
  geom_col(width = 0.6, orientation = "y") +
  annotate("text", x = 23, y = 1, label = "46%\nlosing team fouled", color = "white", size = 6,
           fontface = "bold", lineheight = 0.95) +
  annotate("text", x = 73, y = 1, label = "54%\nno late fouling", color = INK, size = 6,
           fontface = "bold", lineheight = 0.95) +
  scale_fill_manual(values = c(`losing team fouled` = ACCENT, `no late fouling` = LIGHT), guide = "none") +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = "WNBA games with fouling in the last minute",
       subtitle = "2018-2026, 2,220 games, regular season and playoffs",
       x = NULL, y = NULL, caption = CREDIT_WNBA) +
  base_theme() +
  theme(axis.text = element_blank(), panel.grid.major.x = element_blank())


# 2. IT'S SLOW, split into 2 charts so each one compares the same kind of time
# (script: "in close wnba games this year, the last 2 minutes on the clock usually took 13 and a half minutes realtime")
slow_clock <- tibble::tribble(
  ~what,           ~minutes, ~hl,
  "on the clock",   2,       FALSE,
  "in real time",  13.5,     TRUE
) |> mutate(what = fct_inorder(what))

p_slow_clock <- ggplot(slow_clock, aes(x = minutes, y = fct_rev(what), fill = hl)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste(minutes, "min")), hjust = -0.15, color = INK, size = 6) +
  scale_fill_manual(values = c(`TRUE` = ACCENT, `FALSE` = LIGHT), guide = "none") +
  scale_x_continuous(limits = c(0, 17), breaks = c(0, 4, 8, 12), expand = c(0, 0)) +
  labs(title = "the last 2 minutes of a close WNBA game",
       subtitle = "2026 games decided by 3 or fewer, median",
       x = "minutes", y = NULL, caption = CREDIT_WNBA) +
  base_theme()

# (script: "and when i timed unrivaled's first season, the whole fourth quarter averaged about 9 minutes")
# wnba: first play of the 4th to the end of the 4th from wehoop wallclock, 329 of 2026's games. the same
# code gets 31.0 for 2024, matching the 31 (range 20 to 49) from the feb 2025 video
# unrivaled: the feb 2025 video, timed by hand, early 2025 season, range 4 to 17
slow_q4 <- tibble::tribble(
  ~what,                 ~minutes, ~hl,
  "WNBA\n2026",           32,      FALSE,
  "unrivaled\nearly 2025", 9,      TRUE
) |> mutate(what = fct_inorder(what))

p_slow_q4 <- ggplot(slow_q4, aes(x = minutes, y = fct_rev(what), fill = hl)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste(minutes, "min")), hjust = -0.15, color = INK, size = 6) +
  scale_fill_manual(values = c(`TRUE` = ACCENT, `FALSE` = LIGHT), guide = "none") +
  scale_x_continuous(limits = c(0, 38), breaks = c(0, 10, 20, 30), expand = c(0, 0)) +
  labs(title = "how long a whole 4th quarter takes in real time",
       subtitle = "average, start to end\nWNBA: 10 minutes on the clock. unrivaled: no game clock",
       x = "minutes", y = NULL, caption = "data: wehoop, unrivaled games timed by @wnbadata  |  chart: @wnbadata") +
  base_theme()


# 3. GAME WINNERS (script: "only 11 of 353 games were won on a shot in the last 5 seconds")
# the full 2026 season was 354 games. wehoop has no play by play for finals game 7 yet, but that one
# wasn't won late either, so it's really 11 of 354
p_last_5 <- waffle(
  total = 353, highlighted = 11, ncol = 20,
  title = "2026 WNBA games won on a shot\nin the last 5 seconds",
  subtitle = "one square per game, regular season and playoffs",
  hl_label = "won in the last 5 seconds (11)", rest_label = "everything else (342)",
  caption = CREDIT_WNBA
)


# IN SCRIPT ORDER
#   "3 on 3, on a mini full court, with an 18 second shot clock"              p_court
#   "after three quarters / the clock turns off"                              p_elam_1
#   "whoever is winning gets 11 added to their score, and that's the target"  p_elam_2
#   "first team to hit the target score wins / somebody HAS to score"         p_elam_3
#   "in 46% of them, the losing team started fouling in the last minute"      p_fouled_margin, or p_fouled_bar for the simple version
#   "won just 17 times out of a thousand"                                     p_fouling
#   "usually took 13 and a half minutes realtime"                             p_slow_clock
#   "the whole fourth quarter averaged about 9 minutes"                       p_slow_q4
#   "only 11 of 353 games were won on a shot in the last 5 seconds"           p_last_5
p_court
p_elam_1
p_elam_2
p_elam_3
p_fouled_margin
p_fouled_bar
p_fouling
p_slow_clock
p_slow_q4
p_last_5
