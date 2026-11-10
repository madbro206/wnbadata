# d1 wbb games vs wnba games by day of the season (posted on threads 11/6/26)
#
# d1 counts every 2026-27 game with at least one d1 team, regular season, from espn via wehoop.
# wnba counts the 2026 regular season, commissioner's cup final and playoffs, no all-star game.
#   - finals game 7 is still marked scheduled in wehoop, so anything not postponed counts as played
#   - that gives 331 + 23 = 354 games
#
# day 1 is each season's first game: 5/8/26 for the wnba, 11/1/26 for d1.

library(dplyr)
library(ggplot2)
library(lubridate)
library(wehoop)

NCAA   <- "#6A3FB5"
WNBA   <- "#8A8A86"
GRAY   <- "#8A8A86"
INK    <- "#15171D"
CREDIT <- "data: wehoop  |  chart: @wnbadata"

PLAYED_THROUGH <- as.Date("2026-11-06")   # move this up to update the solid part of the d1 line

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
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#E3E3E0", linewidth = 0.3),
      plot.margin   = margin(18, 18, 14, 10)
    )
}

et_day <- function(x) as.Date(format(with_tz(ymd_hm(x, tz = "UTC"), "America/New_York"), "%Y-%m-%d"))

by_day <- function(games, league) {
  games |>
    count(day, name = "games") |>
    arrange(day) |>
    mutate(cum = cumsum(games), season_day = as.integer(day - min(day)) + 1, league = league)
}

wnba <- load_wnba_schedule(seasons = 2026) |>
  filter(type_abbreviation != "ALLSTAR", status_type_name != "STATUS_POSTPONED") |>
  mutate(day = et_day(date)) |>
  by_day("WNBA")

d1 <- load_wbb_schedule(seasons = 2027) |>
  filter(status_type_name != "STATUS_CANCELED") |>
  mutate(day = et_day(date)) |>
  by_day("D1") |>
  mutate(played = day <= PLAYED_THROUGH)

stopifnot(max(wnba$cum) == 354)

d1_played    <- filter(d1, played)
d1_scheduled <- filter(d1, season_day >= max(d1_played$season_day))
d1_passes    <- d1 |> filter(cum > 354) |> slice_head(n = 1)


# WHOLE SEASON, the scale is the point here. the wnba line barely leaves the floor
# d1 stops at the end of the regular season because conference tournaments and march madness aren't scheduled yet
p_whole_season <- ggplot() +
  geom_hline(yintercept = 354, color = INK, linewidth = 0.6, linetype = "22") +
  geom_line(data = wnba, aes(season_day, cum), color = WNBA, linewidth = 1.4) +
  geom_line(data = d1_scheduled, aes(season_day, cum), color = NCAA, linewidth = 1.2, linetype = "13") +
  geom_line(data = d1_played, aes(season_day, cum), color = NCAA, linewidth = 1.6) +
  annotate("segment", x = d1_passes$season_day + 2, xend = 37, y = d1_passes$cum + 25, yend = 840,
           color = INK, linewidth = 0.5) +
  geom_point(data = d1_passes, aes(season_day, cum), color = NCAA, size = 4) +
  annotate("text", x = 38, y = 900, hjust = 0, color = INK, size = 5, fontface = "bold", lineheight = 0.95,
           label = paste0(format(d1_passes$day, "%b "), day(d1_passes$day), ": D1 passes the\nentire WNBA season")) +
  annotate("text", x = max(d1$season_day), y = max(d1$cum), hjust = 1, vjust = -0.8, color = NCAA, size = 5,
           fontface = "bold", label = "D1 WBB regular season") +
  annotate("text", x = max(wnba$season_day), y = 354, hjust = 1, vjust = -1, color = GRAY, size = 5,
           fontface = "bold", label = "WNBA: 354") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "total games played by day of the season",
       subtitle = "D1 women's basketball 2026-27 vs WNBA 2026, dotted = scheduled",
       x = "day of the season", y = "games", caption = CREDIT) +
  base_theme()


p_whole_season
