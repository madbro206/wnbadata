# charts for the valkyries "weird 1/15" video
#
# starter numbers come from wehoop so this runs without the hhs container, and they
#
# the defensive ratings are hardcoded from hhs because possession formulas differ:
# wehoop's estimate gives golden state 97.6 against a 105.9 league average, hhs gives
# 96.8 against 104.9. the charts use the her hoop stats figures.
#
# two mandatory wehoop filters or the records come out wrong:
#   - the commissioner's cup final is inside season_type == 2 but does not count
#   - the all star rosters show up as their own "teams"

library(wehoop)
library(dplyr)
library(ggplot2)

ALLSTAR   <- c("Team Spoon", "Team Coop")
CUP_FINAL <- as.Date("2026-06-30")
GSV       <- "Golden State"

HI   <- "#B897D4"   # golden state
GRAY <- "#8A8A86"   # everyone else
INK  <- "#15171D"

CREDIT <- "2026 season through the fiba break  ·  data: her hoop stats and wehoop  |  chart: @wnbadata"

player_box <- load_wnba_player_box(seasons = 2026) |>
  filter(season_type == 2, !team_location %in% ALLSTAR, game_date != CUP_FINAL)

as_min <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x), 0, suppressWarnings(as.numeric(x)))
}

starters <- player_box |>
  filter(starter, !did_not_play) |>
  group_by(team = team_location) |>
  summarise(
    games    = n_distinct(game_id),
    fga_pg   = sum(field_goals_attempted) / games,
    min_pg   = sum(as_min(minutes)) / games,
    fg_pct   = 100 * sum(field_goals_made) / sum(field_goals_attempted),
    fg3_rate = 100 * sum(three_point_field_goals_attempted) / sum(field_goals_attempted),
    fg3_pct  = 100 * sum(three_point_field_goals_made) / sum(three_point_field_goals_attempted),
    .groups  = "drop"
  )

defense <- tibble::tribble(
  ~team,           ~def_rtg,
  "Golden State",     96.78,
  "Minnesota",       100.84,
  "Washington",      101.26,
  "Atlanta",         103.06,
  "Las Vegas",       103.78,
  "Seattle",         104.47,
  "Dallas",          104.56,
  "Chicago",         104.63,
  "New York",        104.99,
  "Indiana",         105.81,
  "Connecticut",     106.13,
  "Phoenix",         106.59,
  "Los Angeles",     108.41,
  "Portland",        109.26,
  "Toronto",         112.95
)

LEAGUE_AVG_OFF <- 104.9   # hhs, the number you say on camera

# one emphasis dot plot, reused for every beat.
#
# lollipops anchored at zero. the segment length encodes the value, so the axis MUST
# start at zero: that is the whole reason a bar or lollipop cannot be truncated.
#
# the tradeoff is resolution. these metrics all live far from zero, so the bars come
# out similar lengths and the differences read small. the printed value on each row is
# doing more of the work than the bar is. that is the honest version and it is the
# deliberate choice here.
#
# if you ever want the gaps to fill the frame again, drop the geom_segment line and
# only then a non zero axis becomes defensible, because bare dots encode position
# rather than length.
weird_dots <- function(df, value, title, subtitle, digits = 1,
                       low_is_notable = TRUE) {
  d <- df |>
    mutate(value = {{ value }},
           is_gsv = team == GSV,
           lab = sprintf(paste0("%.", digits, "f"), value))

  ggplot(d, aes(x = value, y = reorder(team, if (low_is_notable) -value else value))) +
    geom_segment(aes(x = 0, xend = value, yend = team, colour = is_gsv),
                 linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
    geom_point(aes(colour = is_gsv, size = is_gsv)) +
    geom_text(aes(label = lab, colour = is_gsv),
              hjust = -0.45, size = 4.2, show.legend = FALSE) +
    scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
    scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
    scale_x_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.16))
    ) +
    labs(title = title, subtitle = subtitle, caption = CREDIT, x = NULL, y = NULL) +
    theme_minimal(base_size = 15) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_text(face = "bold", colour = INK, size = 19),
      plot.subtitle = element_text(colour = GRAY, size = 13, margin = margin(b = 14)),
      plot.caption  = element_text(colour = GRAY, size = 11, hjust = 0, margin = margin(t = 14)),
      axis.text.y   = element_text(colour = INK, size = 13),
      axis.text.x   = element_text(colour = GRAY, size = 11),
      panel.grid.major.x = element_line(colour = "#E3E3E0", linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin   = margin(18, 18, 14, 10)
    )
}

# titles are written to stand alone, since charts get screenshotted and reused
# out of order. each one names the team, the metric and the finding.

# 1. the headline. starters shoot the worst percentage in the league
p_fg <- weird_dots(starters, fg_pct,
                   "golden state's starters shoot the worst percentage in the wnba",
                   "field goal percentage, starting fives only")

# 2. the concession. fewest shots and fewest minutes
p_fga <- weird_dots(starters, fga_pg,
                    "golden state's starters take the fewest shots in the league",
                    "field goal attempts per game, starting fives only")

p_min <- weird_dots(starters, min_pg,
                    "golden state's starters play the fewest minutes in the league",
                    "minutes per game, starting fives only")

# 3. the explanation. nobody's starting five shoots this many threes
p_3r <- weird_dots(starters, fg3_rate,
                   "no starting five shoots as many threes as golden state's",
                   "share of shots taken from three, starting fives only",
                   low_is_notable = FALSE)

# 4. the redemption. best defense in the league and it is not close
p_def <- weird_dots(defense, def_rtg,
                    "golden state has the best defense in the wnba",
                    sprintf("points allowed per 100 possessions · league average %.1f", LEAGUE_AVG_OFF),
                    digits = 1)

# the three point accuracy beat is a single number, not a ranking, so a dot plot
# would be the wrong form. this is the "exactly average" moment.
gsv_3pct <- starters$fg3_pct[starters$team == GSV]
p_3pct <- ggplot(starters, aes(x = fg3_pct, y = 0)) +
  geom_vline(xintercept = 34.12, colour = GRAY, linetype = "22", linewidth = 0.4) +
  geom_point(aes(colour = team == GSV, size = team == GSV), alpha = 0.9) +
  annotate("text", x = 34.12, y = 0.045, label = "league average 34.1%",
           colour = GRAY, size = 4, vjust = 0) +
  annotate("text", x = gsv_3pct, y = -0.045, label = sprintf("golden state %.1f%%", gsv_3pct),
           colour = HI, size = 4.4, fontface = "bold", vjust = 1) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 6), guide = "none") +
  scale_y_continuous(limits = c(-0.12, 0.12)) +
  labs(title = "golden state's starters shoot exactly league average from three",
       subtitle = "three point percentage, starting fives only, each dot is a team",
       caption = CREDIT, x = NULL, y = NULL) +
  theme_minimal(base_size = 15) +
  theme(plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(face = "bold", colour = INK, size = 19),
        plot.subtitle = element_text(colour = GRAY, size = 13, margin = margin(b = 14)),
        plot.caption = element_text(colour = GRAY, size = 11, hjust = 0, margin = margin(t = 14)),
        axis.text = element_blank(), panel.grid = element_blank())

# print one at a time in positron. for 9:16, size the plot pane tall and narrow,
# roughly 1080 x 1920 or any 0.5625 aspect.
p_fg
p_fga
p_min
p_3r
p_3pct
p_def
