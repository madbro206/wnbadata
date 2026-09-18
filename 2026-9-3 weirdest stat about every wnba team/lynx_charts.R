# charts for the minnesota lynx "weird 15/15" video
#
# everything here computes live from wehoop (espn) and was cross checked against
# her hoop stats on 9/5 using final numbers through 8/30.
#
# two mandatory wehoop filters or the season comes out wrong:
#   - the commissioner's cup final sits inside season_type == 2 but does not count
#   - the all star rosters appear as their own "teams"
#
# axis rule: anything encoding a length starts at zero. scatters and dumbbells encode
# position on both axes, so those do not need a zero baseline.

library(wehoop)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gt)

ALLSTAR   <- c("Team Spoon", "Team Coop")
CUP_FINAL <- as.Date("2026-06-30")
ME        <- "Minnesota"

HI     <- "#0C2340"   # minnesota lynx
GRAY   <- "#9EA2A2"   # everyone else
ALT    <- "#78BE20"   # second series
INK    <- "#15171D"
CREDIT <- "2026 season through the fiba break  \u00b7  data: her hoop stats and wehoop  |  chart: @wnbadata"

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

# lollipop anchored at zero. the segment encodes the value, so the axis must start at 0.
lolli <- function(df, value, fmt, title, subtitle, low_first = FALSE, hi_when = NULL) {
  d <- df |> mutate(v = {{ value }},
                    mine = if (is.null(hi_when)) team == ME else team %in% hi_when,
                    lab = sprintf(fmt, v))
  # constant gap in data units so single and double digit labels clear the dot equally
  PAD <- 0.035 * max(d$v, na.rm = TRUE)
  ggplot(d, aes(x = v, y = reorder(team, if (low_first) -v else v))) +
    geom_segment(aes(x = 0, xend = v, yend = team, colour = mine),
                 linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
    geom_point(aes(colour = mine, size = mine)) +
    geom_text(aes(label = lab, colour = mine), hjust = 0, nudge_x = PAD, size = 4.2,
              show.legend = FALSE) +
    scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
    scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.18))) +
    labs(title = title, subtitle = subtitle, caption = CREDIT, x = NULL, y = NULL) +
    base_theme()
}

# same thing for values that go negative, so zero sits inside the axis and is drawn
lolli0 <- function(df, value, fmt, title, subtitle, hi_when = NULL) {
  d <- df |> mutate(v = {{ value }},
                    mine = if (is.null(hi_when)) team == ME else team %in% hi_when,
                    lab = sprintf(fmt, v))
  PAD <- 0.045 * max(abs(d$v), na.rm = TRUE)
  ggplot(d, aes(x = v, y = reorder(team, v))) +
    geom_segment(aes(x = 0, xend = v, yend = team, colour = mine),
                 linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
    geom_point(aes(colour = mine, size = mine)) +
    geom_text(aes(label = lab, colour = mine, hjust = if_else(v >= 0, 0, 1)),
              nudge_x = if_else(d$v >= 0, PAD, -PAD), size = 4.2, show.legend = FALSE) +
    geom_vline(xintercept = 0, colour = INK, linewidth = 0.4) +
    scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
    scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.16, 0.16))) +
    labs(title = title, subtitle = subtitle, caption = CREDIT, x = NULL, y = NULL) +
    base_theme()
}

team_box <- load_wnba_team_box(seasons = 2026) |>
  filter(season_type == 2, !team_location %in% ALLSTAR, game_date != CUP_FINAL) |>
  mutate(margin = team_score - opponent_team_score)

teams <- team_box |> group_by(team = team_location) |> summarise(
  g = n(), w = sum(margin > 0), l = sum(margin < 0), mov = mean(margin),
  efg = 100 * (sum(field_goals_made) + 0.5 * sum(three_point_field_goals_made)) / sum(field_goals_attempted),
  p3  = 100 * sum(three_point_field_goals_made) / sum(three_point_field_goals_attempted),
  p3rate = 100 * sum(three_point_field_goals_attempted) / sum(field_goals_attempted),
  pa2 = (sum(field_goals_attempted) - sum(three_point_field_goals_attempted)) / n(),
  ftpct = 100 * sum(free_throws_made) / sum(free_throws_attempted),
  astrate = 100 * sum(assists) / sum(field_goals_made),
  tov = mean(turnovers),
  bl = sum(margin <= -15), bw = sum(margin >= 15),
  .groups = "drop")

player_box <- load_wnba_player_box(seasons = 2026) |>
  filter(season_type == 2, !team_location %in% ALLSTAR, game_date != CUP_FINAL, !did_not_play)
as_min <- function(x) { x <- as.character(x); ifelse(is.na(x), 0, suppressWarnings(as.numeric(x))) }

COLLIER_BACK <- as.Date("2026-07-22")   # napheesa collier's first game of the season

# 1. THE LEAD. the best team in the league got worse when their best player came back.
# negative space matters here, so zero is drawn and the bars are anchored on it.
era <- team_box |> filter(team_location == ME) |>
  mutate(era = if_else(game_date < COLLIER_BACK, "without collier", "with collier"),
         era = factor(era, levels = c("without collier", "with collier"))) |>
  group_by(era) |>
  summarise(g = n(), w = sum(margin > 0), l = sum(margin < 0), mov = mean(margin), .groups = "drop")

p_collier <- ggplot(era, aes(x = era, y = mov, fill = era == "with collier")) +
  geom_col(width = 0.55) +
  geom_text(aes(label = sprintf("%d-%d\n%+.2f a game", w, l, mov)),
            vjust = -0.3, colour = INK, size = 4.8) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = "#B4B2A9"), guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.28))) +
  labs(title = "minnesota point differential per game, before and after collier returned",
       subtitle = "split at her first game on july 22",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 2. and it is not her. she has been good, the team was already this good.
col <- player_box |> filter(athlete_display_name == "Napheesa Collier") |>
  mutate(month = format(game_date, "%m")) |> group_by(month) |>
  summarise(g = n(), ppg = mean(points),
            ts = 100 * sum(points) / (2 * (sum(field_goals_attempted) + 0.44 * sum(free_throws_attempted))),
            .groups = "drop") |>
  mutate(m = recode(month, `07` = "july", `08` = "august"))
p_col <- ggplot(col |> select(m, `points per game` = ppg, `true shooting %` = ts) |>
                  pivot_longer(-m, names_to = "k", values_to = "v"),
                aes(x = m, y = v, fill = k)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f", v)), position = position_dodge(width = 0.7),
            vjust = -0.6, colour = INK, size = 4.2) +
  scale_fill_manual(values = c(`true shooting %` = HI, `points per game` = "#B4B2A9"), name = NULL) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.18))) +
  labs(title = "collier scoring and true shooting, by month",
       subtitle = "since returning on july 22",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top", panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 3. THE ONE TO END ON. zero blowout losses through 40 games.
p_blowout <- lolli(teams, bl, "%d",
  "losses by 15 or more, by team",
  "every other team has at least one", low_first = TRUE)

# 4. how unusual the flatness is. almost every season pivoted around the all star break.
BRK <- as.Date("2026-07-26")
swing <- team_box |> mutate(era = if_else(game_date < BRK, "pre", "post")) |>
  group_by(team = team_location, era) |> summarise(mov = mean(margin), .groups = "drop") |>
  pivot_wider(names_from = era, values_from = mov) |> mutate(swing = post - pre)
p_swing <- lolli0(swing, swing, "%+.2f",
  "point differential after the all star break minus before it, by team",
  NULL)

# 5. the stability underneath it
fives <- player_box |> filter(starter) |>
  group_by(team = team_location, game_id) |>
  summarise(five = paste(sort(athlete_display_name), collapse = "|"), .groups = "drop") |>
  group_by(team) |> summarise(fives = n_distinct(five), .groups = "drop")
p_fives <- lolli(fives, fives, "%d",
  "distinct starting fives used, by team",
  "toronto has used twenty two", low_first = TRUE)

# 6. the miles setup: while collier was out, the number two pick led all rookies
# in scoring by a wide margin. from hhs box scores, 2026 regular season, minimum
# 20 games, rookies only (no prior wnba season in the data). verified 9/17.
rookies <- data.frame(
  player = c("Olivia Miles","Flau'jae Johnson","Sydney Taylor","Azzi Fudd",
             "Kiki Rice","Laura Juskaite","Awa Fam","Pauline Astier"),
  ppg    = c(19.7,14.1,13.8,13.1,12.5,10.3,10.3,9.2)
) |> mutate(mine = player == "Olivia Miles")

p_rookies <- ggplot(rookies, aes(x = ppg, y = reorder(player, ppg), fill = mine)) +
  geom_col(width = 0.72, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f", ppg)), hjust = -0.25, size = 4.2, colour = INK) +
  scale_fill_manual(values = c(`FALSE` = GRAY, `TRUE` = HI)) +
  scale_x_continuous(limits = c(0, 23), expand = c(0, 0)) +
  labs(title = "rookie scoring in 2026",
       subtitle = "olivia miles led all rookies while collier was out, by more than five a game",
       caption = CREDIT, x = "points per game", y = NULL) +
  base_theme()

# 7. the minutes ramp. the restriction was a july thing only: she opened at 22
# minutes and was playing 35+ by mid august, so "she was on a minutes limit" does
# not explain the team's dip. hhs box scores, her 12 games, verified 9/17.
collier_games <- data.frame(
  g       = 1:12,
  date    = c("jul 22","jul 28","jul 30","aug 2","aug 6","aug 8",
              "aug 9","aug 12","aug 15","aug 19","aug 24","aug 30"),
  minutes = c(22.2,25.0,21.8,31.5,33.5,34.7,35.0,32.6,25.1,36.7,31.7,35.0),
  pts     = c(24,15,16,18,21,22,17,20,19,18,9,17)
)

p_collier_mins <- ggplot(collier_games, aes(x = g, y = minutes)) +
  geom_line(colour = HI, linewidth = 1.1) +
  geom_point(colour = HI, size = 3) +
  # labels sit above the line, except at local dips where they would cross it
  geom_text(aes(label = round(minutes),
                vjust = if_else(minutes < lag(minutes, default = 99) &
                                minutes < lead(minutes, default = 99), 2.0, -1.2)),
            size = 3.8, colour = INK) +
  scale_x_continuous(breaks = collier_games$g, labels = collier_games$date) +
  # a line encodes position, not length, so this axis does not need a zero baseline
  scale_y_continuous(limits = c(18, 41)) +
  labs(title = "napheesa collier's minutes, every game since she came back",
       subtitle = "capped around 22 minutes for three games, then playing 35 by mid august",
       caption = CREDIT, x = NULL, y = "minutes played") +
  base_theme() +
  theme(panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11))

# 8. everything else, before and after she came back. per game team numbers from
# hhs box scores, split at her first game on july 22 (27 games before, 13 after).
# the point: almost nothing moved. verified 9/17.
ba_counts <- data.frame(
  stat   = c("points","points allowed","margin","assists","rebounds",
             "off rebounds","turnovers","steals","blocks","3pt attempts","ft attempts"),
  before = c(91.7,83.1,8.6,21.4,35.4,9.6,12.9,8.9,5.0,22.5,18.4),
  after  = c(90.5,84.2,6.3,22.2,32.0,8.6,11.5,8.9,3.7,26.2,15.4)
)
ba_pct <- data.frame(
  stat   = c("fg %","3pt %","efg %"),
  before = c(48.4,37.9,54.4),
  after  = c(47.8,41.2,55.4)
)

# dumbbell with the two value labels pushed to the OUTSIDE of each pair so they
# never collide, however close the two dots are
dumbbell <- function(d, title, subtitle, pad) {
  d <- d |> mutate(stat = factor(stat, levels = rev(stat)),
                   lo = pmin(before, after), hi = pmax(before, after))
  ggplot(d) +
    geom_segment(aes(x = before, xend = after, y = stat, yend = stat),
                 colour = ALT, linewidth = 1.4) +
    geom_point(aes(x = before, y = stat, colour = "before collier"), size = 3.4) +
    geom_point(aes(x = after,  y = stat, colour = "with collier"),   size = 3.4) +
    # hjust anchors each label away from its dot, so they never sit on the marks
    geom_text(aes(x = if_else(before <= after, before - pad, before + pad), y = stat,
                  label = sprintf("%.1f", before),
                  hjust = if_else(before <= after, 1, 0)), size = 3.3, colour = GRAY) +
    geom_text(aes(x = if_else(after > before, after + pad, after - pad), y = stat,
                  label = sprintf("%.1f", after),
                  hjust = if_else(after > before, 0, 1)), size = 3.3, colour = HI) +
    scale_colour_manual(values = c(`before collier` = GRAY, `with collier` = HI), name = NULL) +
    scale_x_continuous(expand = expansion(mult = 0.12)) +
    labs(title = title, subtitle = subtitle, caption = CREDIT, x = NULL, y = NULL) +
    base_theme() +
    theme(legend.position = "top", legend.justification = "left",
          panel.grid.major.y = element_line(colour = "#F0F0EE", linewidth = 0.3))
}

p_before_after <- dumbbell(ba_counts,
  "the lynx before and after collier came back",
  "per game, 27 games without her against 13 with her", pad = 1.2)

p_before_after_pct <- dumbbell(ba_pct,
  "lynx shooting before and after collier came back",
  "27 games without her against 13 with her", pad = 0.35)

# 8b. the same before/after split as a table, which reads better than the dumbbells
# reads better than the dumbbells as a single graphic. change is with-collier minus before.
# COLOUR RULE: green always means GOOD FOR MINNESOTA, not "went up". for points
# allowed and turnovers a lower number is the good one, so those two are flipped.
# shot volume rows follow "more is better" (more threes, more trips to the line),
# which is a judgement call rather than something the data proves. only a change
# of literally zero is left unshaded.
UP   <- "#E3F1E1"   # light green, good
DOWN <- "#FBE7E4"   # light red, bad

ba_all <- rbind(ba_counts, ba_pct) |>
  mutate(change = after - before,
         dir = case_when(
           stat %in% c("points allowed", "turnovers")     ~ if_else(change < 0, "good", "bad"),
           TRUE                                            ~ if_else(change > 0, "good", "bad")),
         dir = if_else(abs(change) < 0.05, "neutral", dir),
         # impact = the change re-signed so that positive always means good for
         # minnesota. sorting on it groups every green together and every red
         # together, at the cost of the change column not running in order
         impact = if_else(stat %in% c("points allowed", "turnovers"), -change, change)) |>
  arrange(desc(impact))

t_before_after <- ba_all |>
  gt() |>
  cols_hide(columns = c(dir, impact)) |>
  cols_label(stat = "", before = "without collier", after = "with collier", change = "change") |>
  fmt_number(columns = c(before, after), decimals = 1) |>
  fmt_number(columns = change, decimals = 1, force_sign = TRUE) |>
  cols_align(align = "right", columns = c(before, after, change)) |>
  tab_header(title = "the lynx with and without napheesa collier") |>
  tab_style(style = cell_fill(color = UP),   locations = cells_body(rows = dir == "good")) |>
  tab_style(style = cell_fill(color = DOWN), locations = cells_body(rows = dir == "bad")) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = c(stat, change))) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  tab_source_note(CREDIT) |>
  tab_options(table.font.size = 15, data_row.padding = 4,
              table.border.top.style = "none", heading.border.bottom.style = "none")

p_rookies       # 1. the miles setup
p_collier_mins  # 1b. her minutes ramp, kills the 'she was restricted' objection
p_before_after      # 2b. per game stats before vs with her. almost nothing moved
p_before_after_pct  # 2c. shooting percentages, the one place they got better
t_before_after      # 2d. the same split as a table (use this one if you talk over it)
p_collier
p_col
p_blowout
p_swing
p_fives
