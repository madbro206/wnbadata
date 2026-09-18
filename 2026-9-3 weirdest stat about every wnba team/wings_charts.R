# charts for the dallas wings "weird 13/15" video
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
ME        <- "Dallas"

HI     <- "#002B5C"   # dallas wings. swap this hex if you want a different color
GRAY   <- "#8A8A86"   # everyone else
ALT    <- "#B4B2A9"   # second series
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

# quarter scores rebuilt from the running play by play score, since wehoop's team box
# is game level only. these were checked against her hoop stats and match.
pbp <- load_wnba_pbp(seasons = 2026) |> filter(season_type == 2)
tbx <- load_wnba_team_box(seasons = 2026) |> filter(season_type == 2) |>
  select(game_id, game_date, team_id, team_location) |> distinct()
eop <- pbp |> filter(period_number <= 4) |>
  group_by(game_id, period_number) |>
  summarise(h = max(home_score), a = max(away_score), .groups = "drop") |>
  pivot_wider(names_from = period_number, values_from = c(h, a))
hid <- pbp |> select(game_id, home_team_id) |> distinct()
gq <- eop |>
  transmute(game_id, h1 = h_1, h2 = h_2 - h_1, h3 = h_3 - h_2, h4 = h_4 - h_3,
                     a1 = a_1, a2 = a_2 - a_1, a3 = a_3 - a_2, a4 = a_4 - a_3) |>
  inner_join(hid, by = "game_id") |>
  inner_join(tbx, by = "game_id", relationship = "many-to-many") |>
  mutate(home = team_id == home_team_id,
         q1 = if_else(home, h1, a1), q2 = if_else(home, h2, a2),
         q3 = if_else(home, h3, a3), q4 = if_else(home, h4, a4),
         o1 = if_else(home, a1, h1), o2 = if_else(home, a2, h2),
         o3 = if_else(home, a3, h3), o4 = if_else(home, a4, h4),
         team = team_location) |>
  filter(!team %in% ALLSTAR, game_date != CUP_FINAL)
qtr <- gq |> group_by(team) |>
  summarise(across(c(q1, q2, q3, q4, o1, o2, o3, o4), mean), .groups = "drop") |>
  mutate(d1 = q1 - o1, d2 = q2 - o2, d3 = q3 - o3, d4 = q4 - o4)

# 1. THE MONEY CHART. home against road for every team, sorted by the gap.
# a dumbbell, so both ends encode position on a shared scale and no zero baseline is
# needed. the zero line is drawn because winning and losing is the meaningful reference.
split <- team_box |>
  group_by(team = team_location, where = team_home_away) |>
  summarise(w = sum(margin > 0), l = sum(margin < 0), mov = mean(margin), .groups = "drop") |>
  pivot_wider(names_from = where, values_from = c(w, l, mov)) |>
  mutate(gap = mov_home - mov_away)

p_split <- ggplot(split, aes(y = reorder(team, gap))) +
  geom_segment(aes(x = mov_away, xend = mov_home, yend = team, colour = team == ME),
               linewidth = 1.5, alpha = 0.5, show.legend = FALSE) +
  geom_point(aes(x = mov_away), colour = ALT, size = 3.6) +
  geom_point(aes(x = mov_home, colour = team == ME, size = team == ME)) +
  geom_vline(xintercept = 0, colour = INK, linewidth = 0.4, linetype = "22") +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.6, `TRUE` = 5.6), guide = "none") +
  labs(title = "point differential per game at home and on the road, by team",
       subtitle = "light dot is on the road, dark dot is at home",
       caption = CREDIT, x = "point differential per game", y = NULL) +
  base_theme()

# 2. the gap itself, ranked. a length, so anchored at zero.
p_gap <- lolli(split, gap, "%.2f",
  "home point differential minus road point differential, by team",
  NULL)

# 3. the two dallas wings side by side
dal <- split |> filter(team == ME) |>
  transmute(where = "x", home = mov_home, away = mov_away) |>
  pivot_longer(-where, names_to = "k", values_to = "mov") |>
  mutate(k = factor(if_else(k == "home", "at home", "on the road"),
                    levels = c("at home", "on the road")),
         rec = c(sprintf("%d-%d", split$w_home[split$team == ME], split$l_home[split$team == ME]),
                 sprintf("%d-%d", split$w_away[split$team == ME], split$l_away[split$team == ME])))
p_dal <- ggplot(dal, aes(x = k, y = mov, fill = mov > 0)) +
  geom_col(width = 0.55) +
  geom_hline(yintercept = 0, colour = INK, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%s\n%+.2f a game", rec, mov),
                vjust = if_else(mov > 0, -0.3, 1.2)), colour = INK, size = 4.6) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = "#B4B2A9"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.3, 0.3))) +
  labs(title = "dallas record and point differential by where the game was played",
       subtitle = "regular season through the fiba break",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 4. best third quarter in the league, and they give a little back in the fourth
p_q3 <- lolli0(qtr |> mutate(team = team), d3, "%+.2f",
  "third quarter point differential per game, by team",
  NULL)

dal_q <- qtr |> filter(team == ME) |> select(d1, d2, d3, d4) |>
  pivot_longer(everything(), names_to = "q", values_to = "d") |>
  mutate(q = recode(q, d1 = "1st", d2 = "2nd", d3 = "3rd", d4 = "4th"))
p_qtr <- ggplot(dal_q, aes(x = q, y = d, fill = d > 0)) +
  geom_col(width = 0.62) +
  geom_hline(yintercept = 0, colour = INK, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.2f", d), vjust = if_else(d > 0, -0.7, 1.5)),
            colour = INK, size = 4.4) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = "#B4B2A9"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.25, 0.2))) +
  labs(title = "dallas point differential per game, by quarter",
       subtitle = NULL,
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 5. second fewest turnovers in the league
p_tov <- lolli(teams, tov, "%.1f",
  "turnovers per game, by team",
  NULL, low_first = TRUE)

# ---------------------------------------------------------------------------
# 6. THE NEW ANGLE, 9/14: dallas and minnesota have the same home record, opposite on the road.
# computed live from wehoop off `split`. matches her hoop stats exactly:
# dallas home 14-6 +8.70, road 10-10 -1.40. minnesota home 14-6 +7.65, road 17-3 +8.10.
LYNX <- "#3F8A24"   # minnesota green, so the two teams read apart from dallas navy

two <- split |> filter(team %in% c(ME, "Minnesota")) |>
  mutate(team = if_else(team == ME, "Dallas Wings", "Minnesota Lynx"))

# 6a. TABLE
t_home_road <- two |>
  transmute(team,
            home_rec = sprintf("%d-%d", w_home, l_home), mov_home,
            road_rec = sprintf("%d-%d", w_away, l_away), mov_away) |>
  gt() |>
  cols_label(team = "", home_rec = "home record", mov_home = "home margin",
             road_rec = "road record", mov_away = "road margin") |>
  fmt_number(columns = c(mov_home, mov_away), decimals = 1, force_sign = TRUE) |>
  cols_align("center", columns = -team) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = team)) |>
  tab_style(style = cell_text(color = HI, weight = "bold"),
            locations = cells_body(columns = c(road_rec, mov_away), rows = team == "Dallas Wings")) |>
  tab_style(style = cell_text(color = LYNX, weight = "bold"),
            locations = cells_body(columns = c(road_rec, mov_away), rows = team == "Minnesota Lynx")) |>
  tab_header(title = "same record at home, not the same on the road",
             subtitle = "margin is average point differential per game") |>
  tab_source_note(CREDIT) |>
  tab_options(table.font.names = "Helvetica", table.font.size = px(18),
              heading.align = "left", heading.title.font.size = px(24),
              heading.subtitle.font.size = px(15),
              column_labels.font.weight = "bold", table.border.top.color = "white")

# 6b. BARS: margin per game at home and on the road, both teams, with the records
hr <- two |>
  transmute(team,
            `at home` = mov_home, `on the road` = mov_away,
            rec_home = sprintf("%d-%d", w_home, l_home),
            rec_away = sprintf("%d-%d", w_away, l_away)) |>
  pivot_longer(c(`at home`, `on the road`), names_to = "where", values_to = "mov") |>
  mutate(rec = if_else(where == "at home", rec_home, rec_away),
         where = factor(where, levels = c("at home", "on the road")))

p_home_road <- ggplot(hr, aes(x = where, y = mov, fill = team)) +
  geom_col(position = position_dodge(width = 0.76), width = 0.68) +
  geom_hline(yintercept = 0, colour = INK, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%s\n%+.1f", rec, mov), vjust = if_else(mov >= 0, -0.35, 1.25)),
            position = position_dodge(width = 0.76), colour = INK, size = 4.2, lineheight = 0.9) +
  scale_fill_manual(values = c(`Dallas Wings` = HI, `Minnesota Lynx` = LYNX), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.25, 0.25))) +
  labs(title = "same at home, opposite on the road",
       subtitle = "record and point differential per game",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top", panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 6c. BARS: how much better at home than on the road, every team. below zero = better on the
# road. historically (1997 to 2025, no 2020) the average team is +5.7, per her hoop stats.
gap_bars <- split |> mutate(who = case_when(team == ME ~ "dallas",
                                            team == "Minnesota" ~ "minnesota",
                                            TRUE ~ "other"))

p_gap_bars <- ggplot(gap_bars, aes(x = gap, y = reorder(team, gap), fill = who)) +
  geom_col(width = 0.72, show.legend = FALSE) +
  geom_vline(xintercept = 0, colour = INK, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.1f", gap), hjust = if_else(gap >= 0, -0.15, 1.15)),
            colour = INK, size = 3.9) +
  scale_fill_manual(values = c(dallas = HI, minnesota = LYNX, other = "#C9C8C2"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.12, 0.1))) +
  labs(title = "how much better each team is at home than on the road",
       subtitle = "points per game. below zero means better on the road",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# ---------------------------------------------------------------------------
# 7. REWORK, 9/14: the offense travels, the defense doesn't. her hoop stats box score, points
# per 100 possessions, 2026 regular season. ranks compare home to every team's home numbers
# and road to every team's road numbers (15 teams). hardcoded, wehoop has no possessions.
dal_off_def <- tibble::tribble(
  ~side,     ~home, ~home_rank, ~road, ~road_rank,
  "offense", 107.7, 5,          108.2, 5,
  "defense",  98.9, 3,          110.4, 13
)
# league for reference: offense 105.8 home / 103.9 road, defense 103.9 home / 105.8 road.
# dallas defensive swing 11.5 is 2nd biggest, new york is 11.6.

ord <- function(n) paste0(n, c("th","st","nd","rd")[ifelse(n %% 100 %in% 11:13, 1, pmin(n %% 10, 4) %% 4 + 1)])

t_dal_off_def <- dal_off_def |>
  mutate(home_lab = sprintf("%.1f (%s)", home, ord(home_rank)),
         road_lab = sprintf("%.1f (%s)", road, ord(road_rank))) |>
  select(side, home_lab, road_lab) |>
  gt() |>
  cols_label(side = "", home_lab = "at home", road_lab = "on the road") |>
  cols_align("center", columns = -side) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = side)) |>
  tab_style(style = cell_text(color = HI, weight = "bold"),
            locations = cells_body(columns = c(home_lab, road_lab), rows = side == "defense")) |>
  tab_header(title = "dallas wings offense and defense, home vs road",
             subtitle = "points per 100 possessions, rank out of 15 in parentheses") |>
  tab_source_note("2026 season through the fiba break  \u00b7  data: her hoop stats  |  chart: @wnbadata") |>
  tab_options(table.font.names = "Helvetica", table.font.size = px(18),
              heading.align = "left", heading.title.font.size = px(24),
              heading.subtitle.font.size = px(15),
              column_labels.font.weight = "bold", table.border.top.color = "white")

# ---------------------------------------------------------------------------
# 8. HISTORY: home court advantage in the wnba by season, 1997 to 2026. her hoop stats,
# regular season, every game counted once from the home team's side. pulled 9/14.
# home_margin is the home team's average point differential per game.
# note: a team's home-minus-road split is about twice this number (it wins by ~x at home and
# loses by ~x on the road), which is where "about six points better at home" comes from.
# 2020 was the bubble, no real home crowds: home teams won exactly 50.0%.
home_court <- tibble::tribble(
  ~yr,  ~games, ~home_win_pct, ~home_margin,
  1997, 112, 63.4, 4.54,   1998, 150, 59.3, 2.97,   1999, 192, 59.9, 3.79,
  2000, 256, 59.0, 3.47,   2001, 256, 59.8, 2.94,   2002, 256, 59.4, 3.07,
  2003, 238, 62.6, 4.29,   2004, 221, 61.1, 3.25,   2005, 221, 64.7, 3.87,
  2006, 238, 60.5, 3.56,   2007, 221, 53.8, 1.72,   2008, 238, 63.9, 3.17,
  2009, 221, 64.3, 4.04,   2010, 204, 58.8, 2.63,   2011, 204, 61.8, 3.63,
  2012, 204, 58.3, 3.37,   2013, 204, 60.8, 3.45,   2014, 204, 57.8, 2.46,
  2015, 204, 61.8, 3.42,   2016, 204, 55.4, 2.41,   2017, 204, 59.3, 3.26,
  2018, 203, 54.2, 1.53,   2019, 204, 60.8, 3.97,   2020, 132, 50.0, 0.83,
  2021, 192, 53.6, 0.45,   2022, 216, 54.6, 0.90,   2023, 240, 52.1, 1.61,
  2024, 240, 52.5, 0.79,   2025, 286, 55.9, 2.63,   2026, 300, 53.7, 1.78
) |> mutate(era = case_when(yr <= 2019 ~ "1997 to 2019", yr == 2020 ~ "2020 bubble",
                            TRUE ~ "2021 to 2026"))

# era averages weighted by games
home_court_eras <- home_court |> group_by(era) |>
  summarise(home_win_pct = round(sum(home_win_pct * games) / sum(games), 1),
            home_margin = round(sum(home_margin * games) / sum(games), 2),
            games = sum(games), .groups = "drop")

p_home_court <- ggplot(home_court, aes(x = yr, y = home_win_pct)) +
  geom_hline(yintercept = 50, colour = INK, linewidth = 0.4, linetype = "22") +
  annotate("text", x = 1997, y = 50, vjust = 1.6, hjust = 0, size = 3.6, colour = INK,
           label = "50%, no home advantage") +
  geom_line(colour = GRAY, linewidth = 1) +
  geom_point(aes(colour = yr == 2020), size = 3, show.legend = FALSE) +
  annotate("text", x = 2020, y = 50, vjust = 1.9, size = 3.7, colour = HI,
           label = "2020 bubble, 50.0%") +
  scale_colour_manual(values = c(`TRUE` = HI, `FALSE` = INK), guide = "none") +
  scale_x_continuous(breaks = c(1997, 2005, 2010, 2015, 2020, 2026)) +
  scale_y_continuous(limits = c(45, 67), breaks = seq(45, 65, 5),
                     labels = function(x) paste0(x, "%")) +
  labs(title = "how often the home team wins in the wnba, by season",
       subtitle = "home court has mattered a lot less since the 2020 bubble",
       caption = "wnba regular seasons 1997 to 2026  \u00b7  data: her hoop stats  |  chart: @wnbadata",
       x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3),
        panel.grid.major.x = element_blank())

# ---------------------------------------------------------------------------
# 9. PAIGE BUECKERS scores more on the road. her hoop stats box score, 2026 regular season.
# 19 home games, 19 road games. true shooting 60.4% in both. she plays more on the road
# (34.6 minutes vs 31.5), but per 36 minutes she still scores more away: 22.6 vs 21.9.
bueckers_hr <- tibble::tribble(
  ~where,        ~ppg, ~mpg, ~ts,
  "at home",     19.2, 31.5, 60.4,
  "on the road", 21.7, 34.6, 60.4
) |> mutate(where = factor(where, levels = c("at home", "on the road")),
            per36 = round(ppg / mpg * 36, 1))

p_bueckers_hr <- ggplot(bueckers_hr, aes(x = where, y = ppg, fill = where == "on the road")) +
  geom_col(width = 0.58, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f", ppg)), vjust = -0.6, colour = INK, size = 5) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = ALT), guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.14))) +
  labs(title = "paige bueckers points per game, home vs road",
       subtitle = "same 60.4% true shooting in both",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# 10. p_home_road with dallas only, no minnesota. same data (`hr`), same style.
p_home_road_dal <- ggplot(filter(hr, team == "Dallas Wings"),
                          aes(x = where, y = mov, fill = mov >= 0)) +
  geom_col(width = 0.58, show.legend = FALSE) +
  geom_hline(yintercept = 0, colour = INK, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%s\n%+.1f", rec, mov), vjust = if_else(mov >= 0, -0.35, 1.25)),
            colour = INK, size = 4.6, lineheight = 0.9) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = ALT), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.25, 0.25))) +
  labs(title = "dallas wings at home vs on the road",
       subtitle = "record and point differential per game",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# 11. "historically about nine out of ten teams play better at home." every team season 1997 to
# 2025 except the 2020 bubble, 353 team seasons, her hoop stats. each team's home point
# differential per game minus its road point differential, binned to 1 point
# (bin 4 = 4.0 to 4.99). 316 of 353 are above zero (89.5%). one team season sits at exactly
# 0.0, so bin 0 is split into 16 better at home + 1 even. hardcoded bin counts.
# dashed line: the 2026 wings at +10.1 for reference (2026 is not in the histogram).
split_hist <- tibble::tribble(
  ~bin, ~home, ~even, ~road,
  -9, 0, 0, 1,   -8, 0, 0, 1,   -5, 0, 0, 2,   -4, 0, 0, 3,   -3, 0, 0, 6,
  -2, 0, 0, 9,   -1, 0, 0, 14,   0, 16, 1, 0,   1, 16, 0, 0,   2, 18, 0, 0,
   3, 28, 0, 0,   4, 35, 0, 0,   5, 24, 0, 0,   6, 35, 0, 0,   7, 30, 0, 0,
   8, 30, 0, 0,   9, 28, 0, 0,  10, 14, 0, 0,  11, 17, 0, 0,  12, 10, 0, 0,
  13,  5, 0, 0,  14,  4, 0, 0,  15,  3, 0, 0,  16,  1, 0, 0,  17,  1, 0, 0,
  23,  1, 0, 0
) |> pivot_longer(-bin, names_to = "side", values_to = "n") |>
  filter(n > 0) |>
  mutate(side = factor(side, levels = c("road", "even", "home"),
                       labels = c("better on the road", "no difference", "better at home")))

p_split_hist <- ggplot(split_hist, aes(x = bin + 0.5, y = n, fill = side)) +
  geom_col(width = 0.92) +
  geom_vline(xintercept = 0, colour = INK, linewidth = 0.5) +
  geom_vline(xintercept = 10.1, colour = HI, linewidth = 0.7, linetype = "22") +
  annotate("text", x = 10.4, y = 36, hjust = 0, size = 3.8, colour = HI,
           label = "2026 wings, +10.1") +
  scale_fill_manual(values = c(`better on the road` = ALT, `no difference` = GRAY,
                               `better at home` = HI), name = NULL) +
  scale_x_continuous(breaks = seq(-10, 25, 5)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  labs(title = "how much better every team was at home than on the road",
       subtitle = "316 of 353 team seasons since 1997 were better at home (not counting 2020)",
       caption = "wnba regular seasons 1997 to 2025, excluding 2020  \u00b7  data: her hoop stats  |  chart: @wnbadata",
       x = "home point differential minus road point differential, per game",
       y = "team seasons") +
  base_theme() +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# 12. SIMPLE ERA BARS for the three spoken lines: before 2020 ~60%, 2020 bubble 50%, since ~54%.
# from home_court_eras (weighted by games). 2021-2026 is 53.8%; 2026 alone is 53.7%.
p_home_court_eras <- home_court_eras |>
  mutate(era = factor(era, levels = c("1997 to 2019", "2020 bubble", "2021 to 2026"),
                      labels = c("1997 to 2019", "2020 bubble", "2021 to now"))) |>
  ggplot(aes(x = era, y = home_win_pct, fill = era == "2021 to now")) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", home_win_pct)), vjust = -0.6, colour = INK, size = 5) +
  geom_hline(yintercept = 50, colour = INK, linewidth = 0.4, linetype = "22") +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = ALT), guide = "none") +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10),
                     labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0))) +
  labs(title = "how often the home team wins in the wnba",
       subtitle = "dashed line is 50%, no home advantage",
       caption = "wnba regular seasons 1997 to 2026  \u00b7  data: her hoop stats  |  chart: @wnbadata",
       x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# ALL THE CHARTS. run any of these on its own to render it.
#
# charts, in order
t_home_road    # 1. TABLE. dallas vs minnesota home and road record and margin ("same as minnesota")
p_split_hist   # 1c. "nine out of ten teams": every team season's home minus road gap, 316 of 353 above zero
p_home_court_eras # 1b-alt. three bars: 60.0% before 2020, 50.0% bubble, 53.8% since
p_home_court   # 1b. HISTORY. home team win % by season since 1997, bubble marked
p_gap_bars     # 2. every team's home minus road gap. dallas biggest this year
p_bueckers_hr  # 3a. bueckers 19.2 at home, 21.7 on the road, same true shooting
t_dal_off_def  # 3. TABLE. dallas offense 5th home and road, defense 3rd home vs 13th road (viewer pane)
p_home_road_dal # 1a. dallas only: 14-6 +8.7 at home, 10-10 -1.4 on the road
p_home_road    # 3b. dallas vs minnesota as bars, if you want a second look at the opening
#
# built but not used in the final video
p_split        # home vs road dumbbell, every team
p_gap          # home minus road, lollipop
p_dal          # dallas alone, home vs road
p_q3           # third quarter differential by team
p_qtr          # dallas by quarter
p_tov          # turnovers per game
#
# tibbles
# split          every team's home and road wins, losses and margin, computed live from wehoop
# home_court     league home win % and home margin by season, 1997 to 2026
# bueckers_hr    paige bueckers home vs road ppg, minutes, true shooting, per 36
# home_court_eras  same, averaged before the bubble, 2020, and since
# split_hist     binned home minus road gaps, 353 team seasons 1997-2025 excl 2020
# teams          every team's 2026 shooting and record
