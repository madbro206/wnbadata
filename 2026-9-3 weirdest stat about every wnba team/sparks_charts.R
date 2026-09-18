# charts for the los angeles sparks "weird 11/15" video
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
ME        <- "Los Angeles"

HI     <- "#552583"   # los angeles sparks. swap this hex if you want a different color
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

# 1. THE LEAD. trailed after the first quarter more than anyone in the league.
trail <- gq |> mutate(d1 = q1 - o1, fin = (q1 + q2 + q3 + q4) - (o1 + o2 + o3 + o4)) |>
  group_by(team) |>
  summarise(trailed = sum(d1 < 0), trail_w = sum(d1 < 0 & fin > 0),
            led = sum(d1 > 0), led_w = sum(d1 > 0 & fin > 0), .groups = "drop")

p_trail <- lolli(trail, trailed, "%d",
  "games spent trailing at the end of the first quarter, by team",
  NULL)

# 2. and what happens when they do. two bars, zero anchored, so the win share is honest.
la <- trail |> filter(team == ME)
after1 <- tibble::tibble(
  state = factor(c("trailing after 1", "leading after 1"),
                 levels = c("trailing after 1", "leading after 1")),
  w = c(la$trail_w, la$led_w),
  l = c(la$trailed - la$trail_w, la$led - la$led_w)) |>
  pivot_longer(c(w, l), names_to = "res", values_to = "n") |>
  mutate(res = factor(if_else(res == "w", "won", "lost"), levels = c("lost", "won")))

p_after1 <- ggplot(after1, aes(x = n, y = state, fill = res)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), colour = "white", size = 4.6) +
  scale_fill_manual(values = c(won = HI, lost = "#D6D5D0"), name = NULL) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "los angeles record by how the first quarter ended",
       subtitle = "los angeles record by how the first quarter ended",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top")

# 3. THE TWIST. the entire deficit is the first quarter and then they play you even.
la_q <- qtr |> filter(team == ME) |> select(d1, d2, d3, d4) |>
  pivot_longer(everything(), names_to = "q", values_to = "d") |>
  mutate(q = recode(q, d1 = "1st", d2 = "2nd", d3 = "3rd", d4 = "4th"))
p_qtr <- ggplot(la_q, aes(x = q, y = d, fill = d < 0)) +
  geom_col(width = 0.62) +
  geom_hline(yintercept = 0, colour = INK, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.2f", d), vjust = if_else(d < 0, 1.5, -0.7)),
            colour = INK, size = 4.4) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = "#9C9A94"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  labs(title = "los angeles point differential per game, by quarter",
       subtitle = NULL,
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 4. the offense is not the problem. second best assist rate in the league on a 15-25 team.
p_ast <- lolli(teams, astrate, "%.1f",
  "share of made field goals that were assisted, by team",
  NULL)

# 5. first quarter differential. included because seattle
# is technically worse, by 0.025 a game, which is one point across the whole season.
p_q1 <- lolli0(qtr |> mutate(team = team), d1, "%+.2f",
  "first quarter point differential per game, by team",
  "los angeles and seattle are 0.025 apart",
  hi_when = c("Los Angeles", "Seattle"))

# ---------------------------------------------------------------------------
# 6. THE NEW LEAD, added 9/12. los angeles allows more first quarter points than
# any team in the wnba. seattle is next. the league average quarter is about 21.8.
p_opp_q1 <- lolli(qtr, o1, "%.1f",
  "points allowed in the first quarter, by team",
  NULL)

# 7. THE POINT. their offense climbs all game and the defense only settles after
# the first. so "they come out flat" is wrong, the offense comes out fine.
# league average quarter drawn in so the first quarter bar has something to clear.
LG <- mean(c(qtr$o1, qtr$o2, qtr$o3, qtr$o4))

la_qtr <- qtr |> filter(team == ME) |>
  select(q1, q2, q3, q4, o1, o2, o3, o4) |>
  pivot_longer(everything(), names_to = "k", values_to = "v") |>
  mutate(q = paste0("q", substr(k, 2, 2)),
         side = if_else(substr(k, 1, 1) == "q", "sparks scored", "sparks allowed"))

p_la_qtr <- ggplot(la_qtr, aes(x = q, y = v, fill = side)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.64) +
  geom_text(aes(label = sprintf("%.1f", v)), position = position_dodge(width = 0.72),
            vjust = -0.6, colour = INK, size = 3.9) +
  geom_hline(yintercept = LG, linetype = "22", colour = INK, linewidth = 0.5) +
  annotate("text", x = 0.55, y = LG, vjust = -0.7, hjust = 0, size = 3.7, colour = INK,
           label = sprintf("league average quarter, %.1f", LG)) +
  scale_fill_manual(values = c(`sparks allowed` = HI, `sparks scored` = ALT), name = NULL) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.18))) +
  labs(title = "sparks points scored and allowed, by quarter",
       subtitle = "the offense climbs all game. the defense only settles after the first",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top", panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# FAKE STANDINGS: every team's record with first quarters removed.
# final score minus the first quarter, overtime included. her hoop stats, pulled 9/12.
# hardcoded because wehoop's play by play only covers regulation, so it scores
# overtime games as ties (la comes out 16-22-2 there instead of 17-22-1).
# sorted by how many wins each team gains. seattle is the punchline, not la.
no_q1_raw <- tibble::tribble(
  ~team,          ~real_w, ~real_l, ~w,  ~l, ~t,
  "Seattle",            8,      32, 20, 19,  1,
  "Toronto",           11,      29, 16, 24,  0,
  "Los Angeles",       15,      25, 17, 22,  1,
  "Golden State",      29,      11, 30, 10,  0,
  "Phoenix",           14,      26, 15, 25,  0,
  "Chicago",           15,      25, 15, 25,  0,
  "Dallas",            24,      16, 23, 16,  1,
  "Minnesota",         31,       9, 30,  9,  1,
  "Connecticut",       10,      30,  8, 31,  1,
  "Washington",        24,      16, 22, 18,  0,
  "Portland",          16,      24, 14, 22,  4,
  "Atlanta",           26,      14, 23, 16,  1,
  "New York",          24,      16, 19, 20,  1,
  "Indiana",           26,      14, 20, 19,  1,
  "Las Vegas",         27,      13, 21, 17,  2
)
no_q1 <- no_q1_raw |> mutate(
  real    = sprintf("%d-%d", real_w, real_l),
  no_q1   = if_else(t > 0, sprintf("%d-%d-%d", w, l, t), sprintf("%d-%d", w, l)),
  change  = w - real_w
) |> select(team, real, no_q1, change)

# 8. THE FAKE STANDINGS CHART. wins in real life (hollow) against wins with every
# first quarter deleted (filled), arrow pointing from real to fake. dumbbells encode
# position, so no zero baseline. seattle and los angeles highlighted: la is the
# subject, seattle is the punchline.
nq <- no_q1_raw |>
  mutate(fake = if_else(t > 0, sprintf("%d-%d-%d", w, l, t), sprintf("%d-%d", w, l)),
         real = sprintf("%d-%d", real_w, real_l),
         change = w - real_w,
         mine = team %in% c(ME, "Seattle"),
         lab_x = pmax(w, real_w) + 1.2,
         lab = sprintf("%s  \u2192  %s", real, fake))

p_no_q1 <- ggplot(nq, aes(y = reorder(team, change))) +
  geom_segment(aes(x = real_w, xend = w, yend = reorder(team, change), colour = mine),
               linewidth = 1.1, arrow = arrow(length = unit(0.14, "cm"), type = "closed"),
               show.legend = FALSE) +
  geom_point(aes(x = real_w, colour = mine), shape = 21, fill = "white", size = 3.2,
             stroke = 1.1, show.legend = FALSE) +
  geom_point(aes(x = w, colour = mine), size = 3.6, show.legend = FALSE) +
  geom_text(aes(x = lab_x, label = lab, colour = mine), hjust = 0, size = 3.8,
            show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_x_continuous(limits = c(5, 45), breaks = seq(10, 30, 5)) +
  labs(title = "wnba standings if every first quarter never happened",
       subtitle = "hollow is the real record, filled is with the first quarter deleted",
       caption = CREDIT, x = "wins", y = NULL) +
  base_theme()

# ---------------------------------------------------------------------------
# 9. HISTORY: how often a team wins given its first quarter margin. every wnba regular
# season game 2006 to 2025, both teams in every game. her hoop stats, pulled 9/12.
# hardcoded because wehoop does not go back that far.
#
# WHY 2006: the wnba played two 20 minute HALVES through 2005 and switched to quarters
# in 2006. in hhs, 2004 and 2005 store the two halves in pts_prd_1 and pts_prd_2 (about
# 33 to 37 points each, with nothing in periods 3 and 4), and 1997 to 2003 are stored as
# 0-0. there is no first quarter before 2006 to find, anywhere.
# the ends are capped: -15 means down 15 or more, +15 means up 15 or more.
# the curve is symmetric by construction, since every game is one team's +m and the
# other team's -m.
wp <- tibble::tribble(
  ~m,   ~n,  ~p,
  -15, 198,  7.1, -14,  93, 17.2, -13, 108, 13.9, -12, 112, 16.1, -11, 156, 17.3,
  -10, 226, 19.5,  -9, 216, 18.5,  -8, 252, 26.6,  -7, 295, 27.5,  -6, 319, 29.5,
   -5, 342, 35.7,  -4, 407, 34.2,  -3, 431, 38.7,  -2, 451, 46.8,  -1, 419, 43.4,
    0, 474, 50.0,   1, 419, 56.6,   2, 451, 53.2,   3, 431, 61.3,   4, 407, 65.8,
    5, 342, 64.3,   6, 319, 70.5,   7, 295, 72.5,   8, 252, 73.4,   9, 216, 81.5,
   10, 226, 80.5,  11, 156, 82.7,  12, 112, 83.9,  13, 108, 86.1,  14,  93, 82.8,
   15, 198, 92.9
)
LA_Q1 <- -3.40   # sparks average first quarter margin, 2026

p_winprob <- ggplot(wp, aes(x = m, y = p)) +
  geom_hline(yintercept = 50, colour = "#E3E3E0", linewidth = 0.6) +
  geom_vline(xintercept = LA_Q1, linetype = "22", colour = HI, linewidth = 0.7) +
  annotate("text", x = LA_Q1 - 0.4, y = 92, hjust = 1, size = 3.9, colour = HI,
           label = "sparks' average first\nquarter this year, -3.4") +
  geom_line(colour = GRAY, linewidth = 1) +
  geom_point(aes(size = n), colour = INK, alpha = 0.85, show.legend = FALSE) +
  scale_size_area(max_size = 3.6) +
  scale_x_continuous(breaks = c(-15, -10, -5, 0, 5, 10, 15),
                     labels = c("-15 or\nworse", "-10", "-5", "tied", "+5", "+10", "+15 or\nbetter")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%")) +
  labs(title = "how often a wnba team wins, by its first quarter margin",
       subtitle = "teams that lead after one quarter win 69.3% of the time",
       caption = "wnba regular seasons 2006 to 2025, the quarters era  \u00b7  data: her hoop stats  |  chart: @wnbadata",
       x = "first quarter margin", y = "won the game") +
  base_theme() +
  theme(panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3),
        panel.grid.major.x = element_blank())

# ---------------------------------------------------------------------------
# 10. EVERY TEAM SEASON SINCE 2006: first quarter points scored against allowed per game.
# 262 team seasons, her hoop stats, pulled 9/12. hardcoded, wehoop does not go back far enough.
# team names are hhs's current franchise names, so 2006 detroit shows as dallas wings etc.
#
# RANKINGS FOR THE SCRIPT. la 2026 at 24.18 allowed is 2nd of 262, behind the 2020 fever
# at 24.36, which was the 22 game bubble season. seattle 2026 is tied 3rd at 23.78.
# ERA CAVEAT: scoring has gone up since 2006, so raw points drift up and right over time
# (the dots darken by year to show it). relative to each season's league average, la is
# only 13th, +2.35. the worst ever relative first quarter defense is phoenix 2009, +4.34.
# so "second most first quarter points allowed since 2006" is true, "second worst first
# quarter defense ever" is not.
q1_hist <- read.csv(text = "yr,team,scored,allowed
2006,Charlotte Sting,16.71,18.38
2006,Chicago Sky,16.32,21.24
2006,Connecticut Sun,19.56,17.59
2006,Dallas Wings,19.76,16.82
2006,Houston Comets,19.18,18.29
2006,Indiana Fever,17.56,16.00
2006,Las Vegas Aces,19.00,17.79
2006,Los Angeles Sparks,17.00,17.35
2006,Minnesota Lynx,18.06,20.56
2006,New York Liberty,15.36,17.76
2006,Phoenix Mercury,21.00,21.06
2006,Sacramento Monarchs,18.50,16.47
2006,Seattle Storm,19.79,17.47
2006,Washington Mystics,18.68,19.76
2007,Chicago Sky,19.00,17.06
2007,Connecticut Sun,19.71,18.00
2007,Dallas Wings,19.06,19.56
2007,Houston Comets,18.38,19.21
2007,Indiana Fever,19.79,16.18
2007,Las Vegas Aces,17.62,17.88
2007,Los Angeles Sparks,17.26,19.97
2007,Minnesota Lynx,18.62,20.82
2007,New York Liberty,17.38,17.44
2007,Phoenix Mercury,22.35,22.24
2007,Sacramento Monarchs,17.71,17.44
2007,Seattle Storm,20.88,19.71
2007,Washington Mystics,17.12,19.38
2008,Atlanta Dream,16.62,21.00
2008,Chicago Sky,17.97,17.65
2008,Connecticut Sun,19.26,19.68
2008,Dallas Wings,19.35,18.29
2008,Houston Comets,19.03,17.15
2008,Indiana Fever,18.62,17.12
2008,Las Vegas Aces,18.29,17.35
2008,Los Angeles Sparks,20.24,18.68
2008,Minnesota Lynx,18.09,18.12
2008,New York Liberty,18.03,17.94
2008,Phoenix Mercury,20.91,21.56
2008,Sacramento Monarchs,20.06,18.26
2008,Seattle Storm,17.06,17.32
2008,Washington Mystics,16.56,19.97
2009,Atlanta Dream,20.41,19.35
2009,Chicago Sky,17.50,18.74
2009,Connecticut Sun,18.41,20.76
2009,Dallas Wings,19.09,17.15
2009,Indiana Fever,18.79,17.76
2009,Las Vegas Aces,17.76,17.65
2009,Los Angeles Sparks,19.03,17.47
2009,Minnesota Lynx,20.26,20.09
2009,New York Liberty,17.21,17.91
2009,Phoenix Mercury,21.56,23.18
2009,Sacramento Monarchs,18.94,20.35
2009,Seattle Storm,17.82,16.71
2009,Washington Mystics,18.03,17.71
2010,Atlanta Dream,21.18,21.26
2010,Chicago Sky,19.47,18.53
2010,Connecticut Sun,19.00,21.03
2010,Dallas Wings,19.15,22.94
2010,Indiana Fever,20.44,19.15
2010,Las Vegas Aces,18.82,19.35
2010,Los Angeles Sparks,18.12,20.09
2010,Minnesota Lynx,19.74,19.18
2010,New York Liberty,19.21,18.18
2010,Phoenix Mercury,24.26,23.41
2010,Seattle Storm,19.65,17.53
2010,Washington Mystics,18.32,16.71
2011,Atlanta Dream,20.35,19.47
2011,Chicago Sky,18.85,17.29
2011,Connecticut Sun,19.71,20.00
2011,Dallas Wings,16.21,21.82
2011,Indiana Fever,20.79,19.03
2011,Las Vegas Aces,19.53,18.74
2011,Los Angeles Sparks,19.76,19.76
2011,Minnesota Lynx,20.59,19.00
2011,New York Liberty,19.35,19.15
2011,Phoenix Mercury,23.00,21.21
2011,Seattle Storm,17.74,17.24
2011,Washington Mystics,17.74,20.91
2012,Atlanta Dream,20.47,17.29
2012,Chicago Sky,18.91,19.38
2012,Connecticut Sun,19.91,18.50
2012,Dallas Wings,19.56,21.12
2012,Indiana Fever,19.21,18.65
2012,Las Vegas Aces,21.62,19.62
2012,Los Angeles Sparks,21.88,19.56
2012,Minnesota Lynx,21.41,20.91
2012,New York Liberty,19.12,19.85
2012,Phoenix Mercury,18.38,21.62
2012,Seattle Storm,17.09,16.97
2012,Washington Mystics,16.18,20.26
2013,Atlanta Dream,20.65,18.85
2013,Chicago Sky,20.03,18.38
2013,Connecticut Sun,15.76,19.50
2013,Dallas Wings,17.71,18.82
2013,Indiana Fever,17.68,18.09
2013,Las Vegas Aces,18.24,19.12
2013,Los Angeles Sparks,21.18,18.12
2013,Minnesota Lynx,21.88,17.68
2013,New York Liberty,17.50,19.85
2013,Phoenix Mercury,19.15,19.74
2013,Seattle Storm,16.79,19.09
2013,Washington Mystics,18.97,18.29
2014,Atlanta Dream,19.41,20.94
2014,Chicago Sky,19.62,20.12
2014,Connecticut Sun,18.88,18.32
2014,Dallas Wings,19.65,21.71
2014,Indiana Fever,18.76,17.35
2014,Las Vegas Aces,19.09,20.91
2014,Los Angeles Sparks,19.91,20.29
2014,Minnesota Lynx,21.47,18.79
2014,New York Liberty,17.79,20.06
2014,Phoenix Mercury,22.15,17.97
2014,Seattle Storm,18.32,18.44
2014,Washington Mystics,17.68,17.82
2015,Atlanta Dream,18.94,20.03
2015,Chicago Sky,22.24,21.15
2015,Connecticut Sun,18.35,20.38
2015,Dallas Wings,21.26,19.59
2015,Indiana Fever,20.74,20.32
2015,Las Vegas Aces,17.65,20.26
2015,Los Angeles Sparks,19.47,18.59
2015,Minnesota Lynx,19.15,18.18
2015,New York Liberty,19.94,18.94
2015,Phoenix Mercury,19.21,17.79
2015,Seattle Storm,17.94,21.76
2015,Washington Mystics,20.24,18.12
2016,Atlanta Dream,20.97,21.35
2016,Chicago Sky,22.97,20.88
2016,Connecticut Sun,20.74,21.03
2016,Dallas Wings,20.65,21.74
2016,Indiana Fever,19.82,20.68
2016,Las Vegas Aces,17.12,20.82
2016,Los Angeles Sparks,20.56,19.41
2016,Minnesota Lynx,22.38,19.18
2016,New York Liberty,21.03,21.24
2016,Phoenix Mercury,19.82,19.94
2016,Seattle Storm,20.88,20.76
2016,Washington Mystics,20.74,20.65
2017,Atlanta Dream,19.97,22.82
2017,Chicago Sky,20.03,21.71
2017,Connecticut Sun,22.35,20.26
2017,Dallas Wings,20.76,23.00
2017,Indiana Fever,18.26,21.74
2017,Las Vegas Aces,18.59,20.32
2017,Los Angeles Sparks,22.59,18.53
2017,Minnesota Lynx,23.15,19.15
2017,New York Liberty,20.74,20.41
2017,Phoenix Mercury,21.41,21.21
2017,Seattle Storm,21.88,21.56
2017,Washington Mystics,21.79,20.82
2018,Atlanta Dream,19.79,19.82
2018,Chicago Sky,21.15,21.91
2018,Connecticut Sun,21.82,20.50
2018,Dallas Wings,23.09,22.09
2018,Indiana Fever,19.12,23.15
2018,Las Vegas Aces,21.70,22.15
2018,Los Angeles Sparks,19.56,18.94
2018,Minnesota Lynx,21.38,19.18
2018,New York Liberty,20.09,21.56
2018,Phoenix Mercury,22.74,21.91
2018,Seattle Storm,22.21,21.06
2018,Washington Mystics,22.18,22.58
2019,Atlanta Dream,18.15,19.35
2019,Chicago Sky,22.59,22.62
2019,Connecticut Sun,22.35,20.26
2019,Dallas Wings,17.38,20.56
2019,Indiana Fever,18.15,18.53
2019,Las Vegas Aces,21.00,21.88
2019,Los Angeles Sparks,19.32,20.35
2019,Minnesota Lynx,20.00,19.76
2019,New York Liberty,18.88,21.88
2019,Phoenix Mercury,20.12,19.68
2019,Seattle Storm,20.44,19.35
2019,Washington Mystics,24.91,19.06
2020,Atlanta Dream,19.64,22.91
2020,Chicago Sky,21.27,21.73
2020,Connecticut Sun,19.73,18.18
2020,Dallas Wings,22.23,21.27
2020,Indiana Fever,21.45,24.36
2020,Las Vegas Aces,23.32,22.00
2020,Los Angeles Sparks,21.41,19.09
2020,Minnesota Lynx,21.00,22.50
2020,New York Liberty,17.73,22.55
2020,Phoenix Mercury,22.68,20.09
2020,Seattle Storm,23.09,18.18
2020,Washington Mystics,20.18,20.86
2021,Atlanta Dream,19.88,20.94
2021,Chicago Sky,21.28,21.06
2021,Connecticut Sun,21.34,16.28
2021,Dallas Wings,20.50,20.53
2021,Indiana Fever,18.03,22.97
2021,Las Vegas Aces,22.91,21.88
2021,Los Angeles Sparks,18.16,20.25
2021,Minnesota Lynx,22.16,21.03
2021,New York Liberty,20.97,22.78
2021,Phoenix Mercury,20.44,20.03
2021,Seattle Storm,22.47,21.69
2021,Washington Mystics,21.13,19.81
2022,Atlanta Dream,21.47,21.97
2022,Chicago Sky,22.61,19.83
2022,Connecticut Sun,21.39,20.08
2022,Dallas Wings,21.03,21.17
2022,Indiana Fever,18.44,22.75
2022,Las Vegas Aces,24.00,21.03
2022,Los Angeles Sparks,20.39,22.78
2022,Minnesota Lynx,20.94,21.11
2022,New York Liberty,19.31,21.36
2022,Phoenix Mercury,19.42,20.83
2022,Seattle Storm,21.67,19.50
2022,Washington Mystics,21.56,19.81
2023,Atlanta Dream,21.80,21.40
2023,Chicago Sky,20.18,21.50
2023,Connecticut Sun,22.20,20.40
2023,Dallas Wings,21.88,21.88
2023,Indiana Fever,20.60,23.78
2023,Las Vegas Aces,24.00,20.25
2023,Los Angeles Sparks,19.98,20.83
2023,Minnesota Lynx,21.55,21.73
2023,New York Liberty,23.43,20.75
2023,Phoenix Mercury,20.10,22.20
2023,Seattle Storm,19.35,22.00
2023,Washington Mystics,20.98,19.33
2024,Atlanta Dream,19.03,19.85
2024,Chicago Sky,20.10,20.73
2024,Connecticut Sun,19.93,19.85
2024,Dallas Wings,20.70,22.88
2024,Indiana Fever,21.70,23.28
2024,Las Vegas Aces,21.75,20.80
2024,Los Angeles Sparks,19.83,20.50
2024,Minnesota Lynx,21.68,19.13
2024,New York Liberty,23.63,19.95
2024,Phoenix Mercury,18.65,20.78
2024,Seattle Storm,20.98,20.28
2024,Washington Mystics,20.65,20.60
2025,Atlanta Dream,21.41,18.95
2025,Chicago Sky,19.07,21.39
2025,Connecticut Sun,18.91,21.89
2025,Dallas Wings,20.77,22.02
2025,Golden State Valkyries,19.18,18.68
2025,Indiana Fever,22.59,21.43
2025,Las Vegas Aces,21.09,20.14
2025,Los Angeles Sparks,21.89,23.00
2025,Minnesota Lynx,22.18,20.43
2025,New York Liberty,22.43,20.80
2025,Phoenix Mercury,20.93,20.14
2025,Seattle Storm,20.36,20.14
2025,Washington Mystics,18.61,20.43
2026,Atlanta Dream,22.10,20.68
2026,Chicago Sky,20.75,21.63
2026,Connecticut Sun,21.68,22.85
2026,Dallas Wings,21.60,20.78
2026,Golden State Valkyries,20.05,18.58
2026,Indiana Fever,25.43,22.88
2026,Las Vegas Aces,24.23,21.65
2026,Los Angeles Sparks,20.78,24.18
2026,Minnesota Lynx,22.63,21.08
2026,New York Liberty,22.93,21.10
2026,Phoenix Mercury,21.48,21.95
2026,Portland Fire,21.18,22.45
2026,Seattle Storm,20.35,23.78
2026,Toronto Tempo,21.60,22.80
2026,Washington Mystics,20.68,21.08", stringsAsFactors = FALSE) |>
  mutate(focus = case_when(
           yr == 2026 & team == "Los Angeles Sparks" ~ "la",
           (yr == 2020 & team == "Indiana Fever") | (yr == 2026 & team == "Seattle Storm") ~ "ref",
           TRUE ~ "other"))

q1_lab <- q1_hist |> filter(focus != "other") |>
  mutate(lab = case_when(focus == "la" ~ "2026 sparks, 24.2",
                         team == "Indiana Fever" ~ "2020 fever, 24.4",
                         TRUE ~ "2026 storm, 23.8"),
         nx = case_when(focus == "la" ~ -0.25, team == "Indiana Fever" ~ 0.25, TRUE ~ -0.25),
         hj = case_when(focus == "la" ~ 1, team == "Indiana Fever" ~ 0, TRUE ~ 1))

p_q1_history <- ggplot(q1_hist, aes(x = scored, y = allowed)) +
  # light red above the diagonal: allowed more than scored, i.e. losing the first quarter.
  # vertices stay inside the 15 to 26 limits, otherwise the scale drops them and the shape breaks
  annotate("polygon", x = c(15, 26, 15), y = c(15, 26, 26), fill = "#FBE3E1") +
  geom_abline(slope = 1, intercept = 0, linetype = "22", colour = GRAY, linewidth = 0.4) +
  annotate("text", x = 15.25, y = 25.55, hjust = 0, size = 3.5, colour = "#B5524A",
           label = "above the dashed line:\nlosing the first quarter") +
  geom_point(data = filter(q1_hist, focus == "other"), aes(colour = yr),
             size = 2, alpha = 0.75) +
  geom_point(data = filter(q1_hist, focus == "ref"), colour = INK, size = 2.8) +
  geom_point(data = filter(q1_hist, focus == "la"), colour = HI, size = 4.6) +
  geom_text(data = q1_lab, aes(label = lab, hjust = hj),
            nudge_x = q1_lab$nx, size = 3.9,
            colour = if_else(q1_lab$focus == "la", HI, INK)) +
  scale_colour_gradient(low = "#DEDDD8", high = "#6F6E6A", guide = "none") +
  # identical limits, breaks and aspect on both axes so a point is the same length either way
  scale_x_continuous(breaks = seq(15, 26, 1), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(15, 26, 1), minor_breaks = NULL) +
  # limits set on the coord, not the scales, with no padding: the panel stops exactly at
  # 15 and 26 so the red triangle fills its corner edge to edge. ratio = 1 keeps both axes equal
  coord_equal(xlim = c(15, 26), ylim = c(15, 26), expand = FALSE) +
  labs(title = "first quarter points, every team season since 2006",
       subtitle = "the 2026 sparks allowed the second most of 262 team seasons",
       caption = "wnba regular seasons 2006 to 2026, the quarters era  \u00b7  data: her hoop stats  |  chart: @wnbadata",
       x = "first quarter points scored per game", y = "first quarter points allowed per game") +
  base_theme() +
  theme(panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# 11. LEAGUE SCORING BY SEASON, 1997 to 2026. the era caveat for the "second most since
# 2006" line. her hoop stats team points per game, pulled 9/12, divided by 4 so it reads
# as points per quarter. that works across eras: the game was 40 minutes the whole time,
# two 20 minute halves through 2005 and four 10 minute quarters from 2006, so before 2006
# this is points per 10 minutes.
# 2006 also cut the shot clock from 30 seconds to 24, which is most of the jump.
# (wnba.com archive: "wnba announces rule changes for 2006 season")
league_scoring <- tibble::tribble(
  ~yr,  ~ppg,
  1997, 69.12, 1998, 70.25, 1999, 68.52, 2000, 68.45, 2001, 65.64,
  2002, 67.43, 2003, 68.03, 2004, 67.00, 2005, 67.20, 2006, 75.22,
  2007, 75.91, 2008, 76.26, 2009, 78.35, 2010, 80.32, 2011, 77.29,
  2012, 77.53, 2013, 75.65, 2014, 77.12, 2015, 75.14, 2016, 81.88,
  2017, 81.45, 2018, 82.80, 2019, 78.70, 2020, 83.06, 2021, 80.66,
  2022, 82.27, 2023, 82.74, 2024, 81.67, 2025, 81.70, 2026, 87.12
) |> mutate(per_q = ppg / 4, era = if_else(yr <= 2005, "halves", "quarters"))

p_scoring_era <- ggplot(league_scoring, aes(x = yr, y = per_q)) +
  annotate("rect", xmin = 1996.5, xmax = 2005.5, ymin = -Inf, ymax = Inf,
           fill = "#F1F0EC") +
  annotate("text", x = 2001, y = 22.6, size = 3.8, colour = GRAY,
           label = "two 20 minute halves\n30 second shot clock") +
  annotate("text", x = 2016, y = 22.6, size = 3.8, colour = GRAY,
           label = "four 10 minute quarters\n24 second shot clock") +
  geom_vline(xintercept = 2005.5, linetype = "22", colour = INK, linewidth = 0.5) +
  geom_line(colour = GRAY, linewidth = 1) +
  geom_point(aes(colour = yr == 2026), size = 2.6, show.legend = FALSE) +
  annotate("text", x = 2006.2, y = 18.55, hjust = 0, vjust = 1.6, size = 3.6, colour = INK,
           label = "+2 a quarter in one year") +
  scale_colour_manual(values = c(`TRUE` = HI, `FALSE` = INK), guide = "none") +
  scale_x_continuous(breaks = c(1997, 2005, 2010, 2015, 2020, 2026)) +
  scale_y_continuous(limits = c(15, 23.2), breaks = 15:23) +
  labs(title = "wnba points per team, per quarter, by season",
       subtitle = "points per 10 minutes",
       caption = "wnba regular seasons 1997 to 2026  \u00b7  data: her hoop stats  |  chart: @wnbadata",
       x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3),
        panel.grid.major.x = element_blank())

# ---------------------------------------------------------------------------
# 12. THE NEW HOOK, 9/12: two teams play worse defense than the sparks over a full game,
# and neither gives up as many first quarter points. her hoop stats 2026, pulled 9/12.
# drtg = points allowed per 100 possessions, whole game. q1 = first quarter points allowed
# per game. q2_4 = average points allowed per quarter in quarters 2 to 4.
# pace check: la averages 84.5 possessions, toronto 83.1, portland 83.2. about a third of
# a point per quarter, nowhere near the 1.4 to 1.7 point first quarter gap.
def_q1 <- tibble::tribble(
  ~team,          ~drtg, ~q1,   ~q2_4,
  "Toronto",      112.8, 22.80, 23.61,
  "Portland",     109.2, 22.45, 22.56,
  "Los Angeles",  108.4, 24.18, 22.51,
  "Phoenix",      106.6, 21.95, 21.66,
  "Connecticut",  106.3, 22.85, 21.56,
  "Indiana",      105.8, 22.88, 22.33,
  "New York",     105.0, 21.10, 21.73,
  "Dallas",       104.7, 20.78, 21.28,
  "Chicago",      104.6, 21.63, 22.45,
  "Seattle",      104.3, 23.78, 21.34,
  "Las Vegas",    103.6, 21.65, 21.52,
  "Atlanta",      103.0, 20.68, 21.56,
  "Washington",   101.0, 21.08, 20.13,
  "Minnesota",    100.5, 21.08, 20.78,
  "Golden State",  96.7, 18.58, 18.95
) |> mutate(focus = case_when(team == ME ~ "la",
                              team %in% c("Toronto", "Portland") ~ "worse d",
                              TRUE ~ "other"))

p_def_vs_q1 <- ggplot(def_q1, aes(x = drtg, y = q1)) +
  geom_point(data = filter(def_q1, focus == "other"), colour = GRAY, size = 3, alpha = 0.8) +
  geom_point(data = filter(def_q1, focus == "worse d"), colour = INK, size = 3.6) +
  geom_point(data = filter(def_q1, focus == "la"), colour = HI, size = 5) +
  geom_text(data = filter(def_q1, focus != "other"),
            aes(label = team, colour = focus), hjust = 1, nudge_x = -0.45, size = 4.2,
            show.legend = FALSE) +
  scale_colour_manual(values = c(la = HI, `worse d` = INK), guide = "none") +
  scale_x_continuous(breaks = seq(96, 114, 2)) +
  labs(title = "full game defense against first quarter points allowed, 2026",
       subtitle = "toronto and portland defend worse than los angeles. both give up fewer in the first",
       caption = CREDIT,
       x = "points allowed per 100 possessions, whole game (right is worse)",
       y = "first quarter points allowed per game") +
  base_theme() +
  theme(panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# 13. PER POSSESSION, 9/12. first quarter defense against quarters 2 to 4, every team.
# points allowed per 100 possessions, built from hhs possessions (poss) joined to the play
# by play (event) by event number, so pace is taken out entirely.
# check: la's first quarter points from this join are 920 in 38 games = 24.2, matching the box.
# coverage gaps: la, washington 38 games, chicago 37, toronto/new york/minnesota 39.
# la q1 107.6 is the worst first quarter defense in the league per possession.
# la q2-q4 102.4 is 4th worst, the rest of the league averages 100.8 in q2-q4.
drtg_q <- tibble::tribble(
  ~team,          ~q1,   ~q2_4,
  "Los Angeles",  107.6, 102.4,
  "Seattle",      106.5,  98.7,
  "Toronto",      105.2, 109.0,
  "Connecticut",  103.6, 101.9,
  "Indiana",      103.1, 100.1,
  "Portland",     100.2, 105.6,
  "Phoenix",      100.1, 102.7,
  "Washington",    99.6,  97.9,
  "Las Vegas",     99.5, 101.5,
  "New York",      98.6, 100.5,
  "Minnesota",     97.2,  96.2,
  "Chicago",       97.1, 102.1,
  "Dallas",        96.5, 101.9,
  "Atlanta",       94.2,  98.1,
  "Golden State",  93.0,  94.1
) |> mutate(mine = team == ME)

p_drtg_q <- ggplot(drtg_q, aes(y = reorder(team, q1))) +
  geom_segment(aes(x = q2_4, xend = q1, yend = reorder(team, q1), colour = mine),
               linewidth = 1.1, show.legend = FALSE) +
  geom_point(aes(x = q2_4, colour = mine), shape = 21, fill = "white", size = 3.2,
             stroke = 1.1, show.legend = FALSE) +
  geom_point(aes(x = q1, colour = mine), size = 3.8, show.legend = FALSE) +
  geom_vline(xintercept = 100.8, linetype = "22", colour = GRAY, linewidth = 0.4) +
  annotate("text", x = 100.8, y = 0.6, hjust = -0.05, vjust = 0, size = 3.4, colour = GRAY,
           label = "league, quarters 2 to 4") +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  labs(title = "points allowed per 100 possessions, first quarter vs the rest",
       subtitle = "filled is the first quarter, hollow is quarters 2 to 4",
       caption = CREDIT, x = "points allowed per 100 possessions (right is worse)", y = NULL) +
  base_theme()

# ---------------------------------------------------------------------------
# 14. THE NEW ENDING, 9/12: what goes wrong in the first quarter is the paint.
# same possession/event join as drtg_q, first quarter only. "rim" = two point attempts whose
# play by play description says layup or dunk. blocks = misses described as "X blocks ...",
# per 100 first quarter possessions.
# la: opponents make 58.4% of twos (worst, portland next 53.7), 65.7% at the rim (worst,
# connecticut next 63.1), and la blocks 3.3 per 100 (fewest, toronto next 3.5).
# not the problem: steals 8.1 per 100 (league 8.8), turnovers forced 16.7 (16.3), and
# offensive rebounds allowed are slightly BELOW league in the first quarter.
q1_paint <- tibble::tribble(
  ~team,          ~opp_2p, ~opp_rim, ~blk_100,
  "Los Angeles",   58.4,    65.7,    3.3,
  "Portland",      53.7,    58.4,    4.5,
  "Atlanta",       53.3,    60.7,    4.6,
  "Seattle",       53.1,    58.7,    4.4,
  "Connecticut",   53.0,    63.1,    6.4,
  "Minnesota",     52.7,    58.4,    5.4,
  "Indiana",       52.4,    59.1,    4.3,
  "New York",      52.1,    60.2,    5.3,
  "Toronto",       52.0,    61.3,    3.5,
  "Las Vegas",     51.6,    59.6,    6.1,
  "Dallas",        51.1,    56.4,    4.9,
  "Phoenix",       49.9,    56.7,    4.6,
  "Golden State",  49.7,    56.2,    4.9,
  "Washington",    49.5,    54.2,    4.3,
  "Chicago",       47.5,    58.4,    5.4
)

p_q1_2p <- lolli(q1_paint, opp_2p, "%.1f%%",
  "opponent two point percentage in the first quarter, by team",
  "portland is second at 53.7%")

# ---------------------------------------------------------------------------
# 15. PER 100 POSSESSIONS, BY QUARTER, la against the rest of the league. 9/12.
# same poss/event join as drtg_q. la has possession data for 38 of 40 games.
# gap to league: q1 +7.9, q2 +1.8, q3 +3.0, q4 +0.2. by the fourth la is league average.
drtg_by_q <- tibble::tribble(
  ~q, ~who,          ~per100,
  1,  "sparks",       107.6,
  2,  "sparks",        99.8,
  3,  "sparks",       104.6,
  4,  "sparks",       102.9,
  1,  "rest of league", 99.7,
  2,  "rest of league", 98.0,
  3,  "rest of league", 101.6,
  4,  "rest of league", 102.7
)

p_drtg_by_q <- ggplot(drtg_by_q, aes(x = q, y = per100, colour = who, group = who)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 3.8) +
  geom_text(data = filter(drtg_by_q, who == "sparks"),
            aes(label = sprintf("%.1f", per100)), vjust = -1.2, size = 4, show.legend = FALSE) +
  geom_text(data = filter(drtg_by_q, who != "sparks"),
            aes(label = sprintf("%.1f", per100)), vjust = 2, size = 3.7, show.legend = FALSE) +
  scale_colour_manual(values = c(sparks = HI, `rest of league` = GRAY), name = NULL) +
  scale_x_continuous(breaks = 1:4, labels = c("1st quarter", "2nd", "3rd", "4th")) +
  scale_y_continuous(limits = c(95, 110)) +
  labs(title = "points allowed per 100 possessions, by quarter",
       subtitle = "the sparks are 7.9 worse than the league in the first quarter and 0.2 worse in the fourth",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# 16. "TEAMS JUST MAKE EVERYTHING" in the first quarter. opponent effective fg% in the first
# quarter, every defense. same poss/event join, 9/12. la 56.9 is the highest, connecticut 54.8
# next, rest of league 51.9. replaces "it's the paint" as the explanation, because opponent 2pt%
# against la is just as bad in the 4th quarter (58.1) when their defense is league average.
q1_efg <- tibble::tribble(
  ~team,          ~opp_efg,
  "Los Angeles",   56.9,
  "Connecticut",   54.8,
  "Atlanta",       53.7,
  "Indiana",       52.9,
  "Portland",      52.9,
  "Seattle",       52.8,
  "Washington",    52.3,
  "Las Vegas",     52.2,
  "New York",      52.2,
  "Minnesota",     51.8,
  "Toronto",       51.3,
  "Phoenix",       51.1,
  "Dallas",        50.6,
  "Chicago",       49.2,
  "Golden State",  48.8
)

p_q1_efg <- lolli(q1_efg, opp_efg, "%.1f%%",
  "first quarter opponent effective fg%, by defense",
  "connecticut is second at 54.8%. the rest of the league is 51.9%")

# ---------------------------------------------------------------------------
# 17. SIMPLE BARS: first quarter points allowed per game, every team. same numbers as
# p_opp_q1 (qtr$o1, computed live from wehoop, matches her hoop stats), just plain bars.
# league average first quarter goes in the subtitle.
q1_bars <- qtr |> transmute(team, o1, mine = team == ME)
LG_Q1 <- mean(q1_bars$o1)

p_q1_bars <- ggplot(q1_bars, aes(x = o1, y = reorder(team, o1), fill = mine)) +
  geom_col(width = 0.72, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f", o1)), hjust = 1, nudge_x = -0.25,
            colour = "white", size = 3.9) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = GRAY), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.04))) +
  labs(title = "points allowed in the first quarter, per game",
       subtitle = sprintf("league average is %.1f", LG_Q1), caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# 18. BARS: sparks points allowed per 100 possessions in each quarter, from drtg_by_q.
# the short gray line across each bar is the rest of the league in that quarter.
# bars start at zero, so the gaps look small on purpose: 107.6 vs 99.7 is still 7.9 points.
drtg_bars <- drtg_by_q |>
  pivot_wider(names_from = who, values_from = per100) |>
  mutate(ql = factor(c("1st quarter", "2nd", "3rd", "4th")[q],
                     levels = c("1st quarter", "2nd", "3rd", "4th")))

p_drtg_bars <- ggplot(drtg_bars, aes(x = ql)) +
  geom_col(aes(y = sparks), fill = HI, width = 0.66) +
  geom_errorbar(aes(ymin = `rest of league`, ymax = `rest of league`),
                width = 0.8, colour = GRAY, linewidth = 1.2) +
  geom_text(aes(y = sparks, label = sprintf("%.1f", sparks)), vjust = -0.6,
            colour = INK, size = 4.4) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "sparks points allowed per 100 possessions, by quarter",
       subtitle = "gray line is the league average in that quarter",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# 19. TABLE instead of p_after1: la's record by how the first quarter ended, next to how
# often every wnba team has won in that spot since 2006 (hhs, 2006 to 2025 regular season,
# led 2,788 of 4,025, trailed 1,237 of 4,025, tied 237 of 474).
# renders in the positron viewer, not the plots pane.
# hardcoded from hhs, not computed from wehoop: wehoop play by play stops at regulation, so
# the one la game tied after the first quarter that went to overtime (a win) scored as a loss.
after1_tbl <- tibble::tribble(
  ~`after the first quarter`, ~games, ~record, ~la_pct, ~hist_pct,
  "trailing",                  27,     "6-21",  6 / 27,  1237 / 4025,
  "tied",                       3,     "2-1",   2 / 3,    237 / 474,
  "leading",                   10,     "7-3",   7 / 10,  2788 / 4025
)

t_after1 <- after1_tbl |>
  gt() |>
  cols_label(games = "sparks games", record = "sparks record",
             la_pct = "sparks win %", hist_pct = "every team since 2006") |>
  fmt_percent(columns = c(la_pct, hist_pct), decimals = 0) |>
  cols_align("center", columns = -`after the first quarter`) |>
  tab_style(style = list(cell_fill(color = "#EFE9F5"), cell_text(weight = "bold")),
            locations = cells_body(rows = `after the first quarter` == "trailing")) |>
  tab_header(title = "sparks record by how the first quarter ended") |>
  tab_source_note(CREDIT) |>
  tab_options(table.font.names = "Helvetica", table.font.size = px(18),
              heading.align = "left", heading.title.font.size = px(24),
              column_labels.font.weight = "bold", table.border.top.color = "white")

# ---------------------------------------------------------------------------
# 20. CAM BRINK ON/OFF, 9/13. sparks defense per 100 possessions with brink on the floor vs off.
# her hoop stats possession data (period, defending team, lineup on court) to tag brink,
# points from the event table. la has possession data for 38 of 40 games.
# ON/OFF IS NOT ADJUSTED for opponent or for the other four players, so this shows the
# defense is better WHEN she plays, not that she is the reason.
# team context: ariel atkins' on/off is actually bigger (101.1 on, 108.2 off, -7.1) than brink's
# (100.2 on, 105.8 off, -5.6). rae burrell is the other way (106.1 on, 98.3 off).
brink_onoff <- tibble::tribble(
  ~span,              ~on,   ~off,  ~poss_on, ~poss_off,
  "first quarter",    104.3, 109.5,  305,      550,
  "quarters 2 to 4",   98.9, 104.5,  927,     1560,
  "whole game",       100.2, 105.8, 1232,     2110
) |> mutate(span = factor(span, levels = rev(span)))

p_brink_onoff <- ggplot(brink_onoff, aes(y = span)) +
  geom_segment(aes(x = on, xend = off, yend = span), colour = GRAY, linewidth = 1.2) +
  geom_point(aes(x = off), shape = 21, fill = "white", colour = INK, size = 4.2, stroke = 1.2) +
  geom_point(aes(x = on), colour = HI, size = 4.8) +
  geom_text(aes(x = off, label = sprintf("%.1f", off)), vjust = -1.3, colour = INK, size = 4) +
  geom_text(aes(x = on, label = sprintf("%.1f", on)), vjust = -1.3, colour = HI, size = 4) +
  scale_x_continuous(limits = c(96, 112), breaks = seq(96, 112, 4)) +
  labs(title = "sparks defense with and without cam brink",
       subtitle = "points allowed per 100 possessions. purple is brink on the floor, hollow is off",
       caption = CREDIT, x = "points allowed per 100 possessions (right is worse)", y = NULL) +
  base_theme() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#E3E3E0", linewidth = 0.3))

# the easiest visual evidence of why: blocks per 100 defensive possessions, whole game.
# includes her own blocks, which is the point.
brink_blocks <- tibble::tribble(
  ~who,            ~blk100,
  "brink on",       5.7,
  "brink off",      2.4
) |> mutate(who = factor(who, levels = who))
# first quarter only: 5.9 on, 1.8 off. quarters 2 to 4: 5.7 on, 2.6 off.
# whole game off value is (1.8*550 + 2.6*1560) / 2110 = 2.4; on is (5.9*305 + 5.7*927) / 1232 = 5.7

p_brink_blocks <- ggplot(brink_blocks, aes(x = who, y = blk100, fill = who == "brink on")) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f", blk100)), vjust = -0.6, colour = INK, size = 4.6) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = GRAY), guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.12))) +
  labs(title = "sparks blocks per 100 defensive possessions",
       subtitle = "more than twice as many with cam brink on the floor",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# 21. TABLE version of p_brink_onoff, same numbers from brink_onoff. matches t_after1's style.
t_brink_onoff <- brink_onoff |>
  mutate(span = factor(span, levels = c("first quarter", "quarters 2 to 4", "whole game"))) |>
  arrange(span) |>
  transmute(span = as.character(span), on, off, diff = on - off, poss_on) |>
  gt() |>
  cols_label(span = "", on = "brink on the floor", off = "brink off",
             diff = "difference", poss_on = "possessions with brink") |>
  fmt_number(columns = c(on, off), decimals = 1) |>
  fmt_number(columns = diff, decimals = 1, force_sign = TRUE) |>
  fmt_number(columns = poss_on, decimals = 0, use_seps = TRUE) |>
  cols_align("center", columns = -span) |>
  tab_style(style = cell_text(color = HI, weight = "bold"),
            locations = cells_body(columns = c(on, diff))) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = span)) |>
  tab_header(title = "sparks defense with and without cam brink",
             subtitle = "points allowed per 100 possessions. lower is better") |>
  tab_source_note(CREDIT) |>
  tab_options(table.font.names = "Helvetica", table.font.size = px(18),
              heading.align = "left", heading.title.font.size = px(24),
              heading.subtitle.font.size = px(15),
              column_labels.font.weight = "bold", table.border.top.color = "white")

# ---------------------------------------------------------------------------
# ALL THE CHARTS. run any of these on its own to render it.
#
# charts, in order
p_def_vs_q1  # 0. NEW HOOK. full game defense vs first quarter allowed. toronto and portland defend worse, allow less
p_q1_bars    # 1-alt. same as p_opp_q1 but plain bars, league average line
p_opp_q1     # 1. first quarter points allowed, all 15 teams. la is highest at 24.2
p_drtg_bars  # 1f. same as p_drtg_by_q as bars, one per quarter, league as a gray line
p_drtg_by_q  # 1e. la vs league per 100 possessions, each quarter. +7.9 in q1, +0.2 in q4
p_drtg_q     # 1d. per possession, q1 vs q2-q4 for every team. la worst q1 at 107.6, 102.4 after
p_q1_efg     # 5. ENDING. "teams just make everything": first quarter opponent efg%. la 56.9, next 54.8
p_q1_2p      # 5b. (secondary now) opponent 2pt% in the first quarter. la 58.4, next is portland 53.7
p_q1_history # 1b. every team season since 2006, scored vs allowed. la 2026 is 2nd of 262
p_scoring_era # 1c. league scoring per quarter since 1997. halves to quarters in 2006, 2026 is the record
p_la_qtr     # 2. la scored against la allowed, by quarter, with the league average line
p_trail      # 3. games trailing after the first quarter. la 27, most in the league
t_brink_onoff  # 6. ENDING. TABLE. defense per 100 with brink on vs off: q1, q2-4, whole game
p_brink_onoff  # 6-old. dumbbell version of the same numbers
p_brink_blocks # 6b. blocks per 100 with brink on (5.7) vs off (2.4)
t_after1     # 4. TABLE. record when trailing, tied, leading after one, next to every team since 2006
p_after1     # 4-old. the bar version maddy didn't like
p_winprob    # 4b. history since 2005: win rate by first quarter margin, la's average marked
p_no_q1      # 5. CLOSER. fake standings with every first quarter deleted. seattle +12, la +2
#
# built but not used in the final video
p_qtr        # point differential by quarter. the old spine, before the rebuild
p_ast        # assist rate by team. cut 9/12, chicago is 1st and was the day 9 video
p_q1         # first quarter differential. la -3.40 vs seattle -3.425, too close to claim
#
# tibbles
# qtr         every team's scored and allowed by quarter, q1..q4 and o1..o4
# trail       games trailed and led after one, with the records
# no_q1       fake standings with every first quarter deleted. seattle +12, la +2
# wp          win rate by first quarter margin, 2006 to 2025, with game counts
# q1_hist     first quarter scored and allowed, all 262 team seasons since 2006
# def_q1      2026 full game drtg, q1 allowed and q2-q4 allowed for all 15 teams
# drtg_q      per possession defense, q1 and q2-q4, all 15 teams
# drtg_by_q   la and rest of league per 100 possessions in each quarter
# q1_paint    first quarter opponent 2pt%, rim%, and blocks per 100 poss, all 15 teams
# q1_efg      first quarter opponent effective fg%, all 15 defenses
# brink_onoff sparks defense per 100 with brink on/off, with possession counts
# league_scoring  league points per game by season, 1997 to 2026
# teams       every team's 2026 shooting, passing and record
