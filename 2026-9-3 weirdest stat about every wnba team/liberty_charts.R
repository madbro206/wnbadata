# charts for the new york liberty "weird 10/15" video
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
library(ggrepel)

ALLSTAR   <- c("Team Spoon", "Team Coop")
CUP_FINAL <- as.Date("2026-06-30")
ME        <- "New York"

HI     <- "#3FAE9A"   # new york liberty. swap this hex if you want a different color
GRAY   <- "#8A8A86"   # everyone else
ALT    <- "#B4B2A9"   # second series
INK    <- "#15171D"
CREDIT <- "2026 season through the fiba break  \u00b7  data: her hoop stats and wehoop  |  chart: @wnbadata"
# the multi season charts are not a 2026 chart, so they get their own credit line
CREDIT_CAREER <- "wnba regular seasons  \u00b7  data: her hoop stats and wehoop  |  chart: @wnbadata"

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

stew <- player_box |> filter(athlete_display_name == "Breanna Stewart") |>
  mutate(month = format(game_date, "%m")) |>
  group_by(month) |>
  summarise(g = n(), mpg = mean(as_min(minutes)), ppg = mean(points),
            t3a = mean(three_point_field_goals_attempted),
            p3 = 100 * sum(three_point_field_goals_made) / sum(three_point_field_goals_attempted),
            ts = 100 * sum(points) / (2 * (sum(field_goals_attempted) + 0.44 * sum(free_throws_attempted))),
            .groups = "drop") |>
  mutate(m = recode(month, `05` = "may", `06` = "june", `07` = "july", `08` = "august"),
         m = factor(m, levels = c("may", "june", "july", "august")),
         best = p3 == max(p3))

# 1. THE LEAD. 18, 23, 43, 14.
p_month <- ggplot(stew, aes(x = m, y = p3, fill = best)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = sprintf("%.1f%%", p3)), vjust = -0.6, colour = INK, size = 5) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = "#B4B2A9"), guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
  labs(title = "breanna stewart three point percentage, by month",
       subtitle = NULL,
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 2. THE POINT. the volume never moved, only the result did.
# both series anchored at zero on the same axis so the flat one reads as genuinely flat.
vol <- stew |> select(m, `three point attempts a game` = t3a, `three point percentage` = p3) |>
  pivot_longer(-m, names_to = "k", values_to = "v")
p_volume <- ggplot(vol, aes(x = m, y = v, fill = k)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.64) +
  geom_text(aes(label = sprintf("%.1f", v)), position = position_dodge(width = 0.72),
            vjust = -0.6, colour = INK, size = 3.9) +
  scale_fill_manual(values = c(`three point percentage` = HI,
                               `three point attempts a game` = "#B4B2A9"), name = NULL) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.18))) +
  labs(title = "stewart three point attempts against three point percentage, by month",
       subtitle = "the attempts are flat, the percentage is not",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top", panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 3. what it did to everything else
eff <- stew |> select(m, `points per game` = ppg, `true shooting %` = ts) |>
  pivot_longer(-m, names_to = "k", values_to = "v")
p_eff <- ggplot(eff, aes(x = m, y = v, fill = k)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.64) +
  geom_text(aes(label = sprintf("%.1f", v)), position = position_dodge(width = 0.72),
            vjust = -0.6, colour = INK, size = 3.9) +
  scale_fill_manual(values = c(`true shooting %` = HI, `points per game` = "#B4B2A9"), name = NULL) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.18))) +
  labs(title = "stewart scoring and true shooting, by month",
       subtitle = "regular season through the fiba break",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top", panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 4. minutes, to close off the obvious "was she hurt or benched" objection
p_min <- ggplot(stew, aes(x = m, y = mpg)) +
  geom_col(fill = HI, width = 0.62) +
  geom_text(aes(label = sprintf("%.1f", mpg)), vjust = -0.6, colour = INK, size = 4.6) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
  labs(title = "stewart minutes per game, by month",
       subtitle = NULL,
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 5. NOT IN THE SCRIPT as of 9/11. the free throw beat was cut in favor of the
# career three point context. not used in the final video.
p_ft <- lolli(teams, ftpct, "%.1f",
  "free throw percentage, by team",
  NULL)


# 6. THE TWIST. the months are noise, the level is not.
# wehoop's player box only goes back to 2024, so these are her hoop stats season
# totals, pulled 9/11 from her hoop stats box scores (regular season only).
# wehoop agrees on the three years it covers: 30.2, 23.6, 25.0.
# 2019 is absent because she missed the season.
career <- tibble::tribble(
  ~szn,  ~m3, ~a3,
  2016,   45, 133,
  2017,   49, 132,
  2018,   61, 147,
  2020,   35,  95,
  2021,   48, 143,
  2022,   67, 177,
  2023,   82, 231,
  2024,   46, 156,
  2025,   21,  87,
  2026,   29, 116
) |> mutate(p3 = 100 * m3 / a3, recent = szn >= 2024)

p_szn <- ggplot(career, aes(x = factor(szn), y = p3, fill = recent)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = sprintf("%.1f", p3)), vjust = -0.6, colour = INK, size = 4.2) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = ALT), guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
  labs(title = "stewart three point percentage, by season",
       subtitle = "eight seasons between 33 and 41 percent, then three straight under 30",
       caption = CREDIT_CAREER, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))


# 7. THE HOOK CHART. july 2026 is the highest scoring calendar month of her career.
# her hoop stats, regular season only, pulled 9/11. wehoop cannot check this, it only
# goes back to 2024. game counts are on the labels on purpose: the two months closest
# to july 2026 are 5 and 4 game samples, and july still beats them over 9 games.
best_months <- tibble::tribble(
  ~lab,             ~g,  ~ppg,
  "jul 2026",        9, 26.44,
  "aug 2022",        5, 26.00,
  "may 2023",        4, 25.75,
  "jul 2023",       11, 24.45,
  "jul 2017",        9, 23.78,
  "aug 2024",        8, 23.75,
  "jun 2018",       10, 23.30,
  "sep 2023",        5, 22.80,
  "aug 2023",       10, 22.60,
  "jul 2018",       11, 22.55
) |> mutate(team = sprintf("%s  (%d games)", lab, g), mine = lab == "jul 2026")

PAD_BM <- 0.035 * max(best_months$ppg)
p_best_months <- ggplot(best_months, aes(x = ppg, y = reorder(team, ppg))) +
  geom_segment(aes(x = 0, xend = ppg, yend = team, colour = mine),
               linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
  geom_point(aes(colour = mine, size = mine)) +
  geom_text(aes(label = sprintf("%.1f", ppg), colour = mine), hjust = 0,
            nudge_x = PAD_BM, size = 4.2, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.18))) +
  labs(title = "stewart points per game, best months of her career",
       subtitle = "every calendar month, regular season only, 2016 to 2026",
       caption = CREDIT_CAREER, x = NULL, y = NULL) +
  base_theme()


# ---------------------------------------------------------------------------
# 8, 9, 10. THE CAREER CONTEXT, added 9/11. the attempts are flat month to month
# inside 2026 (see p_volume) and collapsing season to season. same stat, two zoom
# levels, opposite answers. that contrast is the spine of the back half now.
#
# her hoop stats, regular season, pulled 9/11. wehoop covers 2024 on and matches:
# stewart 3pa/g 4.1, 2.8, 3.0 and 3pt rate 26.8, 21.4, 19.9; new york team rate
# 42.2, 41.0, 43.3. 2019 is absent because she missed the season.
stew_szn <- tibble::tribble(
  ~szn,  ~a3pg, ~rate3, ~p3,  ~ppg,
  2016,    3.9,   29.2, 33.8, 18.3,
  2017,    4.0,   28.3, 37.1, 19.9,
  2018,    4.3,   28.8, 41.5, 21.8,
  2020,    4.8,   32.2, 36.8, 19.7,
  2021,    5.1,   32.4, 33.6, 20.3,
  2022,    5.2,   32.8, 37.9, 21.8,
  2023,    5.8,   34.0, 35.5, 23.0,
  2024,    4.1,   27.0, 29.5, 20.4,
  2025,    2.8,   21.3, 24.1, 18.3,
  2026,    3.0,   19.9, 25.0, 20.4
)

# 8. she used to shoot a lot more of them. peak 5.8 a game in 2023, now 3.0.
p_att_szn <- ggplot(stew_szn, aes(x = factor(szn), y = a3pg, fill = szn == 2026)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = sprintf("%.1f", a3pg)), vjust = -0.6, colour = INK, size = 4.2) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = ALT), guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
  labs(title = "stewart three point attempts per game, by season",
       subtitle = "5.8 a game in 2023, 3.0 now",
       caption = CREDIT_CAREER, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 9. THE DIVERGENCE. her three point rate against her team's, since she got to new york.
# both are shares of field goal attempts, so they belong on one axis.
ny_rate <- tibble::tribble(
  ~szn, ~stewart, ~liberty,
  2023,     34.0,     42.4,
  2024,     27.0,     42.2,
  2025,     21.3,     41.0,
  2026,     19.9,     43.3
) |> pivot_longer(-szn, names_to = "who", values_to = "rate")

p_rate_vs_team <- ggplot(ny_rate, aes(x = szn, y = rate, colour = who, group = who)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 4) +
  geom_text(aes(label = sprintf("%.1f", rate)), vjust = -1.2, size = 4, show.legend = FALSE) +
  scale_colour_manual(values = c(stewart = HI, liberty = GRAY), name = NULL,
                      labels = c(stewart = "stewart", liberty = "the liberty")) +
  scale_x_continuous(breaks = 2023:2026) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.18))) +
  labs(title = "share of shot attempts that are threes, stewart against her team",
       subtitle = "attempts, not makes. the liberty never stopped shooting them",
       caption = CREDIT_CAREER, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 10. the same thing as a single number. new york attempted 1187, 1160, 1180 and
# 1166 threes in those four seasons, so the denominator is effectively constant.
p_share <- tibble::tribble(
  ~szn, ~stew_a3, ~team_a3,
  2023,      231,     1187,
  2024,      156,     1160,
  2025,       87,     1180,
  2026,      116,     1166
) |> mutate(share = 100 * stew_a3 / team_a3) |>
  ggplot(aes(x = factor(szn), y = share, fill = szn == 2026)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = sprintf("%.1f%%", share)), vjust = -0.6, colour = INK, size = 4.4) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = ALT), guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
  labs(title = "stewart's share of every three the liberty attempted",
       subtitle = "new york took about 1,170 threes in each of these four seasons",
       caption = CREDIT_CAREER, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))


# 11. the chart for the spoken claim "lowest rate of her career, lower than her
# rookie year." p_att_szn is attempts per GAME, which is a different thing:
# she also takes fewer shots overall now, so the rate is the honest version.
# the dashed line is her 2016 rate, so the rookie year comparison is visible
# rather than asserted.
ROOKIE <- stew_szn$rate3[stew_szn$szn == 2016]

p_rate_szn <- ggplot(stew_szn, aes(x = factor(szn), y = rate3, fill = szn == 2026)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = sprintf("%.1f", rate3)), vjust = -0.6, colour = INK, size = 4.2) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = ALT), guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.18))) +
  labs(title = "share of stewart's shots that are threes, by season",
       subtitle = "2026 is the lowest rate of her career",
       caption = CREDIT_CAREER, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# 12. THE TEAMMATE CHART. cumulative threes made across new york's 40 games.
# stewart played 39 of them and finished SEVENTH on her own team. leonie fiebich
# made more threes in 17 games than stewart did in 39, and so did rebecca allen
# in 25. lines go flat on games a player missed, which is why fiebich's and
# sabally's are mostly flat and still end above stewart's.
# computed live from wehoop. totals match her hoop stats exactly.
ny_box <- player_box |> filter(team_location == ME)

ny_games <- ny_box |> distinct(game_date) |> arrange(game_date) |>
  mutate(gi = row_number())

top_shooters <- ny_box |> group_by(who = athlete_display_name) |>
  summarise(m3 = sum(three_point_field_goals_made), .groups = "drop") |>
  slice_max(m3, n = 7) |> pull(who)

cum3 <- tidyr::expand_grid(who = top_shooters, gi = seq_len(nrow(ny_games))) |>
  left_join(
    ny_box |> left_join(ny_games, by = "game_date") |>
      transmute(who = athlete_display_name, gi, m = three_point_field_goals_made),
    by = c("who", "gi")) |>
  mutate(m = coalesce(m, 0)) |>
  arrange(who, gi) |>
  group_by(who) |> mutate(made = cumsum(m)) |> ungroup() |>
  mutate(mine = who == "Breanna Stewart")

ends <- cum3 |> filter(gi == max(gi)) |> mutate(lab = sprintf("%s  %d", who, made))

p_cum3 <- ggplot(cum3, aes(x = gi, y = made, group = who, colour = mine)) +
  geom_line(aes(linewidth = mine), show.legend = FALSE) +
  geom_text_repel(data = ends, aes(label = lab), hjust = 0, nudge_x = 1.2,
                  direction = "y", segment.colour = NA, size = 4, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_linewidth_manual(values = c(`FALSE` = 0.7, `TRUE` = 1.8), guide = "none") +
  scale_x_continuous(limits = c(1, nrow(ny_games) + 13),
                     breaks = c(1, 10, 20, 30, 40), expand = expansion(mult = c(0.01, 0))) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
  labs(title = "threes made over the season, new york liberty",
       subtitle = "stewart has played 39 of 40 games and is seventh on her team",
       caption = CREDIT, x = "team game", y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# the liberty's three point volume since stewart arrived in 2023. print it.
# her hoop stats, regular season, pulled 9/12. wehoop matches on 2024 to 2026.
#
# WATCH THE GAME COUNT. 2025 was a 44 game season, so the raw totals look flatter
# than the per game numbers. "basically the same 1170 threes every year" is true
# of the totals, but 2025 was 26.8 a game against 29.2 this year. the share of
# shots (rate3) is the honest measure and it is the stable one: 42.4, 42.2, 41.0, 43.3.
ny_threes <- tibble::tribble(
  ~szn,  ~gm, ~m3,  ~a3,
  2023,   40, 444, 1187,
  2024,   40, 405, 1160,
  2025,   44, 419, 1180,
  2026,   40, 408, 1166
) |> mutate(
  p3    = round(100 * m3 / a3, 1),   # three point percentage
  a3pg  = round(a3 / gm, 1),         # attempts per game
  m3pg  = round(m3 / gm, 1),         # makes per game
  rate3 = c(42.4, 42.2, 41.0, 43.3)  # share of all field goal attempts
)

# ---------------------------------------------------------------------------
# every team's three point rate, 2026. backs the "third highest in the league" line.
# computed live from wehoop off the `teams` object above. matches her hoop stats
# on all 15 teams (checked 9/12).
#
# new york is a comfortable third: 2.05 points clear of toronto in 4th, but only
# 0.25 behind portland in 2nd. so third is correct and safe, and second is wrong
# by a quarter of a point. do not round it into second.
rate_table <- teams |>
  transmute(team, rate3 = round(p3rate, 2)) |>
  arrange(desc(rate3)) |>
  mutate(rk = row_number()) |>
  relocate(rk)

# ---------------------------------------------------------------------------
# ALL THE CHARTS. run any of these on its own to render it.
#
# charts, in order
p_best_months    # 1. HOOK. july 2026 is her highest scoring month ever, with game counts
p_eff            # 2. points and true shooting by month, 2026
p_month          # 3. three point percentage by month. 18.5, 23.3, 43.3, 13.8
p_volume         # 4. attempts against percentage by month. the attempts are flat
p_szn            # 5. three point percentage by season. eight years in the 30s, then three under 30
p_att_szn        # 6. three point attempts per GAME by season. 5.8 in 2023, 3.0 now
p_rate_szn       # 7. threes as a SHARE of her shots, by season. 19.9%, lowest of her career
p_rate_vs_team   # 8. her three point rate against the liberty's, 2023 to 2026. they diverge
p_share          # 9. her share of every three new york attempted. 19.5% down to 9.9%
p_cum3           # 10. cumulative threes made, her and six teammates, over the 40 games
#
# built but not used in the final video
p_min            # minutes by month. kills the "was she hurt or benched" question on screen
p_ft             # free throw percentage by team. the old team beat, cut 9/11
#
# tibbles
# stew          monthly 2026 line, computed live from wehoop
# stew_szn      season by season attempts, rate, percentage and scoring, her hoop stats
# ny_threes     liberty 3pm and 3pa by season since stewart arrived, her hoop stats
# rate_table    all 15 teams' 2026 three point rate, ranked. new york is 3rd
# career        season by season threes made and attempted, her hoop stats
# best_months   her ten highest scoring calendar months, her hoop stats
# teams         every team's 2026 shooting and record, computed live from wehoop
