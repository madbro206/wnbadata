# charts for the portland fire "weird 12/15" video
#
# everything here computes live from wehoop (espn) and was cross checked against
# her hoop stats on 9/5 using final numbers through 8/30.
# the 2000-2002 fire seasons are hardcoded from hhs. wehoop does not go back that far.\n#
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
ME        <- "Portland"

HI     <- "#C8102E"   # portland fire. swap this hex if you want a different color
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

# the 2000, 2001 and 2002 portland fire come from her hoop stats. wehoop's wnba team box
# does not reach back that far, so those four rows are hardcoded and cannot be cross
# checked against wehoop. the 2026 row computes live and DOES match hhs exactly.
fire <- tibble::tribble(
  ~season, ~w,  ~l,  ~mov,   ~ppg,  ~fg3a, ~p3rate, ~p3pct, ~efg,
  "2000",   10,  22, -4.34,  67.3,  13.5,  23.7,    33.5,   45.6,
  "2001",   11,  21, -3.56,  65.3,  16.2,  27.4,    33.1,   43.3,
  "2002",   16,  16, -2.03,  67.6,  12.2,  20.2,    32.4,   45.6,
  "2026",   16,  24, -4.48,  86.6,  29.7,  43.6,    34.8,   52.8) |>
  mutate(now = season == "2026")

fbar <- function(yvar, fmt, title, subtitle) {
  ggplot(fire, aes(x = season, y = {{ yvar }}, fill = now)) +
    geom_col(width = 0.62) +
    geom_text(aes(label = sprintf(fmt, {{ yvar }})), vjust = -0.6, colour = INK, size = 4.6) +
    scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = "#B4B2A9"), guide = "none") +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
    labs(title = title, subtitle = subtitle, caption = CREDIT, x = NULL, y = NULL) +
    base_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))
}

# 1. THE LEAD. nineteen more points a night than the team that had this name before.
p_ppg <- fbar(ppg, "%.1f",
  "portland fire points per game, by season",
  "the original fire folded after 2002 and the name came back this year")

# 2. three point rate, fire against the league average each season. 9/14: the fire-only
# version made it look like portland fell in love with the three, but the whole league did.
# league rate includes portland. her hoop stats, regular season.
# fire rank: 2000 4th of 16, 2001 3rd of 16, 2002 12th of 16 (below league), 2026 2nd of 15.
rate3_vs_lg <- tibble::tribble(
  ~season, ~fire,  ~league,
  "2000",   23.69,  21.59,
  "2001",   27.45,  22.08,
  "2002",   20.18,  22.42,
  "2026",   43.56,  37.10
) |> pivot_longer(-season, names_to = "who", values_to = "rate") |>
  mutate(who = factor(if_else(who == "fire", "portland fire", "league average"),
                      levels = c("league average", "portland fire")))

p_3rate <- ggplot(rate3_vs_lg, aes(x = season, y = rate, fill = who)) +
  geom_col(position = position_dodge(width = 0.74), width = 0.66) +
  geom_text(aes(label = sprintf("%.1f", rate)), position = position_dodge(width = 0.74),
            vjust = -0.6, colour = INK, size = 4.2) +
  scale_fill_manual(values = c(`portland fire` = HI, `league average` = ALT), name = NULL) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.14))) +
  labs(title = "three point rate, portland fire vs the league",
       subtitle = "share of shot attempts taken from three. the whole league shoots more now",
       caption = "wnba regular seasons  \u00b7  data: her hoop stats and wehoop  |  chart: @wnbadata",
       x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top", panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 3. and they are not better at it, they just do it constantly.
# both series on one zero anchored panel so the flat line reads against the climbing one.
vol <- fire |> select(season, `three point rate` = p3rate, `three point percentage` = p3pct) |>
  pivot_longer(-season, names_to = "k", values_to = "v")
p_vol <- ggplot(vol, aes(x = season, y = v, fill = k)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.64) +
  geom_text(aes(label = sprintf("%.1f", v)), position = position_dodge(width = 0.72),
            vjust = -0.6, colour = INK, size = 3.8) +
  scale_fill_manual(values = c(`three point rate` = HI, `three point percentage` = "#B4B2A9"), name = NULL) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
  labs(title = "portland fire three point rate against three point percentage",
       subtitle = "the rate more than doubled, the percentage barely moved",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top", panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 4. THE PUNCHLINE. everything changed and the margin did not.
# negative values, so zero is drawn inside the axis rather than used as a baseline.
p_margin <- ggplot(fire, aes(x = season, y = mov, fill = now)) +
  geom_col(width = 0.62) +
  geom_hline(yintercept = 0, colour = INK, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.2f", mov)), vjust = 1.5, colour = INK, size = 4.6) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = "#B4B2A9"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.22, 0.06))) +
  labs(title = "portland fire point differential per game, by season",
       subtitle = "2000 and 2026 are 0.14 apart",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 5. expansion debuts, verified in hhs. the 1997 teams are the founding eight and are
# deliberately excluded. portland 2026 is SIXTH, which corrects the earlier draft.
exp_debut <- tibble::tribble(
  ~team,               ~w, ~l,
  "Detroit 1998",       17, 13, "Golden State 2025", 23, 21, "Orlando 1999",   16, 16,
  "Minnesota 1999",     15, 17, "Miami 2000",        13, 19, "Portland 2026",  16, 24,
  "Portland 2000",      10, 22, "Indiana 2000",       9, 23, "Toronto 2026",   11, 29,
  "Seattle 2000",        7, 25, "Chicago 2006",       5, 29, "Atlanta 2008",    4, 30,
  "Washington 1998",     3, 27) |>
  mutate(pct = 100 * w / (w + l), mine = grepl("Portland", team))
p_expansion <- ggplot(exp_debut, aes(x = pct, y = reorder(team, pct))) +
  geom_segment(aes(x = 0, xend = pct, yend = team, colour = mine),
               linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
  geom_point(aes(colour = mine, size = mine)) +
  geom_text(aes(label = sprintf("%d-%d", w, l), colour = mine),
            hjust = -0.35, size = 4.2, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
  labs(title = "every wnba expansion debut by win percentage",
       subtitle = "founding 1997 teams excluded",
       caption = CREDIT, x = "win percentage", y = NULL) +
  base_theme()

# 6. the day 1 callback. the two newest teams take the most threes in the league.
p_league3 <- lolli(teams, p3rate, "%.1f",
  "share of shot attempts taken from three, by team",
  NULL,
  hi_when = c("Portland", "Golden State"))

# ---------------------------------------------------------------------------
# 7. THE NEW ANGLE, 9/13: same margin, opposite reasons. her hoop stats, regular season.
# offense and defense are points per 100 possessions from the box score. "vs league" is
# against the average of every team that season. ranks are out of 16 teams in 2000-2002
# and 15 in 2026. hardcoded because wehoop does not reach 2000.
then_now <- tibble::tribble(
  ~season, ~record, ~mov,   ~ortg,  ~ortg_rank, ~ortg_vs_lg, ~drtg,  ~drtg_rank, ~drtg_vs_lg, ~rate3_rank, ~n,
  "2000",  "10-22", -4.34,   90.13, 14,          -5.14,       96.02,  9,           0.72,        4,          16,
  "2001",  "11-21", -3.56,   88.96, 13,          -3.48,       93.62, 11,           1.22,        NA,         16,
  "2002",  "16-16", -2.03,   92.47, 12,          -2.22,       95.50,  8,           0.82,        NA,         16,
  "2026",  "16-24", -4.48,  104.12,  8,          -0.77,      109.24, 14,           4.39,        2,          15
)
# per 100 possessions the margins are NOT identical: 2000 -5.89, 2026 -5.12. the video only
# claims "about the same amount" per game.

# 7a. TABLE: 2000 vs 2026 side by side
t_then_now <- then_now |>
  filter(season %in% c("2000", "2026")) |>
  transmute(season, record, mov,
            offense = sprintf("%d of %d", ortg_rank, n),
            defense = sprintf("%d of %d", drtg_rank, n),
            threes  = sprintf("%d of %d", rate3_rank, n)) |>
  gt() |>
  cols_label(season = "", record = "record", mov = "margin per game",
             offense = "offense rank", defense = "defense rank",
             threes = "three point rate rank") |>
  fmt_number(columns = mov, decimals = 1, force_sign = TRUE) |>
  cols_align("center", columns = -season) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = season)) |>
  tab_style(style = list(cell_text(color = HI, weight = "bold")),
            locations = cells_body(columns = offense, rows = season == "2000")) |>
  tab_style(style = list(cell_text(color = HI, weight = "bold")),
            locations = cells_body(columns = defense, rows = season == "2026")) |>
  tab_header(title = "portland fire, 2000 vs 2026",
             subtitle = "offense and defense ranked by points per 100 possessions") |>
  tab_source_note("wnba regular seasons  \u00b7  data: her hoop stats  |  chart: @wnbadata") |>
  tab_options(table.font.names = "Helvetica", table.font.size = px(18),
              heading.align = "left", heading.title.font.size = px(24),
              heading.subtitle.font.size = px(15),
              column_labels.font.weight = "bold", table.border.top.color = "white")

# 7b. BARS: how much worse than league average each team was, on each end. zero anchored,
# length encodes "points per 100 possessions worse than the league". offense flipped so
# bigger is always worse.
worse <- then_now |>
  filter(season %in% c("2000", "2026")) |>
  transmute(season, offense = -ortg_vs_lg, defense = drtg_vs_lg) |>
  pivot_longer(-season, names_to = "side", values_to = "worse") |>
  mutate(season = paste(season, "fire"),
         side = factor(side, levels = c("offense", "defense")))

p_opposite <- ggplot(worse, aes(x = side, y = worse, fill = season)) +
  geom_col(position = position_dodge(width = 0.74), width = 0.66) +
  geom_text(aes(label = sprintf("%.1f", worse)), position = position_dodge(width = 0.74),
            vjust = -0.6, colour = INK, size = 4.4) +
  scale_fill_manual(values = c(`2000 fire` = ALT, `2026 fire` = HI), name = NULL) +
  scale_x_discrete(labels = c(offense = "offense", defense = "defense")) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
  labs(title = "how much worse than league average, per 100 possessions",
       subtitle = "the 2000 fire couldn't score. the 2026 fire can't guard",
       caption = "wnba regular seasons 2000 and 2026  \u00b7  data: her hoop stats  |  chart: @wnbadata",
       x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top", panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 7c. "they score twenty more a game, but so does everybody." fire vs league average ppg.
scoring_vs_lg <- tibble::tribble(
  ~season, ~who,             ~ppg,
  "2000",  "portland fire",  67.34,
  "2000",  "league average", 68.52,
  "2026",  "portland fire",  86.58,
  "2026",  "league average", 87.16
)

p_scoring_vs_lg <- ggplot(scoring_vs_lg, aes(x = season, y = ppg, fill = who)) +
  geom_col(position = position_dodge(width = 0.74), width = 0.66) +
  geom_text(aes(label = sprintf("%.1f", ppg)), position = position_dodge(width = 0.74),
            vjust = -0.6, colour = INK, size = 4.4) +
  scale_fill_manual(values = c(`portland fire` = HI, `league average` = ALT), name = NULL) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.14))) +
  labs(title = "points per game, portland fire vs the league",
       subtitle = "both fire teams scored within about a point of league average",
       caption = "wnba regular seasons 2000 and 2026  \u00b7  data: her hoop stats  |  chart: @wnbadata",
       x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top", panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# ALL THE CHARTS. run any of these on its own to render it.
#
# charts, in order
p_margin         # 1. margin per game, all four fire seasons. 2000 -4.34, 2026 -4.48
t_then_now       # 2. TABLE. 2000 vs 2026: record, margin, offense/defense/three rate ranks (viewer pane)
p_opposite       # 3. HOOK PAYOFF. how much worse than league on offense vs defense, 2000 vs 2026
p_3rate          # 4. three point rate, fire vs league average each season. 4th of 16 in 2000, 2nd of 15 now
p_scoring_vs_lg  # 5. "so does everybody": fire ppg vs league average, 2000 and 2026
#
# built but not used in the final video
p_ppg            # points per game by season, the old lead
p_vol            # three point rate vs three point percentage
p_expansion      # expansion debuts, portland 6th (detroit fixed to 17-13 on 9/13)
p_league3        # 2026 three point rate by team
#
# tibbles
# fire            all four fire seasons: record, margin, scoring, threes, efg
# then_now        offense/defense per 100 with ranks and vs league, all four seasons
# exp_debut       every expansion debut record
# teams           every team's 2026 shooting and record, computed live from wehoop
