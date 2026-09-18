# charts for the seattle storm "weird 6/15" video
#
# charts, in the order they appear in the video
#
#   p_close_wl  "this year the storm have been in 13 close games, and they won zero"
#   p_margins   the 47 points detail. thirteen nights, none of them by more than five
#   p_record    "no team in wnba history has ever lost this many without winning one"
#   p_tiers     "teams under .300 tend to win about a third of their close games"
#   p_expected  "at that rate seattle should have won about four of the thirteen"
#   p_standings "so they should be something like 12 and 28, which is ahead of both
#                toronto and connecticut"
#   p_mov_wins  "they are not even the worst team in the league by point differential"
#   p_corr      "i checked what actually predicts winning close games. none of it does"
#   p_clutch_ft  "the storm shot eighty percent in those, the mystics shot seventy three"
#   p_clutch_tov "the mystics had the most clutch turnovers in the wnba"
#
# VERIFIED 9/7. the 2026 numbers compute live from wehoop and match her hoop stats
# exactly: 13 close games, 0 wins, 47 total points, average margin 3.62, four losses by
# exactly 2 and five by exactly 5, portland beating them three times.
#
# THE HISTORICAL ROWS ARE HARDCODED FROM HHS and cannot be checked against wehoop,
# whose wnba team box only reaches back to 2024. both were queried on 9/7:
#   - across every team season since 1997, exactly two teams have gone winless in 5 or
#     more close games: seattle 2026 at 0-13 and the 1999 cleveland rockers at 0-7
#   - close game win rate by team quality, 1997 to 2026:
#     under .300 win 32.4% (52 team seasons, 408 close games), .300-.450 win 45.0%
#     (86, 849), .450-.550 win 49.3% (98, 931), over .550 win 59.7% (144, 1220)
#
# DEFINITION: "close" means a final margin of 5 points or fewer. this is a chosen cutoff,
# not a league stat, and the video says so out loud. a clutch time definition would
# give different numbers.
#
# DELIBERATELY NOT HERE: any coin flip framing. close games are not 50/50 and bad teams
# lose more of them, so 1/2^13 would overstate this by about 50x. the honest number is
# 0.676^13 = 0.62%, about 1 in 162, and p_expected is built on that instead.
#
# two mandatory wehoop filters or the season comes out wrong:
#   - the commissioner's cup final sits inside season_type == 2 but does not count
#   - the all star rosters appear as their own "teams"

library(wehoop)
library(dplyr)
library(tidyr)
library(ggplot2)

ALLSTAR   <- c("Team Spoon", "Team Coop")
CUP_FINAL <- as.Date("2026-06-30")
ME        <- "Seattle"
CLOSE     <- 5      # final margin of this or fewer

HI     <- "#2C5234"   # storm green. swap this hex if you want a different colour
GRAY   <- "#8A8A86"
ALT    <- "#B4B2A9"
INK    <- "#15171D"
CREDIT <- "2026 season through the fiba break  ·  data: her hoop stats and wehoop  |  chart: @wnbadata"

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

team_box <- load_wnba_team_box(seasons = 2026) |>
  filter(season_type == 2, !team_location %in% ALLSTAR, game_date != CUP_FINAL) |>
  mutate(margin = team_score - opponent_team_score)

sea <- team_box |> filter(team_location == ME)
sea_close <- sea |> filter(abs(margin) <= CLOSE) |> arrange(game_date)

# ---------------------------------------------------------------------------
# 1. "this year the storm have been in 13 close games, and they won zero of them"
# stacked so the length of each bar is the number of close games and the split is the
# record. seattle is the only bar with no dark segment at all.
close_wl <- team_box |>
  mutate(cl = abs(margin) <= CLOSE) |>
  group_by(team = team_location) |>
  summarise(won = sum(cl & margin > 0), lost = sum(cl & margin < 0), .groups = "drop") |>
  mutate(total = won + lost) |>
  pivot_longer(c(won, lost), names_to = "res", values_to = "n") |>
  mutate(res = factor(res, levels = c("lost", "won")))

p_close_wl <- ggplot(close_wl, aes(x = n, y = reorder(team, total), fill = res)) +
  geom_col(width = 0.68) +
  scale_fill_manual(values = c(won = HI, lost = "#D6D5D0"), name = NULL,
                    labels = c("lost", "won")) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
  labs(title = sprintf("record in games decided by %d points or fewer", CLOSE),
       subtitle = "seattle is the only team in the league without one",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top",
        plot.subtitle = element_text(colour = GRAY, size = 13, lineheight = 1.35,
                                     margin = margin(b = 12)))

# ---------------------------------------------------------------------------
# 2. the 47 points. every close loss, in order, with the running total.
# anchored at zero because the bar length IS the margin.
margins <- sea_close |>
  mutate(n = row_number(),
         lab = sprintf("%s\n%s", format(game_date, "%-m/%-d"), opponent_team_location),
         gap = abs(margin))

p_margins <- ggplot(margins, aes(x = factor(n), y = gap)) +
  geom_col(fill = HI, width = 0.66) +
  geom_text(aes(label = gap), vjust = -0.5, colour = INK, size = 4.6) +
  scale_x_discrete(labels = margins$lab) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.18))) +
  labs(title = "every seattle loss by five points or fewer",
       subtitle = sprintf("thirteen games, %d points between them, an average of %.1f",
                          sum(margins$gap), mean(margins$gap)),
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(axis.text.x = element_text(size = 9, lineheight = 1.1),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# 3. "no team in wnba history has ever lost this many without winning one"
# hardcoded from hhs. these are the ONLY two team seasons since 1997 with five or more
# close games and zero close wins.
record <- tibble::tibble(
  team = c("seattle 2026", "cleveland 1999"),
  losses = c(13, 7),
  note = c("8-32 that season", "5-26 that season")) |>
  mutate(mine = team == "seattle 2026")

p_record <- ggplot(record, aes(x = losses, y = reorder(team, losses))) +
  geom_col(aes(fill = mine), width = 0.5) +
  geom_text(aes(label = sprintf("0-%d", losses)), hjust = 1.35,
            colour = "white", size = 6, fontface = "bold") +
  geom_text(aes(label = note), hjust = -0.15, colour = GRAY, size = 4.2) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = ALT), guide = "none") +
  scale_x_continuous(limits = c(0, 18), expand = expansion(mult = c(0, 0.02))) +
  labs(title = "the only two teams to go winless in five or more close games",
       subtitle = "every wnba team season since 1997",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_blank())

# ---------------------------------------------------------------------------
# 4. "teams under .300 tend to win about a third of their close games"
# this is the chart that keeps you honest. being bad explains SOME of it, not all.
# seattle is NOT on this chart. two earlier attempts failed: a text label near the axis
# got covered by the first bar, and a zero height fifth bar is not a visible highlight
# at all. this chart's job is the league baseline, and p_expected does the comparison.
# one chart, one job.
tiers <- tibble::tibble(
  tier = factor(c("under .300", ".300 to .450", ".450 to .550", "over .550"),
                levels = c("under .300", ".300 to .450", ".450 to .550", "over .550")),
  rate = c(32.4, 45.0, 49.3, 59.7),
  seasons = c(52, 86, 98, 144))

p_tiers <- ggplot(tiers, aes(x = tier, y = rate)) +
  geom_col(aes(fill = tier == "under .300"), width = 0.62) +
  geom_text(aes(label = sprintf("%.1f%%", rate)), vjust = -0.55, colour = INK, size = 4.8) +
  geom_text(aes(label = sprintf("%d team seasons", seasons)), y = 2.4,
            colour = "white", size = 3.5) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = ALT), guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
  labs(title = "how often teams win their close games, by how good they are",
       subtitle = "every wnba team season since 1997. seattle is a .200 team, so they belong in the green group",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# 5. "at that rate seattle should have won about four of the thirteen"
P_BAD <- 0.324
EXPECTED <- nrow(sea_close) * P_BAD
ODDS <- (1 - P_BAD) ^ nrow(sea_close)

expected <- tibble::tibble(
  what = factor(c("expected", "actual"), levels = c("expected", "actual")),
  wins = c(EXPECTED, 0))

# the seattle bar is zero, so there is no bar to colour. the LABEL carries the
# highlight instead: the expected bar sits in muted grey and the 0.0 is printed large
# and in storm green. the absence is the whole point, so it is what gets emphasised.
# subtitle is two lines because the odds sentence does not fit on one at 9:16.
p_expected <- ggplot(expected, aes(x = what, y = wins)) +
  geom_col(fill = ALT, width = 0.5) +
  geom_text(aes(label = sprintf("%.1f", wins), colour = wins == 0,
                fontface = if_else(wins == 0, "bold", "plain")),
            vjust = -0.45, size = 7, show.legend = FALSE) +
  geom_hline(yintercept = 0, colour = INK, linewidth = 0.4) +
  scale_colour_manual(values = c(`TRUE` = HI, `FALSE` = INK), guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.26))) +
  labs(title = "seattle close wins, expected against actual",
       subtitle = sprintf("13 close games at 32.4%%, the rate for teams under .300 since 1997",
                          1 / ODDS),
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(plot.subtitle = element_text(colour = GRAY, size = 13, lineheight = 1.35,
                                     margin = margin(b = 14)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# 6. "they are not even the worst team in the league by point differential"
# a scatter, so both axes encode position and no zero baseline is needed. seattle sits
# below the trend: two teams are worse per game and both have more wins.
record26 <- team_box |>
  group_by(team = team_location) |>
  summarise(wins = sum(margin > 0), mov = mean(margin), .groups = "drop")

p_mov_wins <- ggplot(record26, aes(x = mov, y = wins)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              colour = "#C9C8C3", linewidth = 0.6, linetype = "22") +
  geom_point(aes(colour = team == ME, size = team == ME)) +
  ggrepel::geom_text_repel(aes(label = team, colour = team == ME),
                           size = 4, seed = 1, max.overlaps = 20, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3, `TRUE` = 5.4), guide = "none") +
  labs(title = "wins against point differential, by team",
       subtitle = "connecticut and toronto are both worse per game and both have more wins",
       caption = CREDIT, x = "point differential per game", y = "wins") +
  base_theme() +
  theme(panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# "i went looking for what seattle is missing, and there is nothing there"
#
# BOTH CHARTS BELOW ARE HARDCODED FROM HHS, queried 9/7. wehoop cannot do these, its
# team box only reaches 2024 and this needs every team season since 1997.
#
# METHOD: took all 335 wnba team seasons since 1997 with at least 6 close games and
# correlated close game win RATE against each candidate clutch skill. the query is in

# 1. nothing predicts winning close games except being good in general, and even that
# is r = 0.39, which is about 15% of the variance.
corr <- tibble::tribble(
  ~factor,                  ~r,
  "point differential",    0.392,
  "effective fg%",         0.224,
  "three point %",         0.188,
  "assists",               0.153,
  "free throw %",          0.092,
  "free throw attempts",   0.041,
  "turnovers",            -0.113,
  "fouls",                -0.146) |>
  mutate(ft = factor == "free throw %")

p_corr <- ggplot(corr, aes(x = r, y = reorder(factor, r))) +
  geom_segment(aes(x = 0, xend = r, yend = factor, colour = ft),
               linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
  geom_point(aes(colour = ft, size = ft)) +
  geom_text(aes(label = sprintf("%.2f", r), colour = ft,
                hjust = if_else(r >= 0, -0.35, 1.35)), size = 4.2, show.legend = FALSE) +
  geom_vline(xintercept = 0, colour = INK, linewidth = 0.4) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
  scale_x_continuous(limits = c(-0.3, 0.55), expand = expansion(mult = c(0.05, 0.05))) +
  labs(title = "what stats predict winning close games",
       subtitle = "correlation with 'close' game win rate, wnba since 1997",
       caption = CREDIT, x = "correlation", y = NULL) +
  base_theme()

# 2. THE BEAT. seattle against washington on every clutch skill, as percentiles so the
# four measures share one scale. seattle is BETTER at free throws and turnovers, the two
# things everyone says decide close games, and they went 0-13 while washington went 12-5.
# CLUTCH here means the last 5 minutes of the 4th quarter or overtime with the score
# within 5. computed live from play by play, which is the whole point: an earlier draft
# used SEASON free throw percentage, and shooting 79.6% across a season says nothing
# about what you did in the last two minutes. this measures the actual moment.
#
# sample sizes are small and uneven, so they are printed on the chart. seattle took 46
# clutch free throws and washington took 126, because washington played 17 close games
# and seattle 13. the honest claim is "seattle is not worse," not "seattle is better."
WAS_COL <- "#E03A3E"   # mystics red, for the day 3 callback

tbx_lu <- load_wnba_team_box(seasons = 2026) |>
  filter(season_type == 2, !team_location %in% ALLSTAR, game_date != CUP_FINAL) |>
  select(game_id, team_id, team_location) |> distinct()

pbp <- load_wnba_pbp(seasons = 2026) |>
  filter(season_type == 2) |>
  mutate(secs_left  = clock_minutes * 60 + clock_seconds,
         margin_now = abs(home_score - away_score),
         clutch     = period_number >= 4 & secs_left <= 300 & margin_now <= 5)

clutch_ft <- pbp |>
  filter(grepl("^Free Throw", type_text), !grepl("Technical", type_text), clutch) |>
  inner_join(tbx_lu, by = c("game_id", "team_id")) |>
  group_by(team = team_location) |>
  summarise(fta = n(), pct = 100 * mean(scoring_play), .groups = "drop") |>
  mutate(who = case_when(team == ME ~ "seattle, 0-13",
                         team == "Washington" ~ "washington, 12-5",
                         TRUE ~ "everyone else"))

LG_CL_FT <- 100 * mean(filter(pbp, grepl("^Free Throw", type_text),
                              !grepl("Technical", type_text), clutch)$scoring_play)

p_clutch_ft <- ggplot(clutch_ft, aes(x = pct, y = reorder(team, pct))) +
  geom_segment(aes(x = 0, xend = pct, yend = team, colour = who),
               linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
  geom_point(aes(colour = who, size = who != "everyone else")) +
  geom_text(aes(label = sprintf("%.1f   (%d att)", pct, fta), colour = who),
            hjust = 0, nudge_x = 2.2, size = 4, show.legend = FALSE) +
  scale_colour_manual(values = c(`seattle, 0-13` = HI, `washington, 12-5` = WAS_COL,
                                 `everyone else` = GRAY), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.26))) +
  labs(title = "free throw percentage in the clutch",
       subtitle = sprintf("last 5 minutes of the 4th or overtime, score within 5. league average %.1f%%", LG_CL_FT),
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

clutch_tov <- pbp |>
  filter(grepl("Turnover", type_text), clutch) |>
  inner_join(tbx_lu, by = c("game_id", "team_id")) |>
  group_by(team = team_location) |> summarise(tov = n(), .groups = "drop") |>
  mutate(who = case_when(team == ME ~ "seattle, 0-13",
                         team == "Washington" ~ "washington, 12-5",
                         TRUE ~ "everyone else"))

p_clutch_tov <- ggplot(clutch_tov, aes(x = tov, y = reorder(team, -tov))) +
  geom_segment(aes(x = 0, xend = tov, yend = team, colour = who),
               linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
  geom_point(aes(colour = who, size = who != "everyone else")) +
  geom_text(aes(label = tov, colour = who), hjust = 0, nudge_x = 0.9,
            size = 4.2, show.legend = FALSE) +
  scale_colour_manual(values = c(`seattle, 0-13` = HI, `washington, 12-5` = WAS_COL,
                                 `everyone else` = GRAY), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.14))) +
  labs(title = "turnovers in the clutch",
       subtitle = "last 5 minutes of the 4th or overtime, score within 5. fewest at the top",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# ---------------------------------------------------------------------------
# "so they should be something like 12 and 28, which is ahead of both toronto and
# connecticut, so even by the standard of a bad team they should not be in last place"
#
# the solid bar is wins they actually have. the hollow extension is the 4.2 close wins a
# .200 team would normally have picked up out of 13 chances. it is drawn hollow on
# purpose: those wins did not happen, and a filled bar would read as if they did.
BOTTOM <- 6

standings <- team_box |>
  group_by(team = team_location) |>
  summarise(w = sum(margin > 0), l = sum(margin < 0), .groups = "drop") |>
  slice_min(w, n = BOTTOM) |>
  mutate(mine = team == ME,
         proj = if_else(mine, w + EXPECTED, NA_real_))

p_standings <- ggplot(standings, aes(y = reorder(team, w))) +
  geom_col(aes(x = w, fill = mine), width = 0.6) +
  geom_segment(data = filter(standings, mine),
               aes(x = w, xend = proj, yend = team),
               colour = HI, linewidth = 0.7, linetype = "22") +
  geom_point(data = filter(standings, mine), aes(x = proj), colour = HI, size = 4.5) +
  geom_text(aes(x = w, label = w), hjust = 1.6, colour = "white", size = 4.4) +
  geom_text(data = filter(standings, mine),
            aes(x = proj, label = sprintf("%.1f  where they 'should' be", proj)),
            hjust = -0.12, colour = HI, size = 4.2, fontface = "bold") +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = ALT), guide = "none") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.42))) +
  labs(title = "the bottom of the wnba, and where seattle should be",
       subtitle = sprintf("",
                          EXPECTED),
       caption = CREDIT, x = "wins", y = NULL) +
  base_theme()

# render one at a time in positron, in script order.
# for 9:16 size the plot pane tall and narrow.
p_close_wl
p_margins
p_record
p_tiers
p_expected
p_standings
p_mov_wins
p_corr
p_clutch_ft
p_clutch_tov
