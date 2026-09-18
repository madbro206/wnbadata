# charts for the indiana fever "weird 7/15" video
#
# everything here computes live from wehoop (espn) and was cross checked against
# her hoop stats on 9/5 using final numbers through 8/30.
# game duration is hardcoded from hhs, wehoop does not carry it.\n#
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

ALLSTAR   <- c("Team Spoon", "Team Coop")
CUP_FINAL <- as.Date("2026-06-30")
ME        <- "Indiana"

HI     <- "#002D62"   # indiana fever. swap this hex if you want a different color
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

player_box <- load_wnba_player_box(seasons = 2026) |>
  filter(season_type == 2, !team_location %in% ALLSTAR, game_date != CUP_FINAL, !did_not_play)

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

# 1. THE LEAD. best shooting team in the league
p_efg <- lolli(teams, efg, "%.1f",
  "effective field goal percentage, by team",
  NULL)

# 2. THE TWIST. and the least assisted offense in the league.
# the league average line is what makes the gap readable, indiana is alone below it.
LG_AST <- 100 * sum(team_box$assists) / sum(team_box$field_goals_made)
p_ast <- lolli(teams, astrate, "%.1f",
  "share of made field goals that were assisted, by team",
  sprintf("league average %.1f", LG_AST))

# 3. THE MONEY CHART. both facts at once. a scatter, so no zero baseline needed.
# indiana sits alone in the corner nobody else is in: makes the most, passes the least.
# the fitted line matters here. across the 88 wnba team seasons since 2020, assist rate
# and effective fg% correlate at r = 0.39, so good passing teams really do shoot better.
# some of that is mechanical, since an assisted shot is usually an easier shot. without
# the trend line indiana is just a dot in a corner. with it, indiana is an exception to
# something real, which is the actual point of the video.
p_scatter <- ggplot(teams, aes(x = astrate, y = efg)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              colour = "#C9C8C3", linewidth = 0.7, linetype = "22") +
  geom_point(aes(colour = team == ME, size = team == ME)) +
  ggrepel::geom_text_repel(aes(label = team, colour = team == ME),
                           size = 4, seed = 1, max.overlaps = 20, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3, `TRUE` = 5.4), guide = "none") +
  labs(title = "effective field goal percentage against assist rate, by team",
       subtitle = "dashed line is the league trend. teams that pass more usually shoot better",
       caption = CREDIT, x = "share of baskets assisted", y = NULL) +
  base_theme() +
  theme(panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 4. the quarters. NOT a descending staircase, which is why the chart is honest to show.
ind_q <- qtr |> filter(team == ME) |> select(q1:q4) |>
  pivot_longer(everything(), names_to = "q", values_to = "pts") |>
  mutate(q = recode(q, q1 = "1st", q2 = "2nd", q3 = "3rd", q4 = "4th"))
p_qtr <- ggplot(ind_q, aes(x = q, y = pts)) +
  geom_col(fill = HI, width = 0.62) +
  geom_text(aes(label = sprintf("%.1f", pts)), vjust = -0.6, colour = INK, size = 4.6) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.14))) +
  labs(title = "indiana average points by quarter",
       subtitle = "note that the third quarter is higher than the second",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 5. the runtime bit. hardcoded from hhs, wehoop carries no game duration.
dur <- tibble::tribble(
  ~team, ~mins,
  "Indiana", 132.2, "Washington", 129.8, "Atlanta", 128.9, "Connecticut", 127.7,
  "Chicago", 126.7, "New York", 125.8, "Portland", 125.8, "Phoenix", 125.7,
  "Golden State", 125.1, "Los Angeles", 125.0, "Toronto", 124.7, "Las Vegas", 123.2,
  "Seattle", 123.0, "Dallas", 122.2, "Minnesota", 121.7)
p_dur <- lolli(dur, mins, "%.1f",
  "average real time game length in minutes, by team",
  NULL)

# ---------------------------------------------------------------------------
# "so who is actually taking those" — the unassisted threes themselves.
#
# this replaces the old kelsey mitchell scoring share chart. that stat was true but it
# was orphaned once the video's spine moved to threes: "25.8% of team points" does not
# explain a three point assist gap. unassisted made threes does, and it names two
# players instead of one.
#
# CAVEAT ON WASHINGTON: wehoop drops the assister on 74 washington made field goals, so
# washington's unassisted counts here are OVERSTATED. they are nowhere near the top of
# either chart so it does not change the story, but do not quote a washington number
# off these. see the memory note on wehoop assist attribution.
unast3 <- pbp |>
  filter(scoring_play, score_value == 3) |>
  inner_join(team_box |> select(game_id, team_id, team_location) |> distinct(),
             by = c("game_id", "team_id")) |>
  mutate(unast = is.na(athlete_name_2))

u3_player <- unast3 |>
  group_by(player = athlete_name_1, team = team_location) |>
  summarise(made3 = n(), unassisted = sum(unast), .groups = "drop") |>
  slice_max(unassisted, n = 10) |>
  mutate(lab = paste0(player, "  (", team, ")"))

p_unast3 <- ggplot(u3_player, aes(x = unassisted, y = reorder(lab, unassisted))) +
  geom_segment(aes(x = 0, xend = unassisted, yend = lab, colour = team == ME),
               linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
  geom_point(aes(colour = team == ME, size = team == ME)) +
  geom_text(aes(label = unassisted, colour = team == ME), hjust = 0, nudge_x = 1.2,
            size = 4.2, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.14))) +
  labs(title = "made threes with nobody assisting, league leaders",
       subtitle = "the two highest totals in the wnba are both fever",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

u3_team <- unast3 |> group_by(team = team_location) |>
  summarise(unassisted = sum(unast), .groups = "drop")

p_unast3_team <- lolli(u3_team, unassisted, "%d",
  "made threes with nobody assisting, by team",
  "indiana has 64 percent more than second place")

# ---------------------------------------------------------------------------
# "they have the best effective field goal percentage in the league, the best three
# point percentage, and the best true shooting from their starting five"
#
# THREE SHOOTING TITLES IN ONE IMAGE. the video names three metrics in three seconds,
# so three separate charts would be a slideshow. small multiples, each panel ranked on
# its own, indiana at the top of all three.
#
# FLAG: the three point margin is thin. indiana 39.53 to minnesota 39.11 is 0.42, while
# the eFG gap is 1.91 and the true shooting gap is 1.78. "best in the league" is true for
# all three, but do not say "and it isn't close" about the three point one.
shoot_ind <- team_box |>
  group_by(team = team_location) |>
  summarise(`effective fg%` = 100 * (sum(field_goals_made) + 0.5 * sum(three_point_field_goals_made)) /
                              sum(field_goals_attempted),
            `three point %` = 100 * sum(three_point_field_goals_made) /
                              sum(three_point_field_goals_attempted),
            .groups = "drop") |>
  left_join(player_box |> filter(starter) |> group_by(team = team_location) |>
              summarise(`starters true shooting` = 100 * sum(points) /
                        (2 * (sum(field_goals_attempted) + 0.44 * sum(free_throws_attempted))),
                        .groups = "drop"),
            by = "team") |>
  tidyr::pivot_longer(-team, names_to = "stat", values_to = "v") |>
  mutate(stat = factor(stat, levels = c("effective fg%", "three point %", "starters true shooting")),
         row = paste0(team, "\u0001", as.integer(stat)))

# PER PANEL SORTING. reorder(team, v) computes a single global ordering across all
# facets, so scales = "free_y" alone leaves every panel sorted by the first metric and
# the bars come out visibly out of order. tagging each row with its panel makes the
# ordering local, and the axis labeller strips the tag back off.
# REBUILT 9/9 as dots. the bar version had three problems:
#   1. every team name printed three times, 45 labels for 15 teams
#   2. three separate y axes eating the width the data should be using
#   3. and the real one: these values all live between 46 and 62, so bars anchored at
#      zero were about 80% identical and indiana's lead was invisible
#
# dots fix all three. one shared team order means the names appear once, and a dot
# encodes POSITION rather than length, which is the one case where a non zero axis is
# honest. same exception as p_3pct in the valkyries file. now the gap between indiana
# and second is actually visible, which is the entire point of the chart.
#
# teams are ordered by effective fg% and that order is HELD across all three panels, so
# indiana sitting furthest right in every panel is the thing you read.
ORDER <- shoot_ind |> filter(stat == "effective fg%") |> arrange(v) |> pull(team)
shoot_ind <- shoot_ind |> mutate(team = factor(team, levels = ORDER))

p_shoot <- ggplot(shoot_ind, aes(x = v, y = team)) +
  geom_point(aes(colour = team == ME, size = team == ME)) +
  geom_text(data = filter(shoot_ind, team == ME),
            aes(label = sprintf("%.1f", v)), hjust = -0.35, colour = HI,
            size = 4, fontface = "bold") +
  facet_wrap(~stat, nrow = 1, scales = "free_x") +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 2.6, `TRUE` = 4.6), guide = "none") +
  # indiana is the rightmost point in every panel and its label sits outside the dot,
  # so the right expansion has to leave room or the number gets clipped by the panel edge
  scale_x_continuous(expand = expansion(mult = c(0.10, 0.34))) +
  labs(title = "indiana leads the league in most shooting measures",
       subtitle = "each panel on its own scale, teams in the same order throughout",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(colour = INK, size = 11.5),
        strip.text = element_text(colour = INK, size = 12, face = "bold"),
        panel.grid.major.y = element_line(colour = "#EDECE9", linewidth = 0.3),
        panel.grid.major.x = element_blank(),
        panel.spacing.x = unit(14, "pt"))

# ---------------------------------------------------------------------------
# "in thirty seasons, only twice has a team led the league in shooting and had the
# worst assist rate in the same year"
#
# HARDCODED FROM HHS, queried 9/8 across all 380 wnba team seasons since 1997 that have
# both figures. ranked effective fg% and assist rate within each season, then kept the
# team seasons that were 1st in eFG and last in assist rate. exactly two came back.
# wehoop cannot check this, its team box only reaches 2024.
#
# near miss worth knowing: indiana 2024 was also 1st in eFG with the 3rd LOWEST assist
# rate of 12 teams, so this is a fever pattern rather than a one year fluke.
only_twice <- tibble::tribble(
  ~team,             ~efg,   ~ar,    ~note,
  "indiana 2026",    56.66,  60.88,  "1st in shooting, 15th of 15 in assist rate",
  "houston 2000",    51.58,  55.44,  "1st in shooting, 16th of 16 in assist rate") |>
  mutate(mine = team == "indiana 2026")

p_only_twice <- ggplot(only_twice, aes(x = efg, y = reorder(team, efg))) +
  geom_col(aes(fill = mine), width = 0.5) +
  geom_text(aes(label = sprintf("%.1f%% eFG", efg)), hjust = 1.15,
            colour = "white", size = 5, fontface = "bold") +
  geom_text(aes(label = note), hjust = -0.06, colour = GRAY, size = 4.2) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = "#B4B2A9"), guide = "none") +
  scale_x_continuous(limits = c(0, 105), expand = expansion(mult = c(0, 0.02))) +
  labs(title = "led the league in shooting and had the worst assist rate",
       subtitle = "the only two team seasons to do both, out of 380 since 1997",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.x = element_blank())

# ---------------------------------------------------------------------------
# "yes, caitlin clark is second in the entire league in assists"
#
# this pair exists to kill the obvious comment before it gets written. the answer to
# "isn't clark a great passer?" is that she is, and that an assist needs a passer AND a
# finisher who needed the pass. indiana's two leading scorers mostly did not.
#
# note this is an incidental mention, not a clark themed video. the lead, the mechanism
# and the payoff are all team level.

# 1. she is doing the passing. everyone else is not.
fever_ast <- player_box |>
  filter(team_location == ME) |>
  group_by(team = athlete_display_name) |>
  summarise(ast = sum(assists), .groups = "drop") |>
  slice_max(ast, n = 7)

# clark 302 against mitchell 112 + boston 108 + harris 69 = 289. it is the next THREE
# combined, not four. the next four total 340, which is more than she has.
p_fever_ast <- lolli(fever_ast, ast, "%d",
  "total assists, indiana fever",
  "clark has more than the next three players combined",
  hi_when = "Caitlin Clark")

# 2. THE DEFUSE. clark's own baskets are almost never assisted, and the one player below
# her is olivia miles, who nobody accuses of being a ball stopper. that is the point:
# creators create for themselves too, so a low assisted share is a passer's signature
# rather than a knock.
#
# athlete_name_2 on a scoring row is the assister. it is populated 66.0% of the time
# league wide, which matches the 66.36% box score assist rate almost exactly, so the
# field is doing what it looks like it does.
MIN_COL <- "#236192"   # lynx blue, for olivia miles

tb_lu <- team_box |> select(game_id, team_id, team_location) |> distinct()

assisted <- pbp |>
  filter(scoring_play, score_value >= 2) |>
  inner_join(tb_lu, by = c("game_id", "team_id")) |>
  group_by(player = athlete_name_1, team = team_location) |>
  summarise(makes = n(), ast_pct = 100 * mean(!is.na(athlete_name_2)), .groups = "drop") |>
  filter(makes >= 200) |>
  slice_min(ast_pct, n = 10) |>
  mutate(who = case_when(player == "Caitlin Clark" ~ "clark",
                         player == "Olivia Miles" ~ "miles",
                         TRUE ~ "other"),
         lab = paste0(player, "  (", team, ")"))

p_assisted <- ggplot(assisted, aes(x = ast_pct, y = reorder(lab, -ast_pct))) +
  geom_segment(aes(x = 0, xend = ast_pct, yend = lab, colour = who),
               linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
  geom_point(aes(colour = who, size = who != "other")) +
  geom_text(aes(label = sprintf("%.1f", ast_pct), colour = who),
            hjust = 0, nudge_x = 1.4, size = 4.2, show.legend = FALSE) +
  scale_colour_manual(values = c(clark = HI, miles = MIN_COL, other = GRAY), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.18))) +
  labs(title = "share of a player's own baskets that came off an assist",
       subtitle = "lowest in the league, 200 or more made field goals. the two lowest are both known as passers",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# ---------------------------------------------------------------------------
# "it is almost entirely about threes"
#
# HARDCODED FROM BASKETBALL REFERENCE'S "% of FG Ast'd", pulled 9/8. deliberately not
# computed from wehoop play by play: wehoop drops the assister on 74 washington made
# field goals across 22 games, which pushes washington's assisted share about 5 points
# too low on twos and 9 too low on threes. every other team matched bbref to within
# 0.05, so bbref is the source of record for this one.
#
# THE FINDING: indiana is only mildly below average on two pointers, .551 against a .587
# league average, and golden state is actually lower at .531. on THREES they are at
# .736 against a .859 average, and the next lowest team is toronto at .827. that is a
# nine point gap to 14th place. the low team assist rate is a three point story.
ast_split <- tibble::tribble(
  ~team,           ~p2,   ~p3,
  "Atlanta",       57.2,  92.7,   "Connecticut",  59.2,  90.9,
  "Golden State",  53.1,  90.6,   "Washington",   60.7,  90.2,
  "Phoenix",       61.7,  88.9,   "Portland",     56.0,  88.2,
  "Dallas",        58.7,  87.4,   "Seattle",      56.3,  86.1,
  "Los Angeles",   62.9,  85.9,   "New York",     58.4,  85.0,
  "Minnesota",     56.3,  84.1,   "Chicago",      65.2,  83.7,
  "Las Vegas",     60.5,  83.7,   "Toronto",      58.4,  82.7,
  "Indiana",       55.1,  73.6)

LG3 <- 85.9; LG2 <- 58.7

p_ast3 <- ggplot(ast_split, aes(x = p3, y = reorder(team, p3))) +
  geom_vline(xintercept = LG3, colour = "#C9C8C3", linetype = "22") +
  geom_segment(aes(x = 0, xend = p3, yend = team, colour = team == ME),
               linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
  geom_point(aes(colour = team == ME, size = team == ME)) +
  geom_text(aes(label = sprintf("%.1f", p3), colour = team == ME),
            hjust = 0, nudge_x = 1.6, size = 4.2, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
  labs(title = "share of made threes that came off an assist",
       subtitle = sprintf("dashed line is the %.1f league average. indiana is nine points behind 14th place", LG3),
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# the same thing next to two pointers, so you can see the gap is a THREES gap. a
# dumbbell, both ends on one scale, no zero baseline needed.
p_ast_split <- ggplot(ast_split, aes(y = reorder(team, p3))) +
  geom_segment(aes(x = p2, xend = p3, yend = team, colour = team == ME),
               linewidth = 1.5, alpha = 0.5, show.legend = FALSE) +
  geom_point(aes(x = p2), colour = ALT, size = 3.6) +
  geom_point(aes(x = p3, colour = team == ME, size = team == ME)) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.6, `TRUE` = 5.6), guide = "none") +
  labs(title = "assisted share of twos against assisted share of threes",
       subtitle = "light dot is two pointers, dark dot is threes. everyone's threes come off a pass except indiana's",
       caption = CREDIT, x = "percent assisted", y = NULL) +
  base_theme()

# ---------------------------------------------------------------------------
# ALTERNATIVE to p_shoot, built 9/9. the dot version ranks all 15 teams in all three
# measures, which is 45 marks to support a claim about 3. this one shows only what the
# script actually says: indiana is first, and here is who is second.
#
# it also makes the caveat visible instead of buried in a note. the effective fg and
# true shooting gaps are wide, the three point gap is 0.42, and on this chart you can
# see that without being told.
runner <- function(d, lab) {
  d <- d |> arrange(desc(v))
  tibble::tibble(stat = lab,
                 lead_team = d$team[1], lead = d$v[1],
                 next_team = d$team[2], next_v = d$v[2])
}
tm_ts <- team_box |> group_by(team = team_location) |>
  summarise(v = 100 * sum(team_score) /
                (2 * (sum(field_goals_attempted) + 0.44 * sum(free_throws_attempted))),
            .groups = "drop")
gaps <- bind_rows(
  runner(teams |> transmute(team, v = efg), "effective fg%"),
  runner(teams |> transmute(team, v = p3),  "three point %"),
  runner(tm_ts, "true shooting")) |>
  mutate(gap = lead - next_v,
         stat = factor(stat, levels = c("true shooting", "three point %", "effective fg%")),
         lab = sprintf("indiana %.1f   ·   %s %.1f", lead, tolower(next_team), next_v))

p_shoot_gap <- ggplot(gaps, aes(x = gap, y = stat)) +
  geom_col(fill = HI, width = 0.5) +
  geom_text(aes(label = sprintf("+%.2f", gap)), hjust = -0.25,
            colour = INK, size = 5, fontface = "bold") +
  geom_text(aes(x = 0, label = lab), hjust = 0, nudge_y = 0.42,
            colour = GRAY, size = 4) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.28))) +
  labs(title = "how far ahead of second place indiana is",
       subtitle = "first in all three, but the three point lead is a rounding error",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(axis.text.y = element_text(colour = INK, size = 13),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())

# render one at a time in positron. for 9:16 size the plot pane tall and narrow.
# script order
p_shoot
p_shoot_gap
p_ast
p_ast3
p_ast_split
p_scatter
p_only_twice
p_fever_ast
p_assisted
p_unast3
p_unast3_team
p_dur

# CAPTION ONLY. the quarter beat was cut from the final video in the 9/8 clark rewrite,
# but the first quarter scoring line is still in the caption.
p_qtr
