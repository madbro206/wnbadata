# charts for the chicago sky "weird 9/15" video
#
# everything here computes live from wehoop (espn) and was cross checked against
# her hoop stats on 9/5 using final numbers through 8/30.
# ages are hardcoded from the hhs person table as of 9/5. wehoop carries no birthdate.\n#
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
ME        <- "Chicago"

HI     <- "#418FDE"   # chicago sky. swap this hex if you want a different color
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

player_box <- load_wnba_player_box(seasons = 2026) |>
  filter(season_type == 2, !team_location %in% ALLSTAR, game_date != CUP_FINAL, !did_not_play)
as_min <- function(x) { x <- as.character(x); ifelse(is.na(x), 0, suppressWarnings(as.numeric(x))) }

# 1. THE LEAD, rebuilt 9/10. the old version was a bar chart of three ages, which is
# three numbers and not much of a chart. this shows the actual finding: their ASSISTS
# are basically identical while everything else about them is not.
#
# ages hardcoded from the hhs person table as of 9/11, the publish date. wehoop box
# scores carry no birthdate so this cannot come from wehoop at all.
PG <- c("Natasha Cloud", "Skylar Diggins", "Courtney Vandersloot")
PG_AGE <- tibble::tribble(~player, ~age,
  "Natasha Cloud", 34.6, "Skylar Diggins", 36.1, "Courtney Vandersloot", 37.6)

tm_tot <- player_box |> group_by(game_id, team_location) |>
  summarise(tfga = sum(field_goals_attempted), tfta = sum(free_throws_attempted),
            ttov = sum(turnovers), tmin = sum(as_min(minutes)), .groups = "drop")

pgs <- player_box |>
  inner_join(tm_tot, by = c("game_id", "team_location")) |>
  filter(team_location == ME, athlete_display_name %in% PG) |>
  group_by(player = athlete_display_name) |>
  summarise(`games played` = n(),
            `usage rate` = 100 * sum(field_goals_attempted + 0.44 * free_throws_attempted + turnovers) *
                           (sum(tmin) / sum(as_min(minutes)) / 5) / sum(tfga + 0.44 * tfta + ttov),
            `assists per game` = mean(assists),
            .groups = "drop") |>
  left_join(PG_AGE, by = "player") |>
  mutate(lab = sprintf("%s\n%.1f years old", player, age))

# ORDER HELD ACROSS PANELS, oldest at the top, so the flat assists panel reads against
# the two that are not flat. free_x because the three measures share no units.
ORD <- PG_AGE |> arrange(age) |> pull(player)
pgs_long <- pgs |>
  tidyr::pivot_longer(c(`games played`, `usage rate`, `assists per game`),
                      names_to = "stat", values_to = "v") |>
  mutate(stat = factor(stat, levels = c("games played", "usage rate", "assists per game")),
         lab = factor(lab, levels = pgs$lab[match(ORD, pgs$player)]))

p_pgs <- ggplot(pgs_long, aes(x = v, y = lab)) +
  geom_segment(aes(x = 0, xend = v, yend = lab), colour = HI, linewidth = 1.5, alpha = 0.5) +
  geom_point(colour = HI, size = 4.4) +
  geom_text(aes(label = sprintf("%.1f", v)), hjust = -0.4, colour = INK, size = 4) +
  facet_wrap(~stat, nrow = 1, scales = "free_x") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.34))) +
  labs(title = "chicago's three veteran point guards",
       subtitle = "they play wildly different amounts and use the ball differently. the assists are identical",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(axis.text.y = element_text(size = 10, lineheight = 1.1),
        strip.text = element_text(colour = INK, size = 12, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.spacing.x = unit(16, "pt"))

# 1b. THE THING NOBODY EXPECTS, upgraded 9/11 from games to MINUTES.
#
# "they played in the same game three times" sounds like a scheduling quirk. thirteen
# minutes of shared floor time across a whole season is a finding.
#
# HARDCODED FROM HHS and SINGLE SOURCED. wehoop has no lineup data at all, so unlike
# every other number in this video there is no cross check. the source is
# her hoop stats lineup stint data, and note that minutes and seconds are
# both NULL in that table, sec_total is the populated column.
#
# COMPLETENESS CAVEAT: the attributed lineup minutes total 1,490 against roughly 1,600
# expected for 40 games, so about 7% of floor time is unattributed in the source. say
# "about thirteen minutes," not "exactly 13.3"
floor_time <- tibble::tribble(
  ~who,                            ~minutes,
  "cloud, neither other",             426.2,
  "cloud + vandersloot",              316.1,
  "cloud + diggins",                  274.7,
  "diggins alone",                    224.8,
  "vandersloot alone",                129.0,
  "none of the three",                 92.8,
  "all three together",                13.3,
  "diggins + vandersloot, no cloud",   13.2) |>
  mutate(all3 = who == "all three together",
         pct = 100 * minutes / sum(minutes))

p_combo <- ggplot(floor_time, aes(x = minutes, y = reorder(who, minutes))) +
  geom_segment(aes(x = 0, xend = minutes, yend = who, colour = all3),
               linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
  geom_point(aes(colour = all3, size = all3)) +
  geom_text(aes(label = sprintf("%.1f min  ·  %.1f%%", minutes, pct), colour = all3),
            hjust = 0, nudge_x = 9, size = 4, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.34))) +
  labs(title = "how chicago actually used its three veteran point guards",
       subtitle = "minutes of shared floor time across the season. all three were out there for about thirteen",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# the three stints themselves, for pulling clips. 306 + 298 + 196 seconds = 800 = 13.3 min
all_three_stints <- tibble::tribble(
  ~date,        ~game,            ~minutes, ~pts, ~lineup,
  "2026-06-26", "vs portland",       5.10,   11,  "cardoso, cloud, diggins, stevens, vandersloot",
  "2026-07-03", "at las vegas",      4.97,    9,  "cardoso, cloud, diggins, taylor, vandersloot",
  "2026-07-03", "at las vegas",      3.27,    8,  "cloud, diggins, stevens, vandersloot, e. williams")

all_three_stints

# 2. THE PAYOFF. the oldest point guard room in the league runs the best passing offense.
LG_AST <- 100 * sum(team_box$assists) / sum(team_box$field_goals_made)
p_ast <- lolli(teams, astrate, "%.1f",
  "share of made field goals that were assisted, by team",
  sprintf("league average %.1f", LG_AST))

# 3. the ten oldest players in the league. hardcoded from hhs, 10 or more games played.
oldest <- tibble::tribble(
  ~player, ~team, ~age,
  "Alysha Clark", "Dallas", 39.2, "DeWanna Bonner", "Phoenix", 39.1,
  "Sami Whitcomb", "Phoenix", 38.1, "Courtney Vandersloot", "Chicago", 37.6,
  "Emma Cannon", "Los Angeles", 37.3, "Tiffany Hayes", "Golden State", 37.0,
  "Nneka Ogwumike", "Los Angeles", 36.2, "Rebekah Gardner", "New York", 36.2,
  "Skylar Diggins", "Chicago", 36.1, "Brittney Griner", "Connecticut", 35.9)
p_oldest <- ggplot(oldest, aes(x = age, y = reorder(player, age))) +
  geom_segment(aes(x = 0, xend = age, yend = player, colour = team == ME),
               linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
  geom_point(aes(colour = team == ME, size = team == ME)) +
  geom_text(aes(label = sprintf("%.1f", age), colour = team == ME),
            hjust = -0.4, size = 4.2, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.14))) +
  labs(title = "ten oldest players in the league, 10 or more games",
       subtitle = "age as of september 11",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# 4. the honest caveat chart. chicago is only 6th oldest overall, so this is a point
# guard thing and not a roster thing. ages minutes weighted, from hhs.
team_age <- tibble::tribble(
  ~team, ~age,
  "Phoenix", 31.41, "Los Angeles", 30.43, "Las Vegas", 30.34, "Minnesota", 30.33,
  "New York", 30.18, "Chicago", 29.65, "Golden State", 29.40, "Toronto", 29.17,
  "Dallas", 27.87, "Indiana", 27.64, "Atlanta", 27.12, "Portland", 26.45,
  "Seattle", 25.50, "Connecticut", 25.28, "Washington", 24.31)
p_age <- lolli(team_age, age, "%.1f",
  "average age weighted by minutes played, by team",
  NULL)

# ---------------------------------------------------------------------------
# THE JOKE'S GROUNDING, added 9/10. "kamilla cardoso is a center and she's third on the
# team in assists" is what earns the "i think they signed every point guard in the wnba"
# line. it makes the gag an observation rather than just a bit.
chi_ast <- player_box |>
  filter(team_location == ME) |>
  group_by(team = athlete_display_name) |>
  summarise(ast = sum(assists), .groups = "drop") |>
  slice_max(ast, n = 7)

p_chi_ast <- lolli(chi_ast, ast, "%d",
  "total assists, chicago sky",
  "cardoso is a center and she is third on the roster",
  hi_when = "Kamilla Cardoso")

# ---------------------------------------------------------------------------
# THE TWIST, added 9/11. "they almost never play together, and the numbers say that's
# correct." chicago's net rating by which of the three veterans is on the floor.
#
# HARDCODED FROM HHS, single sourced. wehoop has no lineup data. and note that in
# in the 2026 lineup data only points, possessions and opponent points are populated, so net
# rating is computable but assist rate by lineup is NOT. ast, fgm and tov are all NULL.
#
# SAY IT DESCRIPTIVELY. lineup net ratings are not opponent adjusted and the other three
# players change too. this is "chicago is worse when two of them share the floor," not
# "playing them together causes losses."
vets <- tibble::tribble(
  ~who,                  ~poss, ~off,  ~def,
  "cloud alone",           841, 108.2, 107.4,
  "cloud + vandersloot",   649, 106.8, 110.4,
  "cloud + diggins",       552, 107.8, 117.2) |>
  mutate(net = off - def,
         who = factor(who, levels = c("cloud + diggins", "cloud + vandersloot", "cloud alone")))

vets_long <- vets |>
  tidyr::pivot_longer(c(off, def), names_to = "end", values_to = "rtg") |>
  mutate(end = factor(if_else(end == "off", "points scored", "points allowed"),
                      levels = c("points scored", "points allowed")))

# offense and defense side by side, because the whole finding is that one of them moves
# and the other does not. a dot plot, so both share a zoomed scale honestly.
p_vets <- ggplot(vets_long, aes(x = rtg, y = who, colour = end)) +
  geom_line(aes(group = who), colour = "#D6D5D0", linewidth = 1.2) +
  geom_point(size = 5) +
  geom_text(aes(label = sprintf("%.1f", rtg)), vjust = -1.3, size = 3.9, show.legend = FALSE) +
  scale_colour_manual(values = c(`points scored` = ALT, `points allowed` = HI), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0.10, 0.10))) +
  labs(title = "chicago per 100 possessions", subtitle="by which pgs are on the floor",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top",
        panel.grid.major.y = element_line(colour = "#EDECE9", linewidth = 0.3))

# ---------------------------------------------------------------------------
# EVERY combination, not just the cloud ones. p_vets is the clean three row version for
# the video. this is the full picture for reference.
#
# SAMPLE SIZE IS THE WHOLE STORY HERE. the two combinations without cloud are 28 and 26
# possessions, which is 13 minutes each and pure noise. they are drawn in grey and
# labelled so nobody reads them as findings. "diggins + vandersloot" showing an off and
# def rating of exactly 142.3 is the giveaway, that is what 26 possessions looks like.
#
# hardcoded from hhs, single sourced. wehoop has no lineup data.
all_combos <- tibble::tribble(
  ~who,                       ~poss, ~off,  ~def,
  "all three",                   28, 100.0,  92.0,
  "none of the three",          196, 100.0,  97.9,
  "cloud alone",                841, 108.2, 107.4,
  "diggins + vandersloot",       26, 142.3, 142.3,
  "cloud + vandersloot",        649, 106.8, 110.4,
  "diggins alone",              444, 107.0, 110.9,
  "vandersloot alone",          269, 103.3, 111.1,
  "cloud + diggins",            552, 107.8, 117.2) |>
  mutate(net = off - def,
         usable = poss >= 100,
         lab = sprintf("%s  (%d poss)", who, poss))

XMIN <- 88; XMAX <- 122

combos_long <- all_combos |>
  tidyr::pivot_longer(c(off, def), names_to = "end", values_to = "rtg") |>
  mutate(end = factor(if_else(end == "off", "points scored", "points allowed"),
                      levels = c("points scored", "points allowed")),
         lab = factor(lab, levels = all_combos$lab[order(all_combos$net)]))

p_all_combos <- ggplot(combos_long, aes(x = rtg, y = lab)) +
  geom_line(aes(group = lab), colour = "#DCDBD7", linewidth = 1.1) +
  geom_point(aes(colour = end, alpha = usable), size = 4.2) +
  scale_colour_manual(values = c(`points scored` = ALT, `points allowed` = HI), name = NULL) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.3), guide = "none") +
  # the 26 possession diggins + vandersloot row sits at 142.3 on both ends and dragged
  # the axis out to 145, squashing every meaningful row into a narrow band. clipped to a
  # readable range with that row annotated instead, so all eight combinations still
  # appear but the useful ones are legible.
  geom_text(data = filter(combos_long, rtg > XMAX, end == "points allowed"),
            aes(x = XMAX, label = sprintf("both ends at %.1f, off the chart", rtg)),
            hjust = 1, colour = GRAY, size = 3.6) +
  coord_cartesian(xlim = c(XMIN, XMAX)) +
  labs(title = "chicago per 100 possessions, every point guard combination",
       subtitle = "sorted by net rating, worst at the bottom. faded rows are under 100 possessions and mean nothing",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top",
        axis.text.y = element_text(size = 10),
        panel.grid.major.y = element_line(colour = "#EDECE9", linewidth = 0.3))

# ---------------------------------------------------------------------------
# SAMPLE SIZE AGAINST EFFECT. x is possessions, y is net rating, one dot per point guard
# combination.
#
# this is the honest way to show the lineup data, because the caveat becomes the chart
# instead of a sentence. the two combinations on the far left are 26 and 28 possessions
# and they sit at the extremes of the y axis. the three that carry the story are all far
# right, and the further right you go the more you should believe the dot.
#
# a scatter, so both axes encode position and neither needs a zero baseline. the y zero
# line is drawn because break even is the meaningful reference.
p_net_scatter <- ggplot(all_combos, aes(x = poss, y = net)) +
  annotate("rect", xmin = -Inf, xmax = 100, ymin = -Inf, ymax = Inf,
           fill = "#F0EFEC", alpha = 0.75) +
  geom_hline(yintercept = 0, colour = INK, linewidth = 0.4, linetype = "22") +
  geom_point(aes(colour = usable, size = usable)) +
  ggrepel::geom_text_repel(aes(label = who, colour = usable),
                           size = 4, seed = 1, max.overlaps = 20,
                           box.padding = 0.5, show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = HI, `FALSE` = GRAY), guide = "none") +
  scale_size_manual(values = c(`TRUE` = 5, `FALSE` = 3), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.06, 0.12))) +
  labs(title = "net rating against pg minutes shared",
       subtitle = "chicago, one dot per point guard combination",
       caption = CREDIT, x = "possessions together", y = "net points per 100") +
  base_theme() +
  theme(panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# the three veterans in one table, for reading off camera and for comments.
# ordered by total minutes, which is the column that explains the other ones: cloud has
# played roughly twice the floor time of diggins, so the per game numbers hide how
# differently they were actually used.
pg_table <- player_box |>
  filter(team_location == ME, athlete_display_name %in% PG) |>
  group_by(player = athlete_display_name) |>
  summarise(g       = n(),
            tot_min = sum(as_min(minutes)),
            mpg     = mean(as_min(minutes)),
            tot_pts = sum(points),
            ppg     = mean(points),
            tot_ast = sum(assists),
            apg     = mean(assists),
            .groups = "drop") |>
  left_join(PG_AGE, by = "player") |>
  # ast36 is the column the video is built on. the apg numbers are nearly
  # identical and the per 36 numbers are not, which is the whole beat
  transmute(player, age, g,
            tot_min = round(tot_min, 0), mpg = round(mpg, 1),
            tot_pts, ppg = round(ppg, 1),
            tot_ast, apg = round(apg, 2),
            ast36 = round(36 * tot_ast / tot_min, 2)) |>
  arrange(desc(tot_min))

pg_table

# ---------------------------------------------------------------------------
# every point guard combination in one table: floor time, possessions, and the raw
# points on both ends alongside the per 100 rates.
#
# the raw pts_for and pts_against columns are here on purpose. 37 points for and 37
# against is what produced diggins + vandersloot's identical 142.3 ratings, and seeing
# the raw totals makes it obvious why that row is noise rather than a finding.
#
# hardcoded from hhs, single sourced. wehoop has no lineup data. source is
# her hoop stats lineup stint data, and note that minutes and seconds are
# NULL in that table, sec_total is the populated one.
# OPP_POSS IS A SEPARATE COLUMN AND IT MATTERS. defensive rating is points allowed per
# OPPONENT possession, not per chicago possession, and the two differ by up to 16 here.
# a first draft of this table divided both ends by poss and every net rating came out
# wrong, cloud alone read -1.2 instead of +0.9.
combo_table <- tibble::tribble(
  ~who,                      ~minutes, ~poss, ~opp_poss, ~pts_for, ~pts_against,
  "cloud alone",                426.2,   841,       857,      910,          920,
  "cloud + vandersloot",        316.1,   649,       633,      693,          699,
  "cloud + diggins",            274.7,   552,       558,      595,          654,
  "diggins alone",              224.8,   444,       441,      475,          489,
  "vandersloot alone",          129.0,   269,       270,      278,          300,
  "none of the three",           92.8,   196,       195,      196,          191,
  "all three",                   13.3,    28,        25,       28,           23,
  "diggins + vandersloot",       13.2,    26,        26,       37,           37) |>
  mutate(off_rtg = round(100 * pts_for / poss, 1),
         def_rtg = round(100 * pts_against / opp_poss, 1),
         net     = round(off_rtg - def_rtg, 1),
         usable  = poss >= 100)

combo_table

# render one at a time in positron. for 9:16 size the plot pane tall and narrow.
p_ast
p_pgs
p_combo
p_vets
p_all_combos
p_net_scatter
p_oldest
p_chi_ast
p_age
