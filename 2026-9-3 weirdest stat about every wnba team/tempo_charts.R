# charts for the toronto tempo "weird 5/15" video
#
# charts, in the order they appear in the video
#
#   p_top10       "both sykes and mabrey were at the top of the league with about a
#                  thirty percent usage rate"
#   p_duo_share   the honest version of "combined sixty percent." see the note below
#   p_two_people  the same idea inside the roster, sykes was ending about thirty percent
#   p_spread      "so i went looking for who picked it up. nobody did"
#   p_split       "toronto with sykes was eight and nine. without her, three and twenty"
#   p_sykes       "she came back playing eleven and a half minutes a night, and posted
#                  the highest usage rate of her whole season"        (the kicker, last)
#
#   CAPTION ONLY, not spoken. simplify pass on 9/6 cut these from the script:
#   p_ladder      the three rung version. it is the rigorous answer to "mabrey also
#                 missed time," which is why it stays in the caption even though the
#                 script uses the simpler two rung split
#   p_mabrey      mabrey's usage vs her scoring
#   p_ends        the offense/defense split
#   p_fives       starting fives
#
# the ladder and the scoring splits compute live from wehoop (espn) and match her hoop
# stats EXACTLY: 7-9 at -2.69, 3-13 at -9.44, 0-7 at -15.86, and 88.8/91.1 against
# 84.3/95.7. verified 9/6.
#
# USAGE RATES YOU SAY OUT LOUD ARE HARDCODED from her hoop stats, because possession
# and minutes formulas differ between sources. wehoop gives sykes 28.4 / 28.6 / 33.0 by
# month against hhs's 28.0 / 28.1 / 31.3. identical shape, slightly different levels.
# p_two_people is the exception and computes live, because it is about the SHAPE of the
# roster rather than any single number.
#
# ONE REAL DISAGREEMENT: kia nurse's usage jump is +4.3 in hhs but only +2.5 in wehoop.
# sabally (+6.4 vs +6.7) and rice (+3.9 vs +4.1) agree closely, nurse does not. the
# script gives no number for nurse, and p_spread prints her range instead of a figure.
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
ME        <- "Toronto"

# tempo colours
HI     <- "#612C51"
GRAY   <- "#8A8A86"
ALT    <- "#B8CCEA"
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

player_box <- load_wnba_player_box(seasons = 2026) |>
  filter(season_type == 2, !team_location %in% ALLSTAR, game_date != CUP_FINAL, !did_not_play)

as_min <- function(x) { x <- as.character(x); ifelse(is.na(x), 0, suppressWarnings(as.numeric(x))) }

SYKES_GAMES  <- unique(player_box$game_id[player_box$athlete_display_name == "Brittney Sykes"])
MABREY_GAMES <- unique(player_box$game_id[player_box$athlete_display_name == "Marina Mabrey"])

# team totals per game, used by every usage calculation below. defined up here because
# the first chart that needs it is now the league leaderboard.
tm_tot <- player_box |> group_by(game_id, team_location) |>
  summarise(tfga = sum(field_goals_attempted), tfta = sum(free_throws_attempted),
            ttov = sum(turnovers), tmin = sum(as_min(minutes)), .groups = "drop")


tor <- team_box |> filter(team_location == ME) |>
  mutate(s = game_id %in% SYKES_GAMES, m = game_id %in% MABREY_GAMES)

# ---------------------------------------------------------------------------
# 1. THE HOOK. "she came back playing eleven and a half minutes a night, and her usage
# rate was thirty one point three percent, the highest of her whole season"
#
# july is kept as an empty slot on purpose. the gap is the story, and dropping it would
# hide the thing the whole video is about.
sykes_m <- tibble::tribble(
  ~m,        ~usg,  ~mpg,
  "may",     28.0,  31.9,
  "june",    28.1,  31.7,
  "july",      NA,    NA,
  "august",  31.3,  11.5) |>
  mutate(m = factor(m, levels = c("may", "june", "july", "august")))

# the minutes were printed inside the bars in white, but the text is wider than a 0.6
# bar, so the overhang landed on the white background and vanished. they live on the
# axis now, as a second line under each month, where the bar width cannot clip them.
sykes_m <- sykes_m |>
  mutate(xlab = if_else(is.na(mpg), paste0(m, "\nmissed the month"),
                        sprintf("%s\n%.1f min a night", m, mpg)))

p_sykes <- ggplot(sykes_m, aes(x = m, y = usg)) +
  geom_col(aes(fill = m == "august"), width = 0.6) +
  geom_text(aes(label = if_else(is.na(usg), "", sprintf("%.1f%%", usg))),
            vjust = -0.6, colour = INK, size = 4.8) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = ALT), guide = "none") +
  scale_x_discrete(labels = setNames(sykes_m$xlab, as.character(sykes_m$m))) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.18))) +
  labs(title = "brittney sykes usage rate by month",
       subtitle = "a third of the minutes in august, and a bigger share of the offense than ever",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3),
        axis.text.x = element_text(colour = INK, size = 12.5, lineheight = 1.3))

# WHY THERE IS NO "COMBINED 60% USAGE RATE" CHART, decided 9/6.
# usage rate is measured over a player's OWN court time, not the whole game, so two
# usage rates cannot be added. mabrey is 30.5% and sykes is 28.6%, but each played about
# 29.5 minutes, which is roughly 74% of a 40 minute game. across everything toronto did
# in the 16 games they both played, the two of them account for 42.5%, not 59%. that
# 42.5 is still a big number, but it is third behind indiana's 45.3 and las vegas's
# 43.8, so it does not support "carrying" as a superlative on its own.
# the 60% figure would only be right if both had been on the floor 100% of the time, and
# even in their shared minutes two high usage players usually take a bit off each other.
# box scores cannot measure shared minutes, so it is not knowable from this data at all.
#
# WHAT IS TRUE AND BETTER: toronto and indiana are the ONLY two teams in the wnba with
# two players in the league's top ten in usage rate. that is p_top10, and for an
# expansion team, next to caitlin clark and kelsey mitchell, it is a much stronger fact.

# ---------------------------------------------------------------------------
# "both sykes and mabrey were at the top of the league with about a 30% usage rate"
usage_lg <- player_box |>
  inner_join(tm_tot, by = c("game_id", "team_location")) |>
  group_by(player = athlete_display_name, team = team_location) |>
  summarise(g = n(),
            usg = 100 * sum(field_goals_attempted + 0.44 * free_throws_attempted + turnovers) *
                  (sum(tmin) / sum(as_min(minutes)) / 5) / sum(tfga + 0.44 * tfta + ttov),
            .groups = "drop") |>
  filter(g >= 15) |> slice_max(usg, n = 10) |>
  mutate(lab = paste0(player, "  (", team, ")"), tor = team == ME)

p_top10 <- ggplot(usage_lg, aes(x = usg, y = reorder(lab, usg))) +
  geom_segment(aes(x = 0, xend = usg, yend = lab, colour = tor),
               linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
  geom_point(aes(colour = tor, size = tor)) +
  geom_text(aes(label = sprintf("%.1f", usg), colour = tor),
            hjust = 0, nudge_x = 0.55, size = 4.2, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
  labs(title = "wnba usage rate leaders, 15 or more games",
       subtitle = "toronto and indiana are the only teams with two players in the top ten",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# ---------------------------------------------------------------------------
# the honest "combined" chart. share of everything toronto did in the 16 games sykes
# and mabrey both played. this is a share of TEAM plays, not a usage rate, which is
# exactly why the two pieces can be stacked.
BOTH_GAMES <- intersect(SYKES_GAMES, MABREY_GAMES)

duo <- player_box |>
  filter(team_location == ME, game_id %in% BOTH_GAMES) |>
  mutate(plays = field_goals_attempted + 0.44 * free_throws_attempted + turnovers,
         who = case_when(athlete_display_name == "Brittney Sykes" ~ "brittney sykes",
                         athlete_display_name == "Marina Mabrey" ~ "marina mabrey",
                         TRUE ~ "everyone else")) |>
  group_by(who) |> summarise(plays = sum(plays), .groups = "drop") |>
  mutate(pct = 100 * plays / sum(plays),
         who = factor(who, levels = c("everyone else", "marina mabrey", "brittney sykes")))

# readability notes, 9/6:
#   - the subtitle and the credit were both running off the right edge. the subtitle is
#     shortened, and the credit is wrapped onto two lines FOR THIS CHART ONLY, because a
#     single bar is much wider than it is tall and there is no room for one long line
#   - names and percentages sit on separate lines inside each block, and the two thin
#     blocks get a smaller name so nothing overflows its own colour
#   - the bar is 0.9 wide instead of 0.42 so it fills the panel rather than floating in it
CREDIT_WRAP <- "2026 season through the fiba break  \u00b7  data: her hoop stats and wehoop\nchart: @wnbadata"

duo <- duo |> mutate(name_size = if_else(pct < 30, 4.3, 5.2))

p_duo_share <- ggplot(duo, aes(x = pct, y = "", fill = who)) +
  geom_col(width = 0.9) +
  geom_text(aes(label = as.character(who), size = name_size),
            position = position_stack(vjust = 0.5), colour = "white",
            fontface = "bold", vjust = 2.1, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_stack(vjust = 0.5), colour = "white",
            size = 8, fontface = "bold", vjust = -0.35) +
  scale_size_identity() +
  scale_fill_manual(values = c(`brittney sykes` = HI, `marina mabrey` = ALT,
                               `everyone else` = "#9C9A94"), guide = "none") +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0))) +
  labs(title = "who ended toronto's possessions",
       subtitle = sprintf("the %d games sykes and mabrey both played", length(BOTH_GAMES)),
       caption = CREDIT_WRAP, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text = element_blank(),
        plot.subtitle = element_text(colour = GRAY, size = 14, margin = margin(b = 12)),
        plot.caption = element_text(colour = GRAY, size = 10.5, hjust = 0,
                                    lineheight = 1.3, margin = margin(t = 12)))

# ---------------------------------------------------------------------------
# 2. THE SETUP AND THE CLOSER. "sykes and mabrey were each ending about thirty percent
# of toronto's possessions" and "two people were doing the work of five"
#
# computed live, because this one is about the shape of the roster and not about a
# headline number. it is usage in the games sykes actually played, 10 game
# minimum, which is what keeps small sample bench spikes out of the picture.
two_people <- player_box |>
  inner_join(tm_tot, by = c("game_id", "team_location")) |>
  filter(team_location == ME, game_id %in% SYKES_GAMES) |>
  group_by(player = athlete_display_name) |>
  summarise(g = n(),
            usg = 100 * sum(field_goals_attempted + 0.44 * free_throws_attempted + turnovers) *
                  (sum(tmin) / sum(as_min(minutes)) / 5) / sum(tfga + 0.44 * tfta + ttov),
            .groups = "drop") |>
  filter(g >= 10) |>
  mutate(engine = player %in% c("Marina Mabrey", "Brittney Sykes"))

p_two_people <- ggplot(two_people, aes(x = usg, y = reorder(player, usg))) +
  geom_segment(aes(x = 0, xend = usg, yend = player, colour = engine),
               linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
  geom_point(aes(colour = engine, size = engine)) +
  geom_text(aes(label = sprintf("%.1f", usg), colour = engine),
            hjust = 0, nudge_x = 0.9, size = 4.2, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.14))) +
  labs(title = "toronto usage rate while sykes was healthy",
       subtitle = "players with 10 or more games alongside her",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# ---------------------------------------------------------------------------
# 3. "and it did not go to mabrey either." her share barely moved, her scoring did.
mabrey <- tibble::tibble(
  k = factor(rep(c("usage rate", "points per game"), each = 2),
             levels = c("usage rate", "points per game")),
  sykes = factor(rep(c("with sykes", "without sykes"), 2),
                 levels = c("with sykes", "without sykes")),
  v = c(29.2, 30.9, 18.2, 23.5))

p_mabrey <- ggplot(mabrey, aes(x = k, y = v, fill = sykes)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f", v)), position = position_dodge(width = 0.7),
            vjust = -0.6, colour = INK, size = 4.4) +
  scale_fill_manual(values = c(`with sykes` = ALT, `without sykes` = HI), name = NULL) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
  labs(title = "marina mabrey with and without sykes",
       subtitle = "her share of the offense barely moved, her scoring did",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top", panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# 4. "it scattered instead." nobody took her job, eleven people took a piece of it.
# a dumbbell: both ends encode position on a shared scale, so no zero baseline needed.
# KIA NURSE IS BACK ON, 9/7, in the final revision. every value here is hhs, so the chart is
# single sourced and needs no caveat printed on it.
#
# the one thing to know while editing: hhs has nurse at +4.3, which draws a LONGER arrow
# than kiki rice's +3.9, so she is second on the chart. the voiceover says "kia nurse
# just a little." if that reads wrong on screen, the options are to reorder the spoken
# line, or to hold this chart during a different sentence. do not fix it by swapping in
# wehoop's +2.5 for her alone, that would make one row of five come from a different
# source than the rest.
spread <- tibble::tribble(
  ~player,           ~with, ~without, ~lab,
  "Nyara Sabally",    17.7,   24.1,   "+6.4",
  "Kia Nurse",        14.5,   18.8,   "+4.3",
  "Kiki Rice",        17.6,   21.5,   "+3.9",
  "Laura Juskaite",   17.2,   20.0,   "+2.8",
  "Marina Mabrey",    29.2,   30.9,   "+1.7") |>
  mutate(delta = without - with)

# arrows instead of two dots, so direction is carried by SHAPE rather than by colour
# alone. a reader who cannot separate the plum from the light blue still sees which way
# each player moved. the dot marks where she started, with sykes, and the head lands on
# where she ended up without her.
#
# the segment stops ~0.35 short of the true value so the arrowhead TIP sits on the
# number rather than overshooting it, which is what happens if you let the line run the
# full distance and hang a head off the end.
HEAD <- 0.35

p_spread <- ggplot(spread, aes(y = reorder(player, delta))) +
  geom_segment(aes(x = with, xend = without - HEAD, yend = player),
               colour = HI, linewidth = 1.5, alpha = 0.55,
               arrow = arrow(length = grid::unit(0.26, "cm"), type = "closed")) +
  geom_point(aes(x = with), colour = ALT, size = 4) +
  geom_text(aes(x = without, label = lab), hjust = 0, nudge_x = 0.75,
            colour = INK, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0.06, 0.42))) +
  labs(title = "usage rate with sykes and without her",
       subtitle = "each arrow runs from their usage with sykes to their usage without her",
       caption = CREDIT, x = "usage rate", y = NULL) +
  base_theme()

# ---------------------------------------------------------------------------
# 5. "and here's what that cost toronto." the three step ladder.
# the one game with sykes and no mabrey is dropped, a single game is not a group.
ladder <- tor |>
  mutate(grp = case_when(s & m ~ "both played",
                         !s & m ~ "sykes out",
                         !s & !m ~ "neither played",
                         TRUE ~ "drop")) |>
  filter(grp != "drop") |>
  group_by(grp) |>
  summarise(g = n(), w = sum(margin > 0), l = sum(margin < 0), mov = mean(margin), .groups = "drop") |>
  mutate(grp = factor(grp, levels = c("both played", "sykes out", "neither played")))

p_ladder <- ggplot(ladder, aes(x = grp, y = mov)) +
  geom_col(fill = HI, width = 0.55) +
  geom_hline(yintercept = 0, colour = INK, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%d-%d\n%+.2f a game", w, l, mov)),
            vjust = 1.25, colour = INK, size = 4.6) +
  scale_y_continuous(expand = expansion(mult = c(0.3, 0.05))) +
  labs(title = "toronto point differential per game, by who was available",
       subtitle = "the one game with sykes and without mabrey is excluded",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# "and toronto with sykes was eight and nine. without her, three and twenty."
# the two rung version, which is what the video says now. it reintroduces the mabrey
# confound that p_ladder solves, which is exactly why p_ladder stays in the caption.
split2 <- tor |>
  group_by(sykes = factor(if_else(s, "with sykes", "without sykes"),
                          levels = c("with sykes", "without sykes"))) |>
  summarise(g = n(), w = sum(margin > 0), l = sum(margin < 0), mov = mean(margin),
            .groups = "drop")

p_split <- ggplot(split2, aes(x = sykes, y = mov)) +
  geom_col(aes(fill = sykes == "with sykes"), width = 0.5) +
  geom_hline(yintercept = 0, colour = INK, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%d-%d\n%+.2f a game", w, l, mov)),
            vjust = 1.25, colour = INK, size = 5) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = ALT), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.32, 0.05))) +
  labs(title = "toronto with and without brittney sykes",
       subtitle = "record and point differential per game",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# CAPTION ONLY from here down. these are not in the spoken script.

# the offense/defense split. both ends fell by about the same amount.
ends <- tor |> group_by(sykes = if_else(s, "with sykes", "without sykes")) |>
  summarise(`points scored` = mean(team_score),
            `points allowed` = mean(opponent_team_score), .groups = "drop") |>
  pivot_longer(-sykes, names_to = "k", values_to = "v") |>
  mutate(sykes = factor(sykes, levels = c("with sykes", "without sykes")))

p_ends <- ggplot(ends, aes(x = k, y = v, fill = sykes)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f", v)), position = position_dodge(width = 0.7),
            vjust = -0.6, colour = INK, size = 4.4) +
  scale_fill_manual(values = c(`with sykes` = HI, `without sykes` = ALT), name = NULL) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
  labs(title = "toronto scoring and defense, with and without sykes",
       subtitle = "both ends fell by about the same amount",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top", panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# starting fives. cut from the final video 9/6, still in the caption, and it sets up day 12.
fives <- player_box |> filter(starter) |>
  group_by(team = team_location, game_id) |>
  summarise(five = paste(sort(athlete_display_name), collapse = "|"), .groups = "drop") |>
  group_by(team) |> summarise(fives = n_distinct(five), .groups = "drop")
PAD <- 0.035 * max(fives$fives)

p_fives <- ggplot(fives, aes(x = fives, y = reorder(team, fives))) +
  geom_segment(aes(x = 0, xend = fives, yend = team, colour = team == ME),
               linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
  geom_point(aes(colour = team == ME, size = team == ME)) +
  geom_text(aes(label = fives, colour = team == ME), hjust = 0, nudge_x = PAD,
            size = 4.2, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.14))) +
  labs(title = "distinct starting fives used, by team",
       subtitle = "wehoop counts 22 for toronto and hhs counts 21, the rest of the order is the same",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# render one at a time in positron, in script order.
# for 9:16 size the plot pane tall and narrow.
# script order. the kicker goes last on purpose.
p_top10
p_duo_share
p_two_people
p_spread
p_split
p_sykes

# caption only
p_ladder
p_mabrey
p_ends
p_fives
