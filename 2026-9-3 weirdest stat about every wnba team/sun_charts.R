# charts for the connecticut sun "weird 8/15" video
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

ALLSTAR   <- c("Team Spoon", "Team Coop")
CUP_FINAL <- as.Date("2026-06-30")
ME        <- "Connecticut"

HI     <- "#F05023"   # connecticut sun. swap this hex if you want a different color
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

player_box <- load_wnba_player_box(seasons = 2026) |>
  filter(season_type == 2, !team_location %in% ALLSTAR, game_date != CUP_FINAL, !did_not_play)
as_min <- function(x) { x <- as.character(x); ifelse(is.na(x), 0, suppressWarnings(as.numeric(x))) }

# 1. THE LEAD. lowest three point rate in the league by a mile
p_3rate <- lolli(teams, p3rate, "%.1f",
  "share of shot attempts taken from three, by team",
  NULL, low_first = TRUE)

# 2. the other side of the same fact
p_2pa <- lolli(teams, pa2, "%.1f",
  "two point attempts per game, by team",
  NULL)

# 3. THE FAIRNESS BEAT. last in all three shooting categories at once.
# small multiples so you can see the sun sitting at the bottom of every panel.
shoot <- teams |> select(team, efg, p3, ftpct) |>
  pivot_longer(-team, names_to = "stat", values_to = "v") |>
  mutate(stat = recode(stat, efg = "effective fg%", p3 = "three point %", ftpct = "free throw %"),
         stat = factor(stat, levels = c("effective fg%", "three point %", "free throw %")),
         row = paste0(team, "\u0001", as.integer(stat)))

# PER PANEL SORTING. reorder(team, v) computes a single global ordering across all
# facets, so scales = "free_y" alone leaves every panel sorted by the first metric and
# the bars come out visibly out of order. tagging each row with its panel makes the
# ordering local, and the axis labeller strips the tag back off.
p_shoot <- ggplot(shoot, aes(x = v, y = reorder(row, v), fill = team == ME)) +
  geom_col(width = 0.68) +
  facet_wrap(~stat, nrow = 1, scales = "free_y") +
  scale_y_discrete(labels = function(x) sub("\u0001.*$", "", x)) +
  scale_fill_manual(values = c(`FALSE` = "#D6D5D0", `TRUE` = HI), guide = "none") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  labs(title = "three shooting categories, each ranked separately",
       subtitle = "each panel sorted independently",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(axis.text.y = element_text(size = 9),
        strip.text = element_text(colour = INK, size = 12, face = "bold"))

# 4. the worst fourth quarter in the league
p_q4 <- lolli0(qtr |> mutate(team = team), d4, "%+.2f",
  "fourth quarter point differential per game, by team",
  NULL)

# 5. nobody has separated. flattest scoring distribution in the league.
share <- player_box |> group_by(team = team_location, p = athlete_display_name) |>
  summarise(pts = sum(points), .groups = "drop") |>
  group_by(team) |> mutate(share = 100 * pts / sum(pts)) |>
  slice_max(pts, n = 1) |> ungroup() |> select(team, p, share)
p_share <- lolli(share, share, "%.1f",
  "share of team points taken by the team's top scorer",
  NULL, low_first = TRUE)

# ---------------------------------------------------------------------------
# THE NEW LEAD, 9/9. "nobody shoots more twos than connecticut, nobody makes fewer"
#
# a scatter, so both axes encode position and neither needs a zero baseline. the dashed
# lines are league averages, which turns the panel into four quadrants and leaves
# connecticut alone in the one nobody wants: most volume, worst accuracy.
two_pt <- team_box |>
  group_by(team = team_location) |>
  summarise(pa2 = (sum(field_goals_attempted) - sum(three_point_field_goals_attempted)) / n(),
            p2  = 100 * (sum(field_goals_made) - sum(three_point_field_goals_made)) /
                        (sum(field_goals_attempted) - sum(three_point_field_goals_attempted)),
            .groups = "drop")

LG_PA2 <- mean(two_pt$pa2)
LG_P2  <- 100 * (sum(team_box$field_goals_made) - sum(team_box$three_point_field_goals_made)) /
                (sum(team_box$field_goals_attempted) - sum(team_box$three_point_field_goals_attempted))

p_2pt <- ggplot(two_pt, aes(x = pa2, y = p2)) +
  geom_hline(yintercept = LG_P2,  colour = "#D6D5D0", linetype = "22") +
  geom_vline(xintercept = LG_PA2, colour = "#D6D5D0", linetype = "22") +
  geom_point(aes(colour = team == ME, size = team == ME)) +
  ggrepel::geom_text_repel(aes(label = team, colour = team == ME),
                           size = 4, seed = 1, max.overlaps = 20, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3, `TRUE` = 5.6), guide = "none") +
  # connecticut sits at the far right and its label runs off the panel without this
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.18))) +
  labs(title = "two point attempts against two point percentage",
       subtitle = sprintf("dashed lines are league average. connecticut takes the most and makes %.1f%%, last :/",
                          two_pt$p2[two_pt$team == ME]),
       caption = CREDIT, x = "two point attempts per game", y = "two point %") +
  base_theme() +
  theme(panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ---------------------------------------------------------------------------
# "nobody on connecticut averages fourteen points a game, every other team has somebody"
#
# a printed table rather than a chart, for reading off camera and for answering comments.
#
# NO GAMES MINIMUM ON PURPOSE. the usual way a claim like this dies is somebody saying
# "well X averages 15 in six games," so this takes each team's best per game scorer with
# no filter at all. checked 9/10 at 1, 5, 10, 15 and 20 game minimums and connecticut is
# the only team under 14.0 at every single one. their ceiling is griner at 13.22 in 18
# games and nobody else on the roster is above 11.63, so there is no small sample player
# hiding above the line.
#
# it also survives being made STRICTER: at a 20 game minimum connecticut's best becomes
# leila lacan at 11.63, which widens the gap rather than closing it.
top_scorers <- player_box |>
  group_by(team = team_location, player = athlete_display_name) |>
  summarise(g = n(), ppg = mean(points), .groups = "drop") |>
  group_by(team) |>
  slice_max(ppg, n = 1) |>
  ungroup() |>
  transmute(team, player, g, ppg = round(ppg, 1),
            under_14 = ppg < 14) |>
  arrange(ppg)

top_scorers

p_2pt
p_3rate
p_2pa
p_shoot
p_q4
p_share
