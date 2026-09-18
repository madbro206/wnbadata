# charts for the phoenix mercury "weird 14/15" video
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
library(gt)

ALLSTAR   <- c("Team Spoon", "Team Coop")
CUP_FINAL <- as.Date("2026-06-30")
ME        <- "Phoenix"

HI     <- "#E56020"   # phoenix mercury. swap this hex if you want a different color
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

phx <- player_box |> filter(team_location == ME)
PHX_AST <- sum(phx$assists)

# 1. THE LEAD. one player, four out of every ten assists.
# a single stacked bar reads better than a pie and keeps the part to whole honest.
split <- tibble::tibble(
  who = factor(c("alyssa thomas", "the other 11 mercury players"),
               levels = c("the other 11 mercury players", "alyssa thomas")),
  ast = c(sum(phx$assists[phx$athlete_display_name == "Alyssa Thomas"]),
          PHX_AST - sum(phx$assists[phx$athlete_display_name == "Alyssa Thomas"])))
split$pct <- 100 * split$ast / sum(split$ast)

p_share <- ggplot(split, aes(x = ast, y = "", fill = who)) +
  geom_col(width = 0.42) +
  geom_text(aes(label = sprintf("%s\n%d assists  \u00b7  %.1f%%", who, ast, pct)),
            position = position_stack(vjust = 0.5), colour = "white", size = 4.6, fontface = "bold") +
  scale_fill_manual(values = c(`alyssa thomas` = HI, `the other 11 mercury players` = "#9C9A94"),
                    guide = "none") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.02))) +
  labs(title = "phoenix assists, thomas against the rest of the roster",
       subtitle = sprintf("all %d phoenix assists this season", PHX_AST),
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(), axis.text = element_blank())

# 2. the gap to her own teammates
mercs <- phx |> group_by(team = athlete_display_name) |>
  summarise(apg = mean(assists), .groups = "drop") |> slice_max(apg, n = 8)
p_apg <- lolli(mercs, apg, "%.1f",
  "assists per game, phoenix",
  NULL, hi_when = "Alyssa Thomas")

# 3. THE MONEY CHART. usage against assists for every rotation player in the league.
# thomas is the outlier nobody else is near: she creates the most while shooting the least
# of anyone in that tier. a scatter, so both axes encode position and no zero baseline needed.
tm <- player_box |> group_by(game_id, team_location) |>
  summarise(tfga = sum(field_goals_attempted), tfta = sum(free_throws_attempted),
            ttov = sum(turnovers), tmin = sum(as_min(minutes)), .groups = "drop")
usage <- player_box |> inner_join(tm, by = c("game_id", "team_location")) |>
  group_by(player = athlete_display_name, team = team_location) |>
  summarise(g = n(), apg = mean(assists),
            usg = 100 * sum(field_goals_attempted + 0.44 * free_throws_attempted + turnovers) *
                  (sum(tmin) / sum(as_min(minutes)) / 5) / sum(tfga + 0.44 * tfta + ttov),
            .groups = "drop") |>
  filter(g >= 20)

p_usg <- ggplot(usage, aes(x = usg, y = apg)) +
  geom_point(aes(colour = player == "Alyssa Thomas", size = player == "Alyssa Thomas"), alpha = 0.85) +
  ggrepel::geom_text_repel(
    data = usage |> filter(apg >= 5.2 | usg >= 30 | player == "Alyssa Thomas"),
    aes(label = player, colour = player == "Alyssa Thomas"),
    size = 3.8, seed = 1, max.overlaps = 20, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 2.6, `TRUE` = 6), guide = "none") +
  labs(title = "assists per game against usage rate, 20 or more games",
       subtitle = "20 or more games",
       caption = CREDIT, x = "usage rate", y = "assists per game") +
  base_theme() +
  theme(panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 4. she runs it while shooting less than the person she sets up
duo <- usage |> filter(player %in% c("Alyssa Thomas", "Kahleah Copper")) |>
  select(team = player, usg)
p_duo <- lolli(duo, usg, "%.1f",
  "usage rate, thomas and copper",
  NULL, hi_when = "Alyssa Thomas")

# ---------------------------------------------------------------------------
# REBUILT 9/16: the three point inversion. computed live from wehoop player box, cross checked
# against her hoop stats (phoenix top two 28.2% on 386 attempts, everyone else 36.4% on 536).

# every player's threes, all teams
shooters <- player_box |>
  filter(!did_not_play) |>
  group_by(team = team_location, player = athlete_display_name) |>
  summarise(m3 = sum(three_point_field_goals_made, na.rm = TRUE),
            a3 = sum(three_point_field_goals_attempted, na.rm = TRUE), .groups = "drop") |>
  filter(a3 >= 10) |>
  mutate(p3 = 100 * m3 / a3)

# 5. PHOENIX, 9/16: accuracy against how long they were actually there. the accurate shooters
# were mostly part season players. games are phoenix games only, so akoa makani's 19 stops at the
# trade and plum's 5 start when she arrived. dot size is three point attempts.
# count only games with actual minutes: wehoop's did_not_play flag misses a couple of
# appearances, which made nogic read 18 games against her hoop stats' 16.
phx_shooters <- player_box |>
  filter(team_location == ME, !did_not_play, as_min(minutes) > 0) |>
  group_by(player = athlete_display_name) |>
  summarise(g = n(),
            m3 = sum(three_point_field_goals_made, na.rm = TRUE),
            a3 = sum(three_point_field_goals_attempted, na.rm = TRUE), .groups = "drop") |>
  filter(a3 >= 20) |>
  mutate(p3 = 100 * m3 / a3,
         grp = if_else(rank(-a3) <= 2, "took the most threes", "everyone else"))

p_phx_shooters <- ggplot(phx_shooters, aes(x = g, y = p3)) +
  geom_hline(yintercept = 100 * sum(shooters$m3) / sum(shooters$a3),
             linetype = "22", colour = GRAY, linewidth = 0.5) +
  annotate("text", x = 40, y = 100 * sum(shooters$m3) / sum(shooters$a3), vjust = -0.8,
           hjust = 1, size = 3.6, colour = GRAY, label = "league average") +
  geom_point(aes(colour = grp, size = a3), alpha = 0.9) +
  geom_text_repel(aes(label = sprintf("%s (%d g)", player, g), colour = grp), size = 3.9,
                  seed = 4, box.padding = 0.5, show.legend = FALSE) +
  scale_colour_manual(values = c(`took the most threes` = HI, `everyone else` = GRAY), name = NULL) +
  scale_size_area(max_size = 9, guide = "none") +
  scale_x_continuous(limits = c(0, 42), breaks = seq(0, 40, 10)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "phoenix three point shooting against games played",
       subtitle = "the mercury's best shooters were mostly only there for part of the season",
       caption = CREDIT, x = "games played for phoenix", y = NULL) +
  base_theme() +
  theme(legend.position = "top",
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 6. EVERY TEAM: the two highest volume shooters against the rest of the roster.
# positive means the main shooters are the better shooters, which is how it normally works.
top2_gap <- shooters |>
  group_by(team) |>
  mutate(vol_rank = rank(-a3, ties.method = "first")) |>
  summarise(top2 = 100 * sum(m3[vol_rank <= 2]) / sum(a3[vol_rank <= 2]),
            rest = 100 * sum(m3[vol_rank > 2]) / sum(a3[vol_rank > 2]), .groups = "drop") |>
  mutate(gap = top2 - rest, mine = team == ME)

p_top2_gap <- ggplot(top2_gap, aes(x = gap, y = reorder(team, gap), fill = mine)) +
  geom_col(width = 0.72, show.legend = FALSE) +
  geom_vline(xintercept = 0, colour = INK, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.1f", gap), hjust = if_else(gap >= 0, -0.15, 1.15)),
            colour = INK, size = 3.9) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = "#C9C8C2"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.14, 0.12))) +
  labs(title = "are a team's main three point shooters its best ones?",
       subtitle = "top two by attempts minus everyone else, in three point percentage",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# ---------------------------------------------------------------------------
# ALL THE CHARTS. run any of these on its own to render it.


# ---------------------------------------------------------------------------
# SHOT CHART: every basket alyssa thomas assisted in 2026 (regular season)
#
# this one does NOT come from wehoop. wehoop's pbp drops the assisting player for
# some teams, so the 320 shots come from her hoop stats shot level data, which
# stores the assister as an id instead of parsing it out of the description text.
#
# the 320 shots are inlined below so this file has no external dependency and does
# not care what your working directory is. the same data is in data/at_assists_2026.csv
#
# coordinates: hhs stores full court at 12 units per foot. shots live at both
# baskets, so they are folded onto one half and re centered on the rim. the fold
# was checked against shot_distance_ft and lands within 0.085 ft on average.
#   x = feet left/right of the rim,  y = feet out from the rim (rim at 0, 0)
at_ast <- data.frame(
  x = c(-0.2, -0.2, -0.6, -1.2, -1.6, -1.8, -10.2, -16.6, -16.6, -17.1, -18.8, -2.2, -2.4,
        -2.8, -2.9, -21.0, -3.0, -3.1, -4.7, -7.8, 0.0, 0.8, 0.9, 1.7, 10.3, 13.3, 18.5, 19.0,
        19.7, 2.1, 2.5, 2.5, 22.4, 23.0, 23.0, 23.0, 23.5, 3.3, 4.3, 9.4, -0.3, -0.5, -0.7,
        -1.3, -1.5, -1.6, -1.8, -13.9, -15.0, -16.6, -2.5, -3.2, -4.3, -5.2, -5.3, -5.5, -5.5,
        0.0, 0.1, 0.3, 0.3, 11.7, 11.9, 13.3, 14.5, 17.4, 18.8, 2.0, 2.2, 2.3, 2.5, 2.8, 20.9,
        22.7, 23.2, 3.2, 3.4, 7.2, 8.0, 9.6, -0.3, -1.0, -1.4, -1.6, -1.8, -13.5, -2.4, -2.7,
        -20.8, -21.5, -22.9, -3.7, -4.0, -6.2, -9.3, -9.4, -9.7, 0.3, 0.6, 1.6, 1.6, 1.8,
        10.9, 11.1, 11.3, 15.4, 15.6, 18.7, 2.2, 2.4, 22.9, 23.2, 23.5, 4.1, 4.3, 4.5, 5.0,
        5.8, 6.7, 9.0, -0.7, -1.6, -11.0, -11.2, -13.1, -13.7, -2.3, -2.3, -2.6, -20.1, -20.3,
        -22.0, -23.4, -6.6, -8.3, -9.5, 0.0, 0.1, 0.6, 1.0, 1.7, 1.7, 15.3, 15.6, 16.7, 17.3,
        18.8, 2.2, 2.6, 2.6, 21.6, 22.9, 23.2, 23.4, 3.4, 3.8, 3.8, 5.1, 6.6, 7.2, -0.7, -0.8,
        -0.8, -1.0, -11.3, -14.8, -17.0, -2.1, -2.9, -23.6, -24.4, -6.8, -8.2, -9.2, 0.1, 0.3,
        0.3, 0.9, 1.2, 1.7, 1.7, 14.5, 15.8, 16.1, 19.8, 2.2, 2.3, 2.9, 20.3, 22.3, 22.8,
        22.9, 22.9, 23.2, 23.2, 23.3, 3.1, 3.3, 3.4, 4.3, -0.4, -0.4, -0.8, -1.0, -1.3, -10.2,
        -11.0, -11.3, -16.3, -18.4, -19.1, -20.0, -20.8, -23.1, -23.3, -3.1, -3.2, -3.2, -3.7,
        -5.3, -5.3, -5.8, -7.2, 1.3, 1.4, 1.8, 12.7, 13.2, 13.8, 14.5, 14.8, 19.3, 2.0, 2.1,
        2.1, 20.3, 22.9, 23.0, 23.2, 5.3, -0.3, -1.0, -1.3, -1.8, -1.8, -12.8, -17.2, -19.7,
        -19.8, -2.0, -2.1, -2.3, -2.7, -20.2, -3.7, -6.6, -8.3, 0.3, 0.4, 0.6, 1.0, 1.1, 1.4,
        1.8, 10.6, 12.3, 17.0, 17.9, 19.8, 2.7, 2.9, 20.1, 20.3, 20.8, 20.8, 21.6, 21.6, 23.3,
        23.3, 5.0, -0.2, -0.3, -0.4, -0.8, -1.1, -1.8, -2.0, -2.0, -2.3, -20.0, -21.3, -22.5,
        -23.3, -23.6, -3.6, -4.0, -6.2, -8.9, 0.3, 14.8, 16.4, 17.9, 2.2, 2.3, 2.7, 2.8, 2.9,
        20.2, 22.0, 22.7, 22.9, 23.0, 23.3, 23.6, 3.0, 3.2, 3.9, 4.8, 5.3, 6.9),
  y = c(1.3, 26.6, 1.0, 27.7, -0.3, 3.6, 9.5, 19.6, 20.3, 22.2, 15.8, 2.0, 24.7, 25.6, 24.4,
        11.9, 6.2, 3.3, -0.9, 23.6, 1.7, 1.3, -0.2, 25.5, 2.4, 20.8, 18.6, 23.0, 15.0, 0.4,
        0.7, 1.2, -1.7, -3.3, 0.4, 6.6, 0.1, 0.7, 2.8, 22.6, 2.3, 11.6, 2.0, 2.8, 0.6, 1.5,
        1.3, 20.8, 17.9, 0.9, 19.7, 1.3, 0.3, 12.2, 9.7, 25.5, 3.8, 1.0, 11.9, 0.4, 1.4, 7.7,
        24.5, 21.8, 0.4, 15.7, 19.6, 2.6, 2.0, -0.4, 2.3, 2.5, 15.7, 3.8, -1.9, 2.9, 0.6, 2.3,
        19.3, 26.3, 0.9, 0.9, -1.8, 24.7, 0.5, 20.5, 1.2, 0.7, 11.2, 15.8, 8.8, 4.4, 0.8,
        14.0, 23.5, 1.2, 0.1, 6.6, 4.3, 1.7, 7.8, 0.6, 13.1, 22.8, 8.3, 21.4, 18.0, 17.8,
        16.7, 0.5, 7.5, 2.4, -1.9, 5.6, 1.4, 26.0, 18.6, 23.3, 23.1, 21.9, 2.1, 4.3, 14.9,
        22.0, 7.5, 19.8, 2.3, 2.3, 1.4, 13.8, 20.0, 11.8, -1.2, 18.6, 5.7, 1.3, 1.3, 0.7, 0.7,
        24.5, 1.9, 4.3, 18.4, 7.7, 16.5, 20.8, 22.8, 0.9, 0.7, 1.7, 8.4, 2.6, -1.3, 0.3, 26.1,
        0.3, 1.0, 25.8, 25.1, 23.0, 0.9, 15.9, 20.5, 1.1, 2.2, 8.5, 17.0, 3.8, 3.5, 0.0, 2.4,
        0.2, 18.8, 1.8, 25.6, 0.9, 7.2, -0.1, 0.2, -0.1, 0.3, 20.9, 7.9, 17.8, 13.0, 23.1,
        1.2, 1.4, 17.8, 6.2, 3.9, -0.6, 1.2, 2.0, 2.0, -2.2, 15.4, 3.5, 1.4, 25.5, 1.0, 1.2,
        0.8, 1.4, 0.5, 23.2, 23.2, 14.1, 18.5, 17.4, 14.8, 16.9, 12.5, 9.3, 0.4, 1.5, -0.4,
        3.1, 18.8, 2.9, 5.8, 17.0, 3.4, 2.2, 1.7, 3.8, -1.0, 20.6, 16.3, 20.7, 20.8, 18.6,
        2.0, 0.9, 24.4, 9.7, -1.7, -3.3, -2.2, 7.7, -0.2, 0.0, 0.8, 2.0, 2.8, 23.0, 21.3,
        16.9, 14.2, 2.0, -0.1, 1.3, 2.0, 13.8, 0.7, 25.0, 18.9, 1.6, 4.3, 8.2, 2.7, 0.3, -0.4,
        1.5, -0.8, 20.0, 19.6, 16.2, 21.9, 24.1, 2.4, 15.3, 20.7, 12.1, 13.6, 10.9, 8.2, 2.3,
        5.3, 2.5, -0.2, 1.8, 1.3, -0.2, 3.7, 0.8, 1.4, 4.9, 1.0, 10.3, 12.3, 6.1, 1.8, 4.7,
        1.8, 2.8, 14.2, 3.0, -0.5, 2.5, 16.6, 17.0, 2.0, 1.7, 1.1, 3.2, 0.1, 14.9, 7.5, 3.8,
        -1.8, 0.7, 2.8, 1.8, 25.6, 0.1, 1.4, 5.4, 14.1, 23.4),
  shot_value = c(2, 3, 2, 3, 2, 2, 2, 3, 3, 3, 3, 2, 3, 3, 3, 3, 2, 2, 2, 3, 2, 2, 2, 3, 2, 3, 3, 3, 3,
                 2, 2, 2, 3, 3, 3, 3, 3, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 3, 2, 2, 2, 2, 2, 2, 3, 2, 2,
                 2, 2, 2, 2, 3, 3, 2, 3, 3, 2, 2, 2, 2, 2, 3, 3, 3, 2, 2, 2, 2, 3, 2, 2, 2, 3, 2, 3, 2,
                 2, 3, 3, 3, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 3, 3, 3, 2, 2, 3, 3, 3, 2, 2, 3,
                 2, 3, 3, 3, 2, 2, 2, 3, 2, 3, 2, 2, 2, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 3, 2, 2, 3, 2, 3,
                 3, 3, 2, 2, 2, 3, 3, 3, 3, 3, 2, 2, 3, 3, 3, 2, 2, 2, 2, 2, 2, 3, 2, 2, 3, 3, 2, 2, 2,
                 3, 2, 2, 2, 2, 2, 2, 3, 2, 3, 3, 3, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 3, 2, 2, 2,
                 2, 2, 3, 3, 2, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 3, 3, 3,
                 2, 2, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 2, 2, 2, 2, 3, 2, 3, 2, 2, 2, 2, 2,
                 2, 2, 2, 2, 3, 3, 3, 3, 3, 2, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3,
                 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 3, 3, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2,
                 3)
) |>
  mutate(kind = if_else(shot_value == 3, "three", "two"))

# wnba half court, in feet, with the rim at the origin
court <- function(colour = "#C9C7C1", lw = 0.4) {
  arc <- function(r, a1, a2, n = 200) {
    a <- seq(a1, a2, length.out = n); data.frame(x = r * cos(a), y = r * sin(a))
  }
  three_break <- asin(2.57 / 22.15)   # where the arc meets the straight corner
  list(
    # baseline, sidelines
    annotate("segment", x = -25, xend = 25, y = -5.25, yend = -5.25, colour = colour, linewidth = lw),
    annotate("segment", x = -25, xend = -25, y = -5.25, yend = 30, colour = colour, linewidth = lw),
    annotate("segment", x =  25, xend =  25, y = -5.25, yend = 30, colour = colour, linewidth = lw),
    # paint (16 ft lane) and free throw line 19 ft from the baseline
    annotate("segment", x = -8, xend = -8, y = -5.25, yend = 13.75, colour = colour, linewidth = lw),
    annotate("segment", x =  8, xend =  8, y = -5.25, yend = 13.75, colour = colour, linewidth = lw),
    annotate("segment", x = -8, xend =  8, y = 13.75, yend = 13.75, colour = colour, linewidth = lw),
    geom_path(data = arc(6, 0, pi) |> mutate(y = y + 13.75), aes(x, y),
              colour = colour, linewidth = lw, inherit.aes = FALSE),
    # rim and backboard
    geom_path(data = arc(0.75, 0, 2 * pi), aes(x, y), colour = colour, linewidth = lw, inherit.aes = FALSE),
    annotate("segment", x = -3, xend = 3, y = -1.25, yend = -1.25, colour = colour, linewidth = lw),
    # three point line: straight corners out to 22 ft, then the 22.15 ft arc
    annotate("segment", x = -22, xend = -22, y = -5.25, yend = 2.57, colour = colour, linewidth = lw),
    annotate("segment", x =  22, xend =  22, y = -5.25, yend = 2.57, colour = colour, linewidth = lw),
    geom_path(data = arc(22.15, three_break, pi - three_break), aes(x, y),
              colour = colour, linewidth = lw, inherit.aes = FALSE)
  )
}

p_at_shotchart <- ggplot(at_ast, aes(x, y)) +
  court() +
  geom_point(aes(colour = kind), size = 2.6, alpha = 0.75) +
  scale_colour_manual(values = c(three = HI, two = INK), name = NULL,
                      labels = c(three = "three", two = "two")) +
  coord_fixed(xlim = c(-25.5, 25.5), ylim = c(-6, 30), expand = FALSE) +
  labs(title = "every basket alyssa thomas set up in 2026",
       subtitle = "320 assists, more than anyone in the league. that is 27% of every basket phoenix made",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = "top", legend.justification = "left")



# ---------------------------------------------------------------------------
# CHARTS FOR THE 9/16 TWIST SCRIPT. all of these come from her hoop stats
# (2026 regular season), not wehoop, so the numbers are typed in as data frames
# rather than recomputed. verified 9/16.

# "the phoenix mercury used twenty one different players this season"
players_used <- data.frame(
  team = c("Phoenix","Toronto","Los Angeles","Chicago","New York","Portland",
           "Indiana","Atlanta","Seattle","Minnesota","Dallas","Connecticut",
           "Washington","Golden State","Las Vegas"),
  n    = c(21,20,18,17,17,17,16,16,16,16,16,15,15,15,14)
) |> mutate(mine = team == ME)

p_players_used <- ggplot(players_used, aes(x = n, y = reorder(team, n), fill = mine)) +
  geom_col(width = 0.72, show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.35, size = 4.2, colour = INK) +
  scale_fill_manual(values = c(`FALSE` = GRAY, `TRUE` = HI)) +
  scale_x_continuous(limits = c(0, 23), expand = c(0, 0)) +
  labs(title = "players used in 2026",
       subtitle = "phoenix gave a game to more players than anyone else",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# "tied with last year's wings for the most of any team since twenty ten"
most_players_hist <- data.frame(
  label = c("2026 phoenix","2025 dallas","2026 toronto","2022 minnesota",
            "2025 golden state","2026 los angeles","2025 indiana","2023 phoenix",
            "2010 dallas","2026 new york","2026 chicago","2026 portland"),
  n     = c(21,21,20,20,19,18,18,18,18,17,17,17)
) |> mutate(mine = label == "2026 phoenix")

p_most_players_hist <- ggplot(most_players_hist, aes(x = n, y = reorder(label, n), fill = mine)) +
  geom_col(width = 0.72, show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.35, size = 4.2, colour = INK) +
  scale_fill_manual(values = c(`FALSE` = GRAY, `TRUE` = HI)) +
  scale_x_continuous(limits = c(0, 23), expand = c(0, 0)) +
  labs(title = "most players used in a season since 2010",
       subtitle = "phoenix ties last year's wings at 21",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# "ten of them played fewer than sixteen games. only four were there
# basically the whole year"
phx_games <- data.frame(
  player = c("Brochant","Copper","Thomas","Bonner","Ayayi","Mack","Held",
             "Linskens","Akoa Makani","Whitcomb","Nogic","Suarez","Carter",
             "K. Williams","Plum","Ciezki","Westbeld","Maley","P. Williams",
             "Dunn","Poffenbarger"),
  g      = c(39,38,38,37,33,32,30,19,19,17,16,15,13,8,5,4,3,2,1,1,1)
) |> mutate(short = g < 16)

p_phx_games <- ggplot(phx_games, aes(x = g, y = reorder(player, g), fill = short)) +
  geom_col(width = 0.72) +
  scale_fill_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), name = NULL,
                    labels = c(`FALSE` = "16 or more games", `TRUE` = "fewer than 16")) +
  scale_x_continuous(limits = c(0, 41), expand = c(0, 0)) +
  labs(title = "how long each mercury player actually stayed",
       subtitle = "ten of the twenty one played fewer than sixteen games",
       caption = CREDIT, x = "games played", y = NULL) +
  base_theme() +
  theme(legend.position = "top", legend.justification = "left")

# "one player set up more than a quarter of every basket
# phoenix made. nobody else in the league is above twenty three percent"
creator_share <- data.frame(
  player = c("Thomas","Canada","Clark","Burton","Gray","Young","Leite","Bueckers",
             "Miles","Wheeler","Cloud","Hiedeman","Shepard","Citron","Allemand",
             "Melbourne","Leger-Walker","C. Williams","Amoore","Howard"),
  pct    = c(27.4,23.0,22.1,20.1,20.1,19.6,18.5,17.0,16.7,15.9,15.5,14.5,14.2,
             13.6,13.5,12.8,12.6,12.4,11.8,11.2)
) |> mutate(mine = player == "Thomas")

p_creator_share <- ggplot(creator_share, aes(x = pct, y = reorder(player, pct), fill = mine)) +
  geom_col(width = 0.72, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.2, size = 3.9, colour = INK) +
  scale_fill_manual(values = c(`FALSE` = GRAY, `TRUE` = HI)) +
  scale_x_continuous(limits = c(0, 32), expand = c(0, 0)) +
  labs(title = "share of her team's baskets that she set up",
       subtitle = "every player with 100 or more assists in 2026",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# "seventeen other players made a field goal for phoenix. she set up
# sixteen of them." thomas is excluded because you cannot assist your own
# basket. ciezki is the only other scorer she never set up.
phx_scorers <- data.frame(
  player   = c("Copper","Bonner","Mack","Brochant","Held","Akoa Makani","Nogic",
               "Ayayi","Whitcomb","Plum","Suarez","Linskens","K. Williams",
               "Ciezki","Westbeld","Carter","Dunn"),
  fgm      = c(272,130,115,95,63,58,50,49,27,27,22,19,12,10,4,3,3),
  from_at  = c(94,53,23,40,21,18,18,14,14,8,1,8,5,0,1,1,1)
) |> mutate(other = fgm - from_at) |>
  tidyr::pivot_longer(c(from_at, other), names_to = "src", values_to = "n") |>
  # levels put thomas's share at the axis end of each bar so it is comparable across players
  mutate(src = factor(src, levels = c("other", "from_at")))

p_phx_scorers <- ggplot(phx_scorers,
                        aes(x = n, y = reorder(player, n), fill = src)) +
  geom_col(width = 0.72) +
  scale_fill_manual(values = c(from_at = HI, other = GRAY), name = NULL,
                    breaks = c("from_at", "other"),
                    labels = c(from_at = "set up by thomas", other = "everything else")) +
  scale_x_continuous(limits = c(0, 290), expand = c(0, 0)) +
  labs(title = "who scored for phoenix, and who set them up",
       caption = CREDIT, x = "field goals made", y = NULL) +
  base_theme() +
  theme(legend.position = "top", legend.justification = "left")

# "it's alyssa thomas, who has made two three pointers in her entire career"
at_threes <- data.frame(
  season = 2014:2026,
  a3     = c(5,0,0,3,1,1,1,0,2,3,4,6,3),
  m3     = c(1,0,0,0,0,0,0,0,0,0,0,0,1)
)

p_at_threes <- ggplot(at_threes, aes(x = season)) +
  geom_col(aes(y = a3), fill = GRAY, width = 0.68) +
  geom_col(aes(y = m3), fill = HI, width = 0.68) +
  geom_text(data = subset(at_threes, m3 > 0), aes(y = a3, label = "made one"),
            vjust = -0.8, size = 3.6, colour = HI) +
  scale_x_continuous(breaks = seq(2014, 2026, 2)) +
  scale_y_continuous(limits = c(0, 7), expand = c(0, 0)) +
  labs(title = "alyssa thomas from three, her whole career",
       subtitle = "29 regular season attempts, 2 makes. one in 2014 and one in 2026",
       caption = CREDIT, x = NULL, y = "three point attempts") +
  base_theme() +
  theme(panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3),
        panel.grid.major.x = element_blank())


# ---------------------------------------------------------------------------
# "interestingly though they don't have the most unique starting fives this year"
#
# two different kinds of churn. phoenix cycled the most PEOPLE through the roster
# (21 players, ten of them for fewer than 16 games) but toronto shuffled the
# COMBINATIONS harder, with more starting fives off one fewer player. so phoenix
# is 3rd here, not 1st. hhs box scores, gs = 1, all 15 teams have 40 games with
# exactly 5 starters. verified 9/16.
starting_fives <- data.frame(
  team  = c("Toronto","Connecticut","Phoenix","New York","Chicago","Seattle",
            "Dallas","Portland","Atlanta","Indiana","Los Angeles","Golden State",
            "Washington","Las Vegas","Minnesota"),
  used  = c(20,15,21,17,17,16,16,17,16,16,18,15,15,14,16),
  fives = c(22,18,17,14,12,11,10,9,7,7,7,6,6,5,4)
)

t_starting_fives <- starting_fives |>
  gt() |>
  cols_label(team = "", used = "players used", fives = "starting fives") |>
  tab_header(title = "different starting fives in 2026") |>
  cols_align(align = "right", columns = c(used, fives)) |>
  tab_style(style = list(cell_fill(color = HI), cell_text(color = "white", weight = "bold")),
            locations = cells_body(rows = team == "Phoenix")) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels()) |>
  tab_source_note(CREDIT) |>
  tab_options(table.font.size = 15, data_row.padding = 4,
              table.border.top.style = "none", heading.border.bottom.style = "none")

# ---------------------------------------------------------------------------
# THE ONE PLAYER THOMAS NEVER SET UP: shay ciezki.
#
# shared court time from her hoop stats lineup data (lineup stints joined
# to lineup, phoenix 2026 regular season). they overlapped for 17.2 minutes and
# 37 possessions across 2 games, about 1.4% of thomas's season on the floor.
# verified 9/16.
at_ciezki <- data.frame(
  who  = c("thomas", "ciezki", "together"),
  mins = c(1273.4, 49.0, 17.2),
  poss = c(2527, 100, 37)
)

t_at_ciezki <- at_ciezki |>
  gt() |>
  cols_label(who = "", mins = "minutes", poss = "possessions") |>
  fmt_number(columns = c(mins, poss), decimals = 0, use_seps = TRUE) |>
  cols_align(align = "right", columns = c(mins, poss)) |>
  cols_width(who ~ px(130), mins ~ px(120), poss ~ px(150)) |>
  tab_header(title = "time on court in 2026") |>
  tab_style(style = list(cell_fill(color = HI), cell_text(color = "white", weight = "bold")),
            locations = cells_body(rows = who == "together")) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  tab_options(table.font.size = 18, data_row.padding = 6,
              table.border.top.style = "none", heading.border.bottom.style = "none")

# ---------------------------------------------------------------------------
# every basket shay ciezki made in 2026, and who set it up.
#
# this is the "why" behind the one miss. SIX of her ten makes were unassisted,
# so only four were ever available to be credited to anyone, and those four went
# to four different teammates. she creates her own looks (pullups and drives) in
# the few minutes she plays. her hoop stats shot level data, phoenix 2026 regular season,
# verified 9/16. rows are in date order, not in game sequence within a date.
ciezki_makes <- data.frame(
  date    = c("jun 22","aug 27","aug 27","aug 27","aug 29",
              "aug 29","aug 29","aug 29","aug 29","aug 29"),
  shot    = c("three","two","two","three","three",
              "two","two","two","two","two"),
  set_up  = c("nobody","nobody","Westbeld","Mack","Held",
              "nobody","nobody","nobody","nobody","Brochant"),
  type    = c("pullup","driving","—","pullup","—",
              "floating","driving floating","pullup","driving","running")
)

t_ciezki_makes <- ciezki_makes |>
  gt() |>
  cols_label(date = "", shot = "shot", set_up = "set up by", type = "shot type") |>
  tab_header(title = "every basket shay ciezki made this season",
             subtitle = "six of the ten were unassisted. the other four came from four different teammates, none of them thomas") |>
  tab_style(style = cell_text(color = GRAY),
            locations = cells_body(columns = set_up, rows = set_up == "nobody")) |>
  tab_style(style = list(cell_text(color = HI, weight = "bold")),
            locations = cells_body(columns = set_up, rows = set_up != "nobody")) |>
  tab_style(style = cell_text(color = GRAY), locations = cells_body(columns = type)) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  tab_source_note(CREDIT) |>
  tab_options(table.font.size = 15, data_row.padding = 4,
              table.border.top.style = "none", heading.border.bottom.style = "none")

# ---------------------------------------------------------------------------
# every player who appeared for phoenix in 2026: games and minutes FOR PHOENIX
# only (bonner's atlanta game and akoa makani's LA games are excluded).
# minutes are mp plus leftover sec / 60. the ten players under 16 games are
# in orange. hhs box scores, verified 9/17.
phx_roster <- data.frame(
  player = c("Noemie Brochant","Alyssa Thomas","Kahleah Copper","DeWanna Bonner",
             "Valeriane Ayayi","Natasha Mack","Lexi Held","Monique Akoa Makani",
             "Kyara Linskens","Sami Whitcomb","Jovana Nogic","Marta Suarez",
             "Quionche Carter","Kiana Williams","Kelsey Plum","Shay Ciezki",
             "Maddy Westbeld","Anneli Maley","Kara Dunn","Saylor Poffenbarger",
             "Peyton Williams"),
  games  = c(39,38,38,37,33,32,30,19,19,17,16,15,13,8,5,4,3,2,1,1,1),
  mins   = c(895.4,1273.5,1251.5,1020.6,543.3,711.3,511.8,444.5,202.2,280.8,
             322.5,135.7,80.6,104.7,105.4,49.0,47.9,6.1,18.7,14.1,5.8)
)

t_phx_roster <- phx_roster |>
  gt() |>
  cols_label(player = "", games = "games", mins = "minutes") |>
  fmt_number(columns = mins, decimals = 0, use_seps = TRUE) |>
  cols_align(align = "right", columns = c(games, mins)) |>
  tab_header(title = "everyone who played for the mercury this season",
             subtitle = "21 players. the ten in orange played fewer than 16 of phoenix's 40 games") |>
  tab_style(style = cell_text(color = HI, weight = "bold"),
            locations = cells_body(rows = games < 16)) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  tab_source_note(CREDIT) |>
  tab_options(table.font.size = 14, data_row.padding = 2,
              table.border.top.style = "none", heading.border.bottom.style = "none")

# the two ends of the roster, as plain tibbles (print them, no gt)
# ten players with fewer than 16 games, and the seven with 30 or more
phx_under_16 <- phx_roster |>
  filter(games < 16) 

phx_30_plus <- phx_roster |>
  filter(games >= 30)

# charts, in order
p_players_used       # 1. 21 players, phoenix vs the league
p_phx_games          # 2. how long each one actually stayed. ten under sixteen games
p_creator_share      # 3. THE PAYOFF. share of her team's baskets she set up, 27.4% vs 23.0
p_at_threes          # 4. her whole career from three. 29 attempts, 2 makes
p_at_shotchart       # 5. "this is every basket she set up"
p_phx_scorers        # 6. 16 of the 17 other players who scored
p_most_players_hist  # 7. tied with the 2025 wings, most since 2010
t_starting_fives     # 8. "not the most starting fives though." phoenix 3rd, toronto 22
t_at_ciezki          # 9. the one teammate she never set up. 17.2 min, 37 poss together
t_ciezki_makes       # 10. all ten ciezki makes. six unassisted, which is the real explanation
t_phx_roster         # 11. all 21 players, games and minutes for phoenix
#
# built but not used in the final video
p_share         # share of team ASSISTS (39.8%). demoted to caption only, do NOT show
                # this while saying 27%, the numbers will not match
p_duo           # usage, thomas vs copper. the "fewer shots" beat was cut
p_phx_shooters  # 3pt% against games played (retired inversion angle)
p_top2_gap      # every team's top two vs the rest (retired inversion angle)
p_apg           # assists per game, mercury players
p_usg           # usage against assists
#
# tibbles
# phx_under_16    the ten players with fewer than 16 games for phoenix
# phx_30_plus     the seven players with 30 or more games for phoenix
# phx_roster      all 21 players, games and minutes for phoenix
# creator_share   every player with 100+ assists, share of team baskets they set up
# phx_games       all 21 mercury players and games played
# phx_scorers     phoenix scorers, split by whether thomas set them up
# at_ast          all 320 assisted shots, folded to one half court
# shooters        every player with 10+ three point attempts (retired angle)
# phx_shooters    phoenix only, with the top two flagged (retired angle)
# top2_gap        every team's top two vs rest three point percentage (retired angle)
