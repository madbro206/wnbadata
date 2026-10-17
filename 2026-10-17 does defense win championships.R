# charts for the "does defense actually win championships?" video (posted 10/17, the day the finals start)
#
# all numbers are hardcoded from her hoop stats, regular season only.
#   - 2026 team numbers were cross-checked against wehoop on 10/15. the ranks match
#     (golden state 1st / atlanta 11th in opp eFG, atlanta 1st / golden state 2nd in
#     forced turnovers per 100). wehoop's raw values are within 0.1 of these.
#   - champion history (1997-2025) is from her hoop stats, since wehoop's team box scores
#     start in 2024. the champion list is typed in by hand.
#   - 2024 finals scores (lowest scoring chart) come from wehoop.
#
# axis rule: anything with a length starts at zero. the dot plots and the scatter
# have no length, so their axes are zoomed in.

library(dplyr)
library(ggplot2)
library(forcats)

GSV    <- "#7048B6"   # golden state
ATL    <- "#C8102E"   # atlanta
GRAY   <- "#8A8A86"   # everyone else
INK    <- "#15171D"
CREDIT <- "data: her hoop stats  |  chart: @wnbadata"
CREDIT_2026 <- "2026 regular season  ·  data: her hoop stats  |  chart: @wnbadata"

team_colors <- c(`Golden State` = GSV, Atlanta = ATL, other = GRAY)

base_theme <- function() {
  theme_minimal(base_size = 15) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_text(face = "bold", color = INK, size = 19),
      plot.subtitle = element_text(color = GRAY, size = 13, margin = margin(b = 14)),
      plot.caption  = element_text(color = GRAY, size = 11, hjust = 0, margin = margin(t = 14)),
      axis.text     = element_text(color = INK, size = 13),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#E3E3E0", linewidth = 0.3),
      plot.margin   = margin(18, 18, 14, 10)
    )
}


# 2026 TEAM DEFENSE
# opp_efg: opponent effective field goal %, lower is better ("making you miss")
# forced_tov: turnovers forced per 100 opponent possessions ("taking the ball")
teams_2026 <- tibble::tribble(
  ~team,          ~opp_efg, ~forced_tov,
  "Golden State",  47.8,     18.57,
  "Washington",    49.2,     15.6,
  "Minnesota",     50.2,     18.56,  # kept to 2 decimals so golden state sorts 2nd, both print as 18.6
  "Seattle",       50.3,     16.7,
  "Dallas",        50.6,     15.4,
  "Las Vegas",     50.7,     16.5,
  "Chicago",       50.7,     16.0,
  "New York",      51.2,     14.3,
  "Indiana",       51.6,     15.8,
  "Connecticut",   52.6,     17.4,
  "Atlanta",       53.1,     19.2,
  "Portland",      53.3,     17.5,
  "Phoenix",       53.5,     16.7,
  "Los Angeles",   53.5,     17.1,
  "Toronto",       54.7,     15.4
) |>
  mutate(hl = if_else(team %in% c("Golden State", "Atlanta"), team, "other"),
         is_hl = hl != "other")

stopifnot(nrow(teams_2026) == 15)


# 1. GOLDEN STATE MAKES YOU MISS (script: "teams shoot worse against them than against anyone else")
p_opp_efg <- ggplot(teams_2026, aes(x = opp_efg, y = reorder(team, -opp_efg))) +
  geom_point(aes(color = hl, size = is_hl)) +
  geom_text(aes(label = sprintf("%.1f%%", opp_efg), color = hl),
            hjust = -0.45, size = 4.2, show.legend = FALSE) +
  scale_color_manual(values = team_colors, guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
  scale_x_continuous(limits = c(47, 56), labels = function(x) paste0(x, "%")) +
  labs(title = "opponent effective field goal %",
       subtitle = "2026 regular season, lowest at the top",
       caption = CREDIT_2026, x = NULL, y = NULL) +
  base_theme()


# 2. ATLANTA TAKES THE BALL (script: "they force more turnovers than anyone")
# cross-checked 10/17: wehoop gives the same values (atl 19.14, gs 18.57, min 18.56). basketball reference's
# defensive TOV% (turnovers per 100 plays) has the same top 3 in the same order: atl 17.2, gs 16.1, min 15.9
# smaller text and dots so golden state's label doesn't sit on minnesota's in a short plot pane
p_forced_tov <- ggplot(teams_2026, aes(x = forced_tov, y = reorder(team, forced_tov))) +
  geom_point(aes(color = hl, size = is_hl)) +
  geom_text(aes(label = sprintf("%.1f", forced_tov), color = hl),
            hjust = -0.5, size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = team_colors, guide = "none") +
  scale_size_manual(values = c(`FALSE` = 2.6, `TRUE` = 4.2), guide = "none") +
  scale_x_continuous(limits = c(14, 20)) +
  labs(title = "turnovers forced per 100 possessions",
       subtitle = "2026 regular season",
       caption = CREDIT_2026, x = NULL, y = NULL) +
  base_theme()


# 3. BOTH AT ONCE (script: "so they kind of do both")
# x is flipped so better defense is to the right on both axes. dashed lines are league averages
avg_efg <- mean(teams_2026$opp_efg)
avg_tov <- mean(teams_2026$forced_tov)

p_both <- ggplot(teams_2026, aes(x = opp_efg, y = forced_tov)) +
  geom_vline(xintercept = avg_efg, color = GRAY, linewidth = 0.4, linetype = "dashed") +
  geom_hline(yintercept = avg_tov, color = GRAY, linewidth = 0.4, linetype = "dashed") +
  geom_point(data = filter(teams_2026, !is_hl), color = GRAY, size = 3.4) +
  geom_point(data = filter(teams_2026, is_hl), aes(color = hl), size = 5.4) +
  geom_text(data = filter(teams_2026, is_hl), aes(label = team, color = hl),
            vjust = -1.2, size = 4.8, fontface = "bold") +
  annotate("text", x = 47.2, y = 14.2, label = "better at making you miss  →",
           hjust = 1, color = GRAY, size = 3.9) +
  annotate("text", x = 55.2, y = 19.9, label = "↑ better at taking the ball",
           hjust = 0, color = GRAY, size = 3.9) +
  scale_color_manual(values = team_colors, guide = "none") +
  scale_x_reverse(limits = c(55.3, 47), labels = function(x) paste0(x, "%")) +
  scale_y_continuous(limits = c(14, 20)) +
  labs(title = "opponent eFG % vs turnovers forced",
       subtitle = "2026 regular season. dashed lines are league averages",
       caption = CREDIT_2026, x = "opponent eFG %", y = "turnovers forced per 100") +
  base_theme() +
  theme(panel.grid.major.y = element_line(color = "#E3E3E0", linewidth = 0.3),
        axis.title = element_text(color = GRAY, size = 12))


# CHAMPION HISTORY, 29 champions (1997-2025)
# efg_rk: rank in opponent eFG that regular season (1 = best). tov_rk: rank in turnovers
# forced per 100. teams: league size that season. houston, sacramento and detroit are
# listed under the names they had at the time
champions <- tibble::tribble(
  ~year, ~champion,      ~efg_rk, ~tov_rk, ~teams,
  1997,  "Houston",       5,       3,       8,
  1998,  "Houston",       1,       2,       10,
  1999,  "Houston",       1,       6,       12,
  2000,  "Houston",       2,       12,      16,
  2001,  "Los Angeles",   8,       15,      16,
  2002,  "Los Angeles",   2,       15,      16,
  2003,  "Detroit",       2,       10,      14,
  2004,  "Seattle",       8,       3,       13,
  2005,  "Sacramento",    4,       1,       13,
  2006,  "Detroit",       1,       8,       14,
  2007,  "Phoenix",       7,       8,       13,
  2008,  "Detroit",       6,       3,       14,
  2009,  "Phoenix",       6,       13,      13,
  2010,  "Seattle",       1,       8,       12,
  2011,  "Minnesota",     4,       6,       12,
  2012,  "Indiana",       5,       2,       12,
  2013,  "Minnesota",     3,       7,       12,
  2014,  "Phoenix",       1,       9,       12,
  2015,  "Minnesota",     5,       9,       12,
  2016,  "Los Angeles",   4,       4,       12,
  2017,  "Minnesota",     2,       2,       12,
  2018,  "Seattle",       3,       6,       12,
  2019,  "Washington",    8,       5,       12,
  2020,  "Seattle",       1,       2,       12,
  2021,  "Chicago",       6,       4,       12,
  2022,  "Las Vegas",     8,       10,      12,
  2023,  "Las Vegas",     2,       7,       12,
  2024,  "New York",      2,       5,       12,
  2025,  "Las Vegas",     6,       7,       13
)

stopifnot(nrow(champions) == 29,
          sum(champions$efg_rk == 1) == 6,
          sum(champions$tov_rk == 1) == 1,
          max(champions$efg_rk) == 8)


# HISTORY: EACH CHAMPION'S OFFENSIVE AND DEFENSIVE RATING RANK THAT REGULAR SEASON (her hoop stats)
# rank 1 = best. off_rk is points scored per 100 possessions, def_rk is points allowed per 100.
# these only go on charts without the team colors, because OFF blue is too close to golden state purple
OFF <- "#2B7BB9"   # offense
DEF <- "#C77A1F"   # defense

champ_ratings <- tibble::tribble(
  ~year, ~champion,      ~off_rk, ~def_rk,
  1997,  "Houston",       1,       3,
  1998,  "Houston",       1,       1,
  1999,  "Houston",       1,       1,
  2000,  "Houston",       1,       1,
  2001,  "Los Angeles",   1,       5,
  2002,  "Los Angeles",   2,       2,
  2003,  "Detroit",       4,       2,
  2004,  "Seattle",       1,       3,
  2005,  "Sacramento",    5,       1,
  2006,  "Detroit",       9,       3,
  2007,  "Phoenix",       1,       11,
  2008,  "Detroit",       4,       5,
  2009,  "Phoenix",       1,       13,
  2010,  "Seattle",       2,       3,
  2011,  "Minnesota",     2,       2,
  2012,  "Indiana",       4,       3,
  2013,  "Minnesota",     1,       4,
  2014,  "Phoenix",       1,       1,
  2015,  "Minnesota",     4,       4,
  2016,  "Los Angeles",   2,       2,
  2017,  "Minnesota",     1,       1,
  2018,  "Seattle",       1,       2,
  2019,  "Washington",    1,       6,
  2020,  "Seattle",       2,       1,
  2021,  "Chicago",       6,       6,
  2022,  "Las Vegas",     1,       7,
  2023,  "Las Vegas",     1,       1,
  2024,  "New York",      2,       3,
  2025,  "Las Vegas",     5,       8
)

# every number the script says about the history
stopifnot(nrow(champ_ratings) == 29,
          sum(champ_ratings$off_rk == 1) == 15,
          sum(champ_ratings$def_rk == 1) == 8,
          sum(champ_ratings$off_rk <= 3 & champ_ratings$def_rk <= 3) == 15,
          sum(champ_ratings$off_rk < champ_ratings$def_rk) == 13,
          sum(champ_ratings$def_rk < champ_ratings$off_rk) == 5,
          sum(champ_ratings$def_rk >= 6 & champ_ratings$year >= 2021) == 3,
          sum(champ_ratings$def_rk >= 6 & champ_ratings$year <= 2020) == 3)

finalists_ratings <- tibble::tribble(
  ~team,          ~off_rk, ~def_rk,
  "Golden State",  7,       1,
  "Atlanta",       6,       4
)


# H1. BEST OFFENSE 15, BEST DEFENSE 8 (script: "the team with the best offense in the regular season won 15 of them / the best defense won 8")
best_won <- tibble::tibble(
  what = factor(c("best offense", "best defense"), levels = c("best offense", "best defense")),
  n    = c(sum(champ_ratings$off_rk == 1), sum(champ_ratings$def_rk == 1))
)

p_best_won <- ggplot(best_won, aes(x = what, y = n, fill = what)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = n), vjust = -0.5, color = INK, size = 7, fontface = "bold") +
  scale_fill_manual(values = c(`best offense` = OFF, `best defense` = DEF), guide = "none") +
  scale_y_continuous(limits = c(0, 17), expand = expansion(mult = c(0, 0))) +
  labs(title = "titles won by the #1 offense and #1 defense",
       subtitle = "regular season offensive and defensive rating, 1997-2025",
       caption = paste0("29 champions, 1997-2025  ·  ", CREDIT), x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#E3E3E0", linewidth = 0.3),
        axis.text.y = element_blank())


# H2. THE RANK MAP (script: "15 of the 29 were top 3 on offense and on defense /
# 13 champions ranked higher on offense than defense. only 5 were the other way around")
# one dot per offense/defense rank combo, sized by how many champions landed there.
# defense is flipped so 1st is at the top: top left = good at both. below the diagonal = better on offense
# one dot per champion. champions with the same offense and defense rank are packed into a little
# grid around that spot (up to 3 across) instead of one big sized bubble, so you can count them
champ_dots <- champ_ratings |>
  group_by(off_rk, def_rk) |>
  mutate(i = row_number() - 1, n = n(),
         cols = pmin(n, 3), rows = ceiling(n / 3),
         dx = (i %% 3 - (cols - 1) / 2) * 0.28,
         dy = (i %/% 3 - (rows - 1) / 2) * 0.42) |>
  ungroup() |>
  mutate(x = off_rk + dx, y = def_rk + dy)

# label placement per outlier so nothing crosses the diagonal or runs off the right edge
outliers   <- filter(champ_ratings, year %in% c(2006, 2007, 2009, 2025)) |>
  mutate(label = paste(year, tolower(champion)),
         lab_hjust = case_when(year == 2025 ~ 1.2, year == 2006 ~ 0.5, TRUE ~ -0.22),
         lab_vjust = if_else(year == 2006, -1.5, 0.5))

# shaded halves: above/right of the diagonal = ranked better on defense, below/left = better on offense
OFF_TINT <- "#E3EDF6"
DEF_TINT <- "#F8EBDD"

# no coord_equal: it forces a square and wastes half of a wide plot pane. the diagonal
# still means "same rank on both ends" at any aspect ratio, it just isn't drawn at 45 degrees
rank_map <- function(show_finalists = FALSE) {
  p <- ggplot(champ_dots, aes(x = x, y = y)) +
    annotate("polygon", x = c(0.5, 10.5, 10.5), y = c(0.5, 0.5, 10.5), fill = DEF_TINT) +
    annotate("polygon", x = c(0.5, 0.5, 10.5, 10.5), y = c(0.5, 13.6, 13.6, 10.5), fill = OFF_TINT) +
    # a segment, not geom_abline: abline is drawn after the y flip and ends up off the chart
    annotate("segment", x = 0.5, y = 0.5, xend = 10.5, yend = 10.5,
             color = GRAY, linewidth = 0.4, linetype = "dashed") +
    annotate("rect", xmin = 0.5, xmax = 3.5, ymin = 0.5, ymax = 3.5,
             fill = NA, color = INK, linewidth = 0.5) +
    annotate("text", x = 0.55, y = -0.3, label = "top 3 in both: 15", hjust = 0,
             color = INK, size = 4.2, fontface = "bold") +
    annotate("text", x = 4.6, y = 12.3, label = "better on offense: 13", hjust = 0,
             color = OFF, size = 4.4, fontface = "bold") +
    annotate("text", x = 10.4, y = 5.2, label = "better on defense: 5", hjust = 1,
             color = DEF, size = 4.4, fontface = "bold") +
    geom_point(color = INK, size = 3) +
    geom_text(data = outliers, aes(x = off_rk, y = def_rk, label = label,
                                   hjust = lab_hjust, vjust = lab_vjust),
              color = INK, size = 3.6) +
    scale_x_continuous(limits = c(0.5, 10.5), breaks = 1:10, expand = expansion(0)) +
    scale_y_reverse(limits = c(13.6, -0.8), breaks = 1:13, expand = expansion(0)) +
    labs(title = "offense and defense rank of every champion",
         subtitle = "regular season rating rank, one dot per champion, 1997-2025",
         caption = paste0("29 champions, 1997-2025  ·  ", CREDIT),
         x = "offense rank", y = "defense rank") +
    base_theme() +
    theme(panel.grid.major = element_blank(), panel.grid.major.x = element_blank(),
          axis.title = element_text(color = GRAY, size = 12))

  if (show_finalists) {
    p <- p +
      geom_point(data = finalists_ratings, aes(x = off_rk, y = def_rk, color = team), size = 5.5) +
      geom_text(data = finalists_ratings, aes(x = off_rk, y = def_rk, label = tolower(team), color = team),
                hjust = -0.3, size = 4.2, fontface = "bold") +
      scale_color_manual(values = team_colors, guide = "none") +
      labs(title = "offense and defense rank, 2026 finalists",
           subtitle = "regular season rating rank, past champions in black",
           caption = paste0("29 champions, 1997-2025, plus the 2026 finalists  ·  ", CREDIT))
  }
  p
}

# BACKUP FOR THE SAME BEATS, simpler to read: what kind of team each champion was.
# good at both = top 3 on offense and defense. the other 14 split by which rank was better
champ_types <- champ_ratings |>
  mutate(type = case_when(off_rk <= 3 & def_rk <= 3 ~ "top 3 on both",
                          off_rk < def_rk           ~ "better on offense",
                          def_rk < off_rk           ~ "better on defense",
                          TRUE                      ~ "same rank on both")) |>
  count(type) |>
  mutate(type = factor(type, levels = c("top 3 on both", "better on offense",
                                        "better on defense", "same rank on both")))

stopifnot(champ_types$n[champ_types$type == "top 3 on both"] == 15,
          champ_types$n[champ_types$type == "better on offense"] == 8,
          champ_types$n[champ_types$type == "better on defense"] == 4,
          champ_types$n[champ_types$type == "same rank on both"] == 2)

p_champ_types <- ggplot(champ_types, aes(x = n, y = fct_rev(type), fill = type)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = n), hjust = -0.4, color = INK, size = 5.5, fontface = "bold") +
  scale_fill_manual(values = c(`top 3 on both` = INK, `better on offense` = OFF,
                               `better on defense` = DEF, `same rank on both` = GRAY), guide = "none") +
  scale_x_continuous(limits = c(0, 17), expand = expansion(mult = c(0, 0))) +
  labs(title = "what kind of team won each title",
       subtitle = "29 champions by regular season offense and defense rank, 1997-2025",
       caption = paste0("29 champions, 1997-2025  ·  ", CREDIT), x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(), axis.text.x = element_blank())

p_ranks_map      <- rank_map()
# H4. same map with the 2026 finalists on it (script: "both teams are better on defense than offense /
# golden state has the best defense and the 7th best offense / the only champion that lopsided was the 2006 detroit shock")
p_ranks_map_2026 <- rank_map(show_finalists = TRUE)


# H5. POINTS VS LEAGUE AVERAGE (script: "golden state allows about 9 fewer points per 100 possessions than an
# average team, and their offense is right at league average / no champion has ever leaned that hard on defense /
# atlanta is about 3 points better than average on both ends")
# off_vs_lg = offensive rating minus that season's league average. def_vs_lg = league average minus defensive
# rating, so positive is better on both. her hoop stats, regular season, points per 100 possessions
lean_points <- tibble::tribble(
  ~year, ~team,          ~off_vs_lg, ~def_vs_lg,
  1997,  "Houston",       6.8,        2.7,
  1998,  "Houston",       10.6,       6.7,
  1999,  "Houston",       7.8,        5.4,
  2000,  "Houston",       10.8,       6.6,
  2001,  "Los Angeles",   10.3,       1.3,
  2002,  "Los Angeles",   4.8,        3.5,
  2003,  "Detroit",       2.9,        3.7,
  2004,  "Seattle",       4.9,        2.1,
  2005,  "Sacramento",    1.5,        8.7,
  2006,  "Detroit",       -1.3,       5.2,
  2007,  "Phoenix",       6.8,        -2.3,
  2008,  "Detroit",       3.0,        2.4,
  2009,  "Phoenix",       8.1,        -4.6,
  2010,  "Seattle",       5.5,        4.7,
  2011,  "Minnesota",     6.8,        3.6,
  2012,  "Indiana",       4.3,        3.5,
  2013,  "Minnesota",     9.9,        3.1,
  2014,  "Phoenix",       8.8,        5.1,
  2015,  "Minnesota",     2.0,        3.0,
  2016,  "Los Angeles",   4.4,        4.6,
  2017,  "Minnesota",     6.5,        8.1,
  2018,  "Seattle",       5.7,        3.7,
  2019,  "Washington",    14.4,       0.5,
  2020,  "Seattle",       5.6,        8.7,
  2021,  "Chicago",       0.9,        1.5,
  2022,  "Las Vegas",     8.3,        -1.4,
  2023,  "Las Vegas",     11.4,       4.3,
  2024,  "New York",      5.5,        5.4,
  2025,  "Las Vegas",     2.8,        1.1
)

finalists_points <- tibble::tribble(
  ~team,          ~off_vs_lg, ~def_vs_lg,
  "Golden State",  -0.1,       8.9,
  "Atlanta",       2.8,        3.3
)

# "no champion has ever leaned that hard on defense": golden state's gap is bigger than every champion's,
# and 2006 detroit is the only champion with a below-average offense
stopifnot(nrow(lean_points) == 29,
          max(lean_points$def_vs_lg - lean_points$off_vs_lg) < 8.9 - (-0.1),
          sum(lean_points$off_vs_lg < 0) == 1)

# drawn in "points allowed vs league average" so the signs read naturally: golden state is at 9 fewer
# allowed (-8.9). the y axis is flipped so better defense is still at the top, like the rank map.
# the diagonal is "same amount better on both ends": points above it are better on defense than offense
lean_plot   <- mutate(lean_points, allowed_vs_lg = -def_vs_lg)
fin_plot    <- mutate(finalists_points, allowed_vs_lg = -def_vs_lg)

# only the champions the script and caption name
lean_labels <- filter(lean_plot, year %in% c(2005, 2006)) |>
  mutate(label = paste(year, tolower(team)),
         lab_hjust = if_else(year == 2005, 0.2, -0.15),
         lab_vjust = if_else(year == 2005, -1.2, 0.5))

# fmt "%g" for axis ticks ("5 more"), "%.1f" for the tables ("3.0 fewer")
more_fewer <- function(x, word_up, word_down, fmt = "%g") {
  ifelse(x == 0, "average",
         ifelse(x > 0, paste(sprintf(fmt, x), word_up), paste(sprintf(fmt, abs(x)), word_down)))
}

p_lean_points <- ggplot(lean_plot, aes(x = off_vs_lg, y = allowed_vs_lg)) +
  # tinted halves, split by the diagonal allowed = -scored
  annotate("polygon", x = c(-2.5, -2.5, 11.5), y = c(2.5, -11.5, -11.5), fill = DEF_TINT) +
  annotate("polygon", x = c(-2.5, -2.5, 15.5, 15.5, 11.5), y = c(2.5, 5.5, 5.5, -11.5, -11.5), fill = OFF_TINT) +
  annotate("segment", x = -2.5, y = 2.5, xend = 11.5, yend = -11.5,
           color = GRAY, linewidth = 0.4, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "#BDB9B0", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "#BDB9B0", linewidth = 0.4) +
  annotate("text", x = 5.8, y = -10.6, label = "better on defense than offense",
           color = DEF, size = 4.4, fontface = "bold") +
  annotate("text", x = 15.2, y = 2.9, label = "better on offense than defense", hjust = 1,
           color = OFF, size = 4.4, fontface = "bold") +
  geom_point(color = INK, size = 3) +
  geom_text(data = lean_labels, aes(label = label, hjust = lab_hjust, vjust = lab_vjust),
            color = INK, size = 3.6) +
  geom_point(data = fin_plot, aes(color = team), size = 5.5) +
  # golden state labeled below its dot, atlanta out to the left with a leader line (its neighbors are crowded)
  annotate("text", x = -0.1, y = -8.9, label = "golden state", vjust = 2, hjust = 0.65, color = GSV, size = 4.4, fontface = "bold") +
  annotate("segment", x = 2.5, y = -3.3, xend = 0.4, yend = -3.3, color = ATL, linewidth = 0.5) +
  annotate("text", x = 0.3, y = -3.3, label = "atlanta", hjust = 1, color = ATL, size = 4.4, fontface = "bold") +
  scale_color_manual(values = team_colors, guide = "none") +
  scale_x_continuous(limits = c(-2.5, 15.5), breaks = c(0, 5, 10, 15), expand = expansion(0),
                     labels = function(x) more_fewer(x, "more", "fewer")) +
  scale_y_reverse(limits = c(5.5, -11.5), breaks = c(5, 0, -5, -10), expand = expansion(0),
                  labels = function(x) more_fewer(x, "more", "fewer")) +
  labs(title = "points per 100 possessions vs league average",
       subtitle = "regular season. past champions in black, 2026 finalists in color",
       caption = paste0("29 champions, 1997-2025, plus the 2026 finalists  ·  ", CREDIT),
       x = "points scored", y = "points allowed") +
  base_theme() +
  theme(panel.grid.major = element_blank(), panel.grid.major.x = element_blank(),
        axis.title = element_text(color = GRAY, size = 12))


# H3. DEFENSE RANK BY YEAR (script: "3 of the last 5 champions ranked 6th or worse on defense /
# before that, it happened 3 times in 24 years")
# 2019 and 2021 sit next to each other at 6th, so 2019 is labeled on its left. inside the
# last-5 box the year is already on the axis, so those labels are just the rank
def_by_year <- champ_ratings |>
  mutate(low = def_rk >= 6,
         lab = if_else(year >= 2021, paste0(def_rk, "th"), paste0(year, ": ", def_rk, "th")),
         lab_hjust = if_else(year == 2019, 1.2, 0.5),
         lab_vjust = if_else(year == 2019, 0.5, 2))

p_def_rank_by_year <- ggplot(def_by_year, aes(x = year, y = def_rk)) +
  annotate("rect", xmin = 2020.5, xmax = 2025.5, ymin = 0.3, ymax = 13.7, fill = "#F1EEE8") +
  annotate("text", x = 2023, y = 0.2, label = "last 5", color = GRAY, size = 4) +
  geom_point(aes(color = low, size = low)) +
  geom_text(data = filter(def_by_year, low),
            aes(label = lab, hjust = lab_hjust, vjust = lab_vjust),
            color = DEF, size = 3.9, fontface = "bold") +
  scale_color_manual(values = c(`FALSE` = INK, `TRUE` = DEF), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.2, `TRUE` = 4.6), guide = "none") +
  scale_y_reverse(limits = c(14.5, 0), breaks = c(1, 3, 6, 9, 13)) +
  scale_x_continuous(limits = c(1996.5, 2025.8), breaks = c(1997, 2005, 2015, 2025)) +
  labs(title = "defensive rating rank of every champion",
       subtitle = "regular season, 1997-2025",
       caption = paste0("29 champions, 1997-2025  ·  ", CREDIT), x = NULL, y = "defense rank") +
  base_theme() +
  theme(panel.grid.major.y = element_line(color = "#E3E3E0", linewidth = 0.3),
        axis.title.y = element_text(color = GRAY, size = 12))


# 4. CHAMPIONS WHO LED EACH DEFENSIVE CATEGORY (script: "only 1 champion has ever led the league in forced
# turnovers, the 2005 sacramento monarchs"). one bar per category this year's finalists led, colored by which
# finalist leads it in 2026. her hoop stats, regular season, 29 champions 1997-2025. steals are per 100 opponent possessions,
# defensive rebounding is the share of opponent misses rebounded
#   defensive rating: 1998, 1999, 2000, 2005, 2014, 2017, 2020, 2023
#   opponent eFG:     1998, 1999, 2006, 2010, 2014, 2020
#   def rebounding:   2001, 2002, 2006, 2008, 2011
#   steals:           1998, 2012, 2020
#   forced TOs:       2005
led <- tibble::tribble(
  ~category,                ~n, ~leader_2026,
  "defensive rating",        8, "Golden State",
  "opponent eFG %",          6, "Golden State",
  "defensive rebounding",    5, "Atlanta",
  "steals",                  3, "Atlanta",
  "forced turnovers",        1, "Atlanta"
) |>
  mutate(category = factor(category, levels = rev(category)))

# the three that live in the champion tibbles above get checked against them
stopifnot(led$n[led$category == "defensive rating"] == sum(champ_ratings$def_rk == 1),
          led$n[led$category == "opponent eFG %"]   == sum(champions$efg_rk == 1),
          led$n[led$category == "forced turnovers"] == sum(champions$tov_rk == 1))

p_champs_led <- ggplot(led, aes(x = n, y = category, fill = leader_2026)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = n, color = leader_2026), hjust = -0.5, size = 5.5, fontface = "bold") +
  annotate("text", x = 1.7, y = 1, label = "2005 sacramento", hjust = 0, color = ATL, size = 4) +
  scale_fill_manual(values = team_colors, labels = c(`Golden State` = "golden state leads it this year",
                                                     Atlanta = "atlanta leads it this year"),
                    breaks = c("Golden State", "Atlanta"), name = NULL) +
  scale_color_manual(values = team_colors, guide = "none") +
  scale_x_continuous(limits = c(0, 9.5), expand = expansion(mult = c(0, 0))) +
  labs(title = "champions who led each defensive category",
       subtitle = "regular season, 1997-2025",
       caption = paste0("29 champions, 1997-2025  ·  ", CREDIT), x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top", legend.justification = "left", legend.location = "plot",
        legend.text = element_text(color = INK, size = 12),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank())


# 5. NOBODY WORSE THAN 8TH (script: "not one champion ranked worse than 8th at making teams miss / atlanta is 11th")
# y is reversed so 1st is at the top. 2026 shows both finalists, which haven't won yet
finalists_2026 <- tibble::tribble(
  ~year, ~team,          ~efg_rk, ~lab_vjust,
  2026,  "Golden State",  1,       -1.1,   # label above the dot
  2026,  "Atlanta",       11,      0.5     # label to the left, clear of the last-place line
)

# league size, drawn as a "last place" step so you can see how many teams each rank was out of.
# only 1997 had 8 teams. every champion that ranked 8th was in a league of 12 to 16
league_size <- bind_rows(select(champions, year, teams), tibble::tibble(year = 2026, teams = 15))
last_place  <- bind_rows(transmute(league_size, x = year - 0.5, teams),
                         tibble::tibble(x = 2026.5, teams = 15))

stopifnot(all(champions$teams[champions$efg_rk == 8] >= 12),
          sum(league_size$teams == 8) == 1)

p_champs_rank <- ggplot(champions, aes(x = year, y = efg_rk)) +
  geom_step(data = last_place, aes(x = x, y = teams), direction = "hv",
            color = "#B8B4AA", linewidth = 0.9, inherit.aes = FALSE) +
  annotate("text", x = 1997, y = 8, label = "8 teams", vjust = -0.7, color = "#8F8B80", size = 3.6) +
  annotate("text", x = 2001, y = 16, label = "16 teams", vjust = 1.7, color = "#8F8B80", size = 3.6) +
  annotate("text", x = 2017, y = 12, label = "last place that season", vjust = 1.7, color = "#8F8B80", size = 3.6) +
  geom_hline(yintercept = 8, color = GRAY, linewidth = 0.5, linetype = "dashed") +
  annotate("text", x = 2006, y = 8, label = "worst champion: 8th",
           hjust = 0, vjust = 1.8, color = GRAY, size = 4.2) +
  geom_point(color = INK, size = 3.4) +
  geom_point(data = finalists_2026, aes(color = team), size = 5.4) +
  geom_text(data = finalists_2026, aes(label = paste0(tolower(team), ": ", efg_rk, c("st", "th")),
                                       color = team, vjust = lab_vjust,
                                       hjust = if_else(team == "Atlanta", 1.12, 0.85)),
            size = 4.4, fontface = "bold") +
  scale_color_manual(values = team_colors, guide = "none") +
  scale_y_reverse(limits = c(17, 0.1), breaks = c(1, 4, 8, 12, 16)) +
  scale_x_continuous(limits = c(1996.5, 2027), breaks = c(1997, 2005, 2015, 2026)) +
  labs(title = "opponent eFG % rank of every champion",
       subtitle = "regular season. the gray steps are last place that season",
       caption = paste0("champions 1997-2025, 2026 finalists  ·  ", CREDIT), x = NULL, y = "rank") +
  base_theme() +
  theme(panel.grid.major.y = element_line(color = "#E3E3E0", linewidth = 0.3),
        axis.title.y = element_text(color = GRAY, size = 12))


# HEAD TO HEAD, the 3 regular season meetings (golden state won all 3)
h2h <- tibble::tribble(
  ~game,                            ~team,          ~tov_forced, ~efg,
  "6/24 at gs\ngs 77, atl 66",      "Atlanta",      17,          42.2,
  "6/24 at gs\ngs 77, atl 66",      "Golden State", 13,          57.5,
  "6/26 at gs\ngs 78, atl 75",      "Atlanta",      18,          38.8,
  "6/26 at gs\ngs 78, atl 75",      "Golden State", 14,          47.6,
  "7/4 at atl\ngs 88, atl 83",      "Atlanta",      15,          45.5,
  "7/4 at atl\ngs 88, atl 83",      "Golden State", 17,          56.0
) |>
  mutate(game = factor(game, levels = unique(game)))

ATL_SEASON_EFG <- 50.5


# 6. ATLANTA WON THE TURNOVER BATTLE 2 OUT OF 3 TIMES (script: "atlanta won the turnover battle in 2 of those games")
p_h2h_tov <- ggplot(h2h, aes(x = game, y = tov_forced, fill = team)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  geom_text(aes(label = tov_forced, color = team), position = position_dodge(width = 0.7),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = team_colors, name = NULL, labels = tolower) +
  scale_color_manual(values = team_colors, guide = "none") +
  scale_y_continuous(limits = c(0, 20), expand = expansion(mult = c(0, 0))) +
  labs(title = "turnovers forced, atlanta vs golden state",
       subtitle = "2026 regular season meetings",
       caption = CREDIT_2026, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top", legend.justification = "left",
        legend.text = element_text(color = INK, size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#E3E3E0", linewidth = 0.3))


# 7. AND STILL LOST ALL 3 (script: "and still lost, because golden state didn't let them make shots")
p_h2h_efg <- ggplot(filter(h2h, team == "Atlanta"), aes(x = game, y = efg)) +
  geom_col(fill = ATL, width = 0.55) +
  geom_hline(yintercept = ATL_SEASON_EFG, color = INK, linewidth = 0.6, linetype = "dashed") +
  annotate("text", x = 0.55, y = ATL_SEASON_EFG, label = "atlanta's season: 50.5%",
           hjust = 0, vjust = -0.7, color = INK, size = 4.2) +
  geom_text(aes(label = sprintf("%.1f%%", efg)), vjust = -0.5, color = INK, size = 5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 58), expand = expansion(mult = c(0, 0)),
                     labels = function(x) paste0(x, "%")) +
  labs(title = "atlanta's eFG % against golden state",
       subtitle = "2026 regular season meetings",
       caption = CREDIT_2026, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#E3E3E0", linewidth = 0.3))


# 8. OPTIONAL SWAP-IN: LOWEST SCORING FINALS
# combined points per game in each finals. 1997 was a single game (116), so it's left off
# and 1999 is the lowest multi-game finals. 2024 is from wehoop
finals_ppg <- tibble::tribble(
  ~year, ~ppg,
  1998, 133.0, 1999, 124.7, 2000, 131.5, 2001, 138.5, 2002, 134.5, 2003, 140.7,
  2004, 132.7, 2005, 130.8, 2006, 147.8, 2007, 180.0, 2008, 137.3, 2009, 186.2,
  2010, 166.0, 2011, 166.0, 2012, 150.0, 2013, 152.3, 2014, 159.7, 2015, 142.8,
  2016, 155.4, 2017, 151.2, 2018, 164.3, 2019, 177.0, 2020, 173.0, 2021, 158.8,
  2022, 154.3, 2023, 165.0, 2024, 156.4, 2025, 176.3
) |>
  mutate(hl = year == 1999)

H2H_PPG <- round(mean(c(77 + 66, 78 + 75, 88 + 83)), 1)   # 155.7

p_lowest <- ggplot(finals_ppg, aes(x = year, y = ppg, fill = hl)) +
  geom_col(width = 0.75) +
  geom_hline(yintercept = H2H_PPG, color = INK, linewidth = 0.6, linetype = "dashed") +
  annotate("text", x = 1997.6, y = H2H_PPG, label = "atl vs gs this season: 155.7",
           hjust = 0, vjust = -0.7, color = INK, size = 4.2) +
  annotate("text", x = 1999, y = 62, label = "124.7", angle = 90,
           color = "white", fontface = "bold", size = 4.4) +
  scale_fill_manual(values = c(`FALSE` = GRAY, `TRUE` = ATL), guide = "none") +
  scale_y_continuous(limits = c(0, 200), expand = expansion(mult = c(0, 0))) +
  scale_x_continuous(breaks = c(1998, 2005, 2015, 2025)) +
  labs(title = "combined points per game in each finals",
       subtitle = "1998-2025, 1997 was a single game",
       caption = paste0("1998-2025, 2024 from wehoop  ·  ", CREDIT), x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#E3E3E0", linewidth = 0.3))


# GOLDEN STATE OFFENSE VS LEAGUE AVERAGE (printable, for checking the "right at league average" line)
# her hoop stats, 2026 regular season. league_avg is the mean of the 15 teams, rank is out of 15 with 1 = best.
# turnovers are per 100 possessions (gs is 2nd per 100, 4th per game because they play slow).
# free throws are made free throws per 100 FG attempts; other free throw rate formulas rank gs a bit higher
gsv_offense <- tibble::tribble(
  ~stat,                                  ~gsv,  ~league_avg, ~gsv_rank, ~better_is,
  "points per 100 possessions",           104.7, 104.8,        7,        "higher",
  "effective FG %",                       49.9,  51.5,         12,       "higher",
  "2-point %",                            48.6,  51.8,         14,       "higher",
  "3-point %",                            34.4,  33.9,         6,        "higher",
  "3-point attempts / FG attempts %",     44.6,  37.2,         1,        "more threes",
  "turnovers per 100 possessions",        14.6,  16.7,         2,        "lower",
  "offensive rebound %",                  26.8,  25.4,         4,        "higher",
  "free throws made per 100 FG attempts", 21.0,  23.8,         13,       "higher",
  "FG attempts per 100 possessions",      86.7,  82.6,         1,        "higher"
) |>
  mutate(vs_avg = round(gsv - league_avg, 1), .after = league_avg)

stopifnot(nrow(gsv_offense) == 9)


# TABLES FOR BEATS WITHOUT A CHART

# "no champion has ever been that much better on defense compared to on offense, than the valks"
# title: "biggest defensive bias: golden state vs every wnba champion"
# (the title says golden state isn't a champion yet, so the team name doesn't need a tag.
#  atlanta's bias is +0.5, so they're not in the top 6)
# all numbers are points per 100 possessions vs league average, same signs as p_lean_points:
#   points_allowed: negative = allowed fewer than average (good). points_scored: positive = scored more (good).
#   defensive_bias = how much better the defense is than the offense = -points_allowed - points_scored
# golden state shows 9.0 here because the inputs are rounded to 1 decimal (unrounded it's 8.9)
defensive_bias_top <- bind_rows(mutate(lean_points, team = paste(year, team)),
                      mutate(finalists_points, team = paste("2026", team))) |>
  mutate(defensive_bias = round(def_vs_lg - off_vs_lg, 1)) |>
  arrange(desc(defensive_bias)) |>
  head(6) |>
  # written out in words like the p_lean_points axes, so "fewer allowed" doesn't print as a scary red -8.9
  transmute(team,
            `points allowed`    = more_fewer(-def_vs_lg, "more", "fewer", fmt = "%.1f"),
            `points scored`     = more_fewer(off_vs_lg, "more", "fewer", fmt = "%.1f"),
            `defense better by` = sprintf("%.1f", defensive_bias))

# "they had the most steals ever as a team this year"
# her hoop stats, regular season team steals, all seasons. it's the total: 2009 indiana averaged more per game in a
# 34-game season, so don't say per game
most_team_steals <- tibble::tribble(
  ~season, ~team,       ~steals, ~games, ~per_game,
  2026,    "Atlanta",    400,     44,     9.1,
  2026,    "Minnesota",  384,     44,     8.7,
  2025,    "Seattle",    373,     44,     8.5,
  2009,    "Indiana",    373,     34,     11.0,
  2024,    "Seattle",    372,     40,     9.3,
  2025,    "Minnesota",  361,     44,     8.2
)

# the ending: "if it comes down to defense, golden state's looks like a champion's /
# if it comes down to offense, atlanta's looks more like one"
# title: "golden state and atlanta vs the typical champion"
# subtitle: "points per 100 possessions vs league average, regular season"
# typical champion = median of the 29 champions. same signs as p_lean_points: points_allowed negative = fewer
# allowed than average (good), points_scored positive = more scored (good), net = scored minus allowed
finalists_vs_champs <- tibble::tibble(
  who            = c("typical champion", "golden state", "atlanta"),
  points_allowed = c(-median(lean_points$def_vs_lg), -finalists_points$def_vs_lg),
  points_scored  = c(median(lean_points$off_vs_lg), finalists_points$off_vs_lg)
) |>
  mutate(net = round(points_scored - points_allowed, 1),
         # the typical champion's net is the median of the 29 nets (9.5), not the sum of the two medians
         net = if_else(who == "typical champion",
                       median(lean_points$def_vs_lg + lean_points$off_vs_lg), net)) |>
  # in words, same as the p_lean_points axes
  transmute(who,
            `points allowed` = more_fewer(points_allowed, "more", "fewer", fmt = "%.1f"),
            `points scored`  = more_fewer(points_scored, "more", "fewer", fmt = "%.1f"),
            `outscores opponents by` = sprintf("%.1f", net))

stopifnot(most_team_steals$steals[1] == 400,
          defensive_bias_top$team[1] == "2026 Golden State")


# IN SCRIPT ORDER (rewritten 10/17 to match the final script)
#   "29 champions... best offense won 15 / best defense won 8"           p_best_won
#   "many champions are just good at both / 15 of the 29 top 3"          p_ranks_map
#   "the ones that weren't... worse on defense, especially lately"       p_ranks_map (13 vs 5) or p_champ_types (8 vs 4), then p_def_rank_by_year
#   "both teams arguably lean toward defense"                            p_lean_points
#   "golden state allows about 9 fewer... offense right at average"      p_lean_points, gsv_offense table
#   "no champion has ever been that much better on defense..."           defensive_bias_top table
#   "the dream... force more turnovers than anyone"                      p_forced_tov
#   "they had the most steals ever as a team this year"                  most_team_steals table
#   "only 1 champion has ever led the league in forced turnovers"        p_champs_led
#   "defense doesn't usually win on its own... if it comes down to..."   finalists_vs_champs table
# extras, not in the script: p_champs_rank and p_opp_efg (the eFG beat was cut 10/17, it's caption only now),
# p_ranks_map_2026, p_both, p_h2h_tov, p_h2h_efg, p_lowest
# tables print as data frames so 104.7 doesn't show as "105."
p_best_won
p_ranks_map
p_champ_types
p_def_rank_by_year
p_lean_points
as.data.frame(gsv_offense)
as.data.frame(defensive_bias_top)
p_forced_tov
as.data.frame(most_team_steals)
p_champs_led
as.data.frame(finalists_vs_champs)
p_champs_rank
p_opp_efg
p_ranks_map_2026
p_both
p_h2h_tov
p_h2h_efg
p_lowest
