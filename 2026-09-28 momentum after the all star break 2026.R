# =========================================
# Does momentum matter going into the WNBA playoffs? (2026)
# =========================================
# the lynx had the best point differential in the wnba this season and the worst of any
# playoff team over their last 10 games, then lost game 1 at home to the liberty by 16.
# this script builds the tables and charts for the video, in the order they appear:
#   1. the lynx: best all season, worst lately        (two tables)
#   2. before vs after the all star break             (chart)
#   3. the lynx schedule got harder                   (chart + table)
#   4. what history says                              (tables + chart)
#
# 2026 data comes from wehoop. the historical playoff numbers (2006 to 2025) are hardcoded
# from Her Hoop Stats, because wehoop's team box only goes back to 2024.
#
# all star break: last game 7/22, back 7/28. "before" = through 7/22, "after" = 7/28 to 9/24.

library(wehoop)
library(dplyr)
library(tidyr)
library(ggplot2)

# ============================
# Data: one row per team per game
# ============================
team_box <- load_wnba_team_box(seasons = 2026)
schedule <- load_wnba_schedule(seasons = 2026)

skip <- schedule$game_id[schedule$notes_headline %in% c("AT&T WNBA All-Star Game",
                                                        "WNBA Commissioner's Cup Championship")]

BREAK <- as.Date("2026-07-25")   # all star game day

team_games <- team_box %>%
  filter(season_type == 2,                          # regular season
         !game_id %in% skip) %>%                    # no all star game or cup final
  mutate(game_date = as.Date(game_date),
         poss = field_goals_attempted - offensive_rebounds + total_turnovers + 0.44 * free_throws_attempted)

# attach the opponent's name and possessions to each row
team_games <- team_games %>%
  left_join(team_games %>% select(game_id, team_id, opp_poss = poss),
            by = c("game_id", "opponent_team_id" = "team_id")) %>%
  transmute(game_id, game_date,
            team = team_display_name, abbr = team_abbreviation,
            opp = opponent_team_display_name,
            pts = team_score, opp_pts = opponent_team_score,
            margin = pts - opp_pts, win = margin > 0,
            poss, opp_poss,
            window = ifelse(game_date < BREAK, "before", "after")) %>%
  arrange(team, game_date)

seeds <- c("Minnesota Lynx", "Golden State Valkyries", "Las Vegas Aces", "Atlanta Dream",
           "Washington Mystics", "Indiana Fever", "Dallas Wings", "New York Liberty")

# ============================
# Chart style
# ============================
LYNX    <- "#236192"   # lynx blue
MYSTICS <- "#C8102E"   # mystics red
GRAY    <- "#8A8A86"
FADED   <- "#D6D2DE"
INK     <- "#010101"
CREDIT  <- "2026 regular season  |  chart: @wnbadata\ndata: wehoop"

base_theme <- function() {
  theme_minimal(base_size = 15) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_text(face = "bold", colour = INK, size = 19),
      plot.subtitle = element_text(colour = GRAY, size = 13, margin = margin(b = 14)),
      plot.caption  = element_text(colour = GRAY, size = 11, hjust = 0, margin = margin(t = 14)),
      axis.text     = element_text(colour = INK, size = 13),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#E3E3E0", linewidth = 0.3),
      plot.margin   = margin(18, 18, 14, 10)
    )
}

highlight_colours <- c(MIN = LYNX, WSH = MYSTICS, playoff = INK, other = FADED)

plus <- function(x) ifelse(x > 0, paste0("+", x), x)

# ============================
# 1. the lynx: best all season, worst lately
# ============================
# "the best point differential in the whole wnba this season"
point_diff_table <- team_games %>%
  group_by(team) %>%
  summarise(record     = paste0(sum(win), "-", sum(!win)),
            point_diff = sum(margin),
            per_game   = round(mean(margin), 1),
            .groups = "drop") %>%
  arrange(desc(point_diff)) %>%
  mutate(rank = row_number(), .before = team)

print(point_diff_table, n = Inf)

# "but over their last 10 games, their point differential has been the worst of any playoff team"
# sorted best to worst, so the lynx end up at the bottom
last_10_table <- team_games %>%
  filter(team %in% seeds) %>%
  group_by(team) %>%
  slice_max(game_date, n = 10) %>%
  summarise(record     = paste0(sum(win), "-", sum(!win)),
            point_diff = sum(margin),
            per_game   = round(mean(margin), 1),
            .groups = "drop") %>%
  arrange(desc(point_diff)) %>%
  mutate(rank = row_number(), .before = team)

print(last_10_table)

# ============================
# 2. before vs after the all star break
# ============================
# net rating = points scored minus points allowed, per 100 possessions
# above the dotted line = better after the break, below = worse
scatter_data <- team_games %>%
  group_by(team, abbr, window) %>%
  summarise(net = 100 * sum(pts) / sum(poss) - 100 * sum(opp_pts) / sum(opp_poss), .groups = "drop") %>%
  pivot_wider(names_from = window, values_from = net) %>%
  mutate(change = after - before,
         group  = case_when(abbr %in% c("MIN", "WSH") ~ abbr,
                            team %in% seeds ~ "playoff",
                            TRUE ~ "other"))

print(scatter_data %>% arrange(desc(change)), n = Inf)

lims <- range(c(scatter_data$before, scatter_data$after)) + c(-2, 2)

ggplot(scatter_data, aes(x = before, y = after, colour = group)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", colour = GRAY, linewidth = 0.8) +
  geom_hline(yintercept = 0, colour = "#E3E3E0", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "#E3E3E0", linewidth = 0.3) +
  geom_point(size = 4) +
  ggrepel::geom_text_repel(aes(label = abbr), size = 4.5, fontface = "bold", point.padding = 0.3,
                           min.segment.length = Inf, seed = 1) +
  annotate("text", x = lims[1] + 1, y = lims[2] - 1, label = "better after the break",
           hjust = 0, colour = GRAY, size = 4.5) +
  annotate("text", x = lims[2] - 1, y = lims[1] + 1, label = "worse after the break",
           hjust = 1, colour = GRAY, size = 4.5) +
  scale_colour_manual(values = highlight_colours, guide = "none") +
  scale_x_continuous(limits = lims, labels = plus) +
  scale_y_continuous(limits = lims, labels = plus) +
  coord_equal() +
  labs(title = "before vs after the all star break",
       subtitle = "net rating (points per 100 possessions, minus points allowed)\nplayoff teams in black",
       caption = CREDIT, x = "before the break", y = "after the break") +
  base_theme() +
  theme(panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3),
        axis.title = element_text(colour = GRAY, size = 12))

# ============================
# 3. the lynx schedule got harder
# ============================
# "in their last 10 games, they played a playoff team 8 times / while the mystics only played two"
# all 15 teams: the lynx played the most, the mystics the fewest
vs_playoff_table <- team_games %>%
  group_by(team) %>%
  slice_max(game_date, n = 10) %>%
  summarise(playoff_team            = first(team %in% seeds),
            vs_playoff_teams        = sum(opp %in% seeds),
            record_vs_playoff       = paste0(sum(win & opp %in% seeds), "-", sum(!win & opp %in% seeds)),
            record_vs_everyone_else = paste0(sum(win & !opp %in% seeds), "-", sum(!win & !opp %in% seeds)),
            .groups = "drop") %>%
  arrange(desc(vs_playoff_teams), desc(playoff_team))

print(vs_playoff_table, n = Inf, width = Inf)

# ============================
# 4. what history says (2006 to 2025, hardcoded from Her Hoop Stats)
# ============================
# every wnba playoff series 2006 to 2025, 140 total, including the single game eliminations
# from 2016 to 2021. for each series:
#   better all season = better average point margin over the full regular season
#   hotter            = better average point margin over the last 10 regular season games

# "most of the time, the team that was better all season was also the hotter team"
history_split <- tribble(
  ~matchup,                                         ~series, ~better_all_season_team_won,
  "same team was better all season and hotter",       97,      72,
  "different teams",                                  43,      28,
  "all series",                                      140,     100
) %>%
  mutate(pct = scales::percent(better_all_season_team_won / series, accuracy = 1))

print(history_split, width = Inf)

# "but when they weren't the same team, the team that was better all season won the series
#  about two thirds of the time" (27 of those 43 were 3+ game series; the better team won 17)
history <- tribble(
  ~winner,             ~series,
  "better all season",  28,
  "hotter at the end",  15
) %>%
  mutate(pct    = series / sum(series),
         winner = factor(winner, levels = rev(winner)))   # better all season on the left

print(history)

# one bar split in two: about two thirds vs one third
# colors go by position (first level gray, second black), so renaming the labels won't break them
ggplot(history, aes(x = series, y = "", fill = winner)) +
  geom_col(width = 1, colour = "white", linewidth = 1.5) +
  geom_text(aes(label = sprintf("%s\n%d", winner, series)), position = position_stack(vjust = 0.5),
            colour = "white", size = 6, fontface = "bold", lineheight = 0.9) +
  scale_fill_manual(values = c(GRAY, INK), guide = "none") +
  scale_x_continuous(expand = expansion(mult = 0)) +
  scale_y_discrete(expand = expansion(add = 0)) +
  labs(title = "the better team usually still wins",
       subtitle = "43 playoff series since 2006 where the better team wasn't the hotter team",
       caption = "chart: @wnbadata\ndata: her hoop stats", x = NULL, y = NULL) +
  base_theme() +
  theme(axis.text = element_blank(), panel.grid.major.x = element_blank(),
        aspect.ratio = 0.3)   # short and wide, so the title and caption hug the bar

# "teams with the better regular season record that lost game 1 at home in a best of 3
#  have come back 3 out of 9 times"
# every such series 1997 to 2025 (the first one is 2010). the 2026 row is game 1 on 9/27, from
# ESPN via wehoop. the 2022 sky had game 2 at home; the 2026 lynx play game 2 in new york
comebacks <- tribble(
  ~year, ~team,     ~rec,    ~opp,      ~opp_rec, ~g1_margin, ~series_w, ~series_l,
  2010,  "mystics", "22-12", "dream",   "19-15",  -5,         0,         2,
  2010,  "liberty", "22-12", "dream",   "19-15",  -6,         0,         2,
  2011,  "sun",     "21-13", "dream",   "20-14",  -5,         0,         2,
  2012,  "fever",   "22-12", "dream",   "19-15",  -9,         2,         1,
  2013,  "sky",     "24-10", "fever",   "16-18",  -13,        0,         2,
  2013,  "sparks",  "24-10", "mercury", "19-15",  -11,        1,         2,
  2014,  "dream",   "19-15", "sky",     "15-19",  -3,         1,         2,
  2015,  "liberty", "23-11", "mystics", "18-16",  -3,         2,         1,
  2022,  "sky",     "26-10", "liberty", "16-20",  -7,         2,         1,
  2026,  "lynx",    "33-11", "liberty", "26-18",  -16,        NA,        NA
) %>%
  transmute(year,
            team           = sprintf("%s (%s)", team, rec),
            opponent       = sprintf("%s (%s)", opp, opp_rec),
            lost_game_1_by = -g1_margin,
            series         = case_when(is.na(series_w) ~ "game 2 is in new york",
                                       series_w == 2   ~ sprintf("came back, won %d-%d", series_w, series_l),
                                       TRUE            ~ sprintf("lost %d-%d", series_w, series_l)))

print(comebacks, n = Inf, width = Inf)
