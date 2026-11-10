# charts for the "does rebounding win championships?" video (posted 11/4/26)
#
# the 2026 finals game by game pulls from ESPN via wehoop, so that chart runs from scratch.
# the rest is season-level totals typed in from her hoop stats, because the
# public box scores in wehoop start in 2003 and are missing games in several seasons after that.
#   - "best rebounding" is the share of all rebounds in a team's games that it got:
#     rebounds / (rebounds + opponent rebounds), regular season, ranked within each season.
#   - the champion list is typed in by hand.
#   - game-level win rates leave out games with tied rebounds.
#
# axis rule: anything with a length starts at zero.

library(dplyr)
library(ggplot2)
library(wehoop)

GSV    <- "#7048B6"   # golden state
ATL    <- "#C8102E"   # atlanta
HI     <- "#C77A1F"   # highlight for the #1 rebounding team
GRAY   <- "#8A8A86"
INK    <- "#15171D"
CREDIT <- "data: Her Hoop Stats  |  chart: @wnbadata"

team_colors <- c(`Golden State` = GSV, Atlanta = ATL)

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
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#E3E3E0", linewidth = 0.3),
      plot.margin   = margin(18, 18, 14, 10)
    )
}


# EVERY CHAMPION'S REGULAR SEASON REBOUNDING
# reb_rk: rank in share of rebounds that season (1 = best).
# houston, sacramento and detroit are listed under the names they had at the time
champ_reb <- tibble::tribble(
  ~year, ~champion,       ~reb_rk,
  1997,  "Houston",      4,
  1998,  "Houston",      8,
  1999,  "Houston",      3,
  2000,  "Houston",      2,
  2001,  "Los Angeles",  1,
  2002,  "Los Angeles",  1,
  2003,  "Detroit",      1,
  2004,  "Seattle",      3,
  2005,  "Sacramento",   2,
  2006,  "Detroit",      1,
  2007,  "Phoenix",      13,
  2008,  "Detroit",      1,
  2009,  "Phoenix",      11,
  2010,  "Seattle",      1,
  2011,  "Minnesota",    1,
  2012,  "Indiana",      11,
  2013,  "Minnesota",    1,
  2014,  "Phoenix",      8,
  2015,  "Minnesota",    4,
  2016,  "Los Angeles",  7,
  2017,  "Minnesota",    2,
  2018,  "Seattle",      4,
  2019,  "Washington",   5,
  2020,  "Seattle",      6,
  2021,  "Chicago",      8,
  2022,  "Las Vegas",    7,
  2023,  "Las Vegas",    4,
  2024,  "New York",     1,
  2025,  "Las Vegas",    10,
  2026,  "Golden State", 7
) |>
  mutate(era = case_when(year <= 2000 ~ "1997-2000", year <= 2013 ~ "2001-2013", TRUE ~ "2014-2026"))

# every number the script and hooks say
stopifnot(nrow(champ_reb) == 30,
          sum(champ_reb$reb_rk == 1) == 9,
          sum(champ_reb$reb_rk <= 3) == 14,
          sum(champ_reb$reb_rk == 1 & champ_reb$era == "2001-2013") == 8,
          sum(champ_reb$era == "2001-2013") == 13,
          sum(champ_reb$reb_rk == 1 & champ_reb$era == "2014-2026") == 1,
          sum(champ_reb$era == "2014-2026") == 13)


# 1. REBOUNDING RANK OF EVERY CHAMPION (hook: "from 2001 to 2013 she was right 8 out of 13 times. since then, 1 out of 13")
# y is flipped so 1st is at the top. #1 rebounding teams in orange, 2001-2013 shaded
# the script's other number from this table: the 2007 phoenix team finished last and got outrebounded by 7.0 a game
reb_labels <- filter(champ_reb, year %in% c(2007, 2024, 2026)) |>
  mutate(label = case_when(year == 2007 ~ "2007 phoenix: last",
                           year == 2024 ~ "2024 new york",
                           year == 2026 ~ "2026\ngolden state"),
         # 2026 above its dot and 2024 below it, so neither runs into the 2022 dot
         lab_hjust = case_when(year == 2026 ~ 1, year == 2024 ~ 0.5, TRUE ~ -0.12),
         lab_vjust = case_when(year == 2026 ~ -0.45, year == 2024 ~ 2.2, TRUE ~ 0.5))

p_reb_rank_by_year <- ggplot(champ_reb, aes(x = year, y = reb_rk)) +
  annotate("rect", xmin = 2000.5, xmax = 2013.5, ymin = 0.4, ymax = 13.6, fill = "#F8EBDD") +
  geom_point(aes(color = reb_rk == 1), size = 3.6) +
  geom_text(data = reb_labels, aes(label = label, hjust = lab_hjust, vjust = lab_vjust),
            color = INK, size = 3.6) +
  scale_color_manual(values = c(`FALSE` = INK, `TRUE` = HI), guide = "none") +
  scale_y_reverse(limits = c(13.8, 0.4), breaks = c(1, 4, 7, 10, 13)) +
  scale_x_continuous(limits = c(1996.5, 2026.5), breaks = c(1997, 2001, 2013, 2026), expand = expansion(0)) +
  labs(title = "rebounding rank of every wnba champion",
       subtitle = "regular season reb%, 1997-2026. orange = led the league",
       caption = paste0("30 champions, 1997-2026  ·  ", CREDIT), x = NULL, y = "rebounding rank") +
  base_theme() +
  theme(axis.title.y = element_text(color = GRAY, size = 12))


# 2. TITLES WON BY THE TEAM THAT LED THE LEAGUE (for comparison with the defense video)
# with 2026 included: the #1 defense won 9 (golden state was #1 on defense), the #1 offense 15,
# the #1 rebounding team 9
led_compare <- tibble::tribble(
  ~category,          ~n,
  "best offense",      15,
  "best defense",      9,
  "best rebounding",   9
) |>
  mutate(category = factor(category, levels = category))

stopifnot(led_compare$n[led_compare$category == "best rebounding"] == sum(champ_reb$reb_rk == 1))

p_reb_vs_others <- ggplot(led_compare, aes(x = category, y = n, fill = category == "best rebounding")) +
  geom_col(width = 0.58) +
  geom_text(aes(label = n), vjust = -0.5, color = INK, size = 7, fontface = "bold") +
  scale_fill_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_y_continuous(limits = c(0, 17), expand = expansion(mult = c(0, 0))) +
  labs(title = "titles won by the team that led the league",
       subtitle = "regular season offensive rating, defensive rating and reb%, 1997-2026",
       caption = paste0("30 champions, 1997-2026  ·  ", CREDIT), x = NULL, y = NULL) +
  base_theme() +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())


# 3. HOW OFTEN THE TEAM WITH MORE REBOUNDS WINS THE GAME
# games with tied rebounds left out. regular season 1997-2026: 4,129 of 6,210. finals games 1997-2026:
# 69 of 106. the edge is partly a byproduct of the other team missing more shots, so this is a
# pattern, not proof that rebounding causes wins
game_level <- tibble::tribble(
  ~games,                     ~won, ~decided,
  "regular season games",     4129, 6210,
  "finals games",             69,   106
) |>
  mutate(pct = 100 * won / decided,
         games = factor(games, levels = games))

p_more_reb_wins <- ggplot(game_level, aes(x = games, y = pct)) +
  geom_col(width = 0.55, fill = HI) +
  geom_hline(yintercept = 50, color = INK, linewidth = 0.5, linetype = "dashed") +
  annotate("text", x = 1.5, y = 50, label = "coin flip", vjust = -0.6, color = INK, size = 4) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.5, color = INK, size = 6.5, fontface = "bold") +
  geom_text(aes(y = 4, label = paste(formatC(won, big.mark = ",", format = "d"), "of",
                                     formatC(decided, big.mark = ",", format = "d"))), color = "white", size = 4) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100),
                     labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0))) +
  labs(title = "how often the team with more rebounds wins",
       subtitle = "games with tied rebounds left out, 1997-2026",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()


# 4. THE 2026 FINALS, GAME BY GAME (ESPN via wehoop)
# golden state won 4-3. the team with more rebounds won 5 of 7, but atlanta had more rebounds for the
# series (259 to 251) and lost
# game 7 isn't in wehoop's season file yet, so each game is pulled by its ESPN game id
finals_ids <- 401918305:401918311

finals_2026 <- bind_rows(lapply(finals_ids, \(id) espn_wnba_team_box(game_id = id))) |>
  transmute(game = as.integer(game_id) - min(finals_ids) + 1L,
            team = factor(ifelse(team_short_display_name == "Valkyries", "Golden State", "Atlanta"),
                          levels = c("Golden State", "Atlanta")),
            reb = as.integer(total_rebounds),
            pts = as.integer(team_score),
            won = as.logical(team_winner)) |>
  arrange(game, team)

finals_games <- finals_2026 |>
  group_by(game) |>
  summarise(winner = team[won], more_reb = team[which.max(reb)],
            score = paste0(max(pts), "-", min(pts)), .groups = "drop") |>
  # scores live in the tibble; the axis label only says who won so the 7 labels fit side by side
  mutate(game_label = paste0("game ", game, "\n", ifelse(winner == "Golden State", "gs", "atl"), " won"))

stopifnot(sum(finals_games$winner == finals_games$more_reb) == 5,
          sum(finals_2026$reb[finals_2026$team == "Golden State"]) == 251,
          sum(finals_2026$reb[finals_2026$team == "Atlanta"]) == 259)

# the winner's bar is solid, the loser's is faded
p_finals_2026_reb <- ggplot(finals_2026, aes(x = factor(game), y = reb, fill = team, alpha = won)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.72) +
  geom_text(aes(label = reb, color = team), position = position_dodge(width = 0.78),
            vjust = -0.5, size = 4.4, fontface = "bold", alpha = 1) +
  scale_fill_manual(values = team_colors, labels = c(`Golden State` = "golden state", Atlanta = "atlanta"),
                    name = NULL) +
  scale_color_manual(values = team_colors, guide = "none") +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.3), guide = "none") +
  scale_x_discrete(labels = finals_games$game_label) +
  scale_y_continuous(limits = c(0, 50), expand = expansion(mult = c(0, 0))) +
  labs(title = "rebounds in each game of the 2026 finals",
       subtitle = "solid bar = won the game. more rebounds won 5 of 7",
       caption = "data: ESPN via wehoop  |  chart: @wnbadata", x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top", legend.justification = "left", legend.location = "plot",
        legend.text = element_text(color = INK, size = 12),
        axis.text.x = element_text(size = 11, lineheight = 0.9))


# TABLES

# 2026 finals game by game (ESPN via wehoop)
finals_2026_games <- finals_2026 |>
  group_by(game) |>
  summarise(score = paste0(ifelse(team[won] == "Golden State", "gs ", "atl "), max(pts), "-", min(pts)),
            `gs rebounds` = reb[team == "Golden State"],
            `atl rebounds` = reb[team == "Atlanta"],
            .groups = "drop") |>
  mutate(`more rebounds won?` = ifelse((`gs rebounds` > `atl rebounds`) == startsWith(score, "gs"), "yes", "no"))

stopifnot(sum(finals_2026_games$`more rebounds won?` == "yes") == 5)

# chart index, in script order
#   p_reb_vs_others      "the best offense in the league won 15 titles, the best defense won 9 ... 9 teams that led in rebounding"
#   p_reb_rank_by_year   "8 of those 9 came pretty much in the first half of the wnba ... since 2014, the only team was the 2024 libs"
#   p_finals_2026_reb    "in the 2026 finals, 5 out of the 7 games were won by the team with more rebounds"
#   finals_2026_games    same beat as a table: score, both teams' rebounds, whether more rebounds won
#   p_more_reb_wins      "about 2 out of 3 regular season games ... and in the finals it's about the same"
p_reb_vs_others
p_reb_rank_by_year
p_finals_2026_reb
as.data.frame(finals_2026_games)
p_more_reb_wins
