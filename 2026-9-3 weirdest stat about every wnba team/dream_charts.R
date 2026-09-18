# charts for the atlanta dream "weird 2/15" video
#
# everything here is computed from wehoop (espn). quarter scores are rebuilt from the
# running play by play score, since wehoop's team box is game level only. these numbers
# were cross checked against her hoop stats and match exactly.
#
# two mandatory wehoop filters or the season is wrong:
#   - the commissioner's cup final sits inside season_type == 2 but does not count
#   - the all star rosters appear as their own "teams"
#
# axis rule, same as the valkyries file: anything with a length starts at zero.
# bars and lollipops are anchored at 0 and the axis is always drawn, never hidden.

library(wehoop)
library(dplyr)
library(tidyr)
library(ggplot2)

ALLSTAR   <- c("Team Spoon", "Team Coop")
CUP_FINAL <- as.Date("2026-06-30")
ATL       <- "Atlanta"

HI     <- "#C8102E"   # atlanta
GRAY   <- "#8A8A86"   # everyone else
OPP    <- "#B4B2A9"   # opponent bars
INK    <- "#15171D"
CREDIT <- "2026 season through the fiba break  ·  data: her hoop stats and wehoop  |  chart: @wnbadata"

pbp <- load_wnba_pbp(seasons = 2026) |> filter(season_type == 2)
tb  <- load_wnba_team_box(seasons = 2026) |>
  filter(season_type == 2) |>
  select(game_id, game_date, team_id, team_location) |>
  distinct()

# end of period cumulative score, then differenced into per quarter points
end_of_period <- pbp |>
  filter(period_number <= 4) |>
  group_by(game_id, period_number) |>
  summarise(h = max(home_score), a = max(away_score), .groups = "drop") |>
  pivot_wider(names_from = period_number, values_from = c(h, a))

quarters <- end_of_period |>
  transmute(game_id,
            h1 = h_1, h2 = h_2 - h_1, h3 = h_3 - h_2, h4 = h_4 - h_3,
            a1 = a_1, a2 = a_2 - a_1, a3 = a_3 - a_2, a4 = a_4 - a_3,
            h_after3 = h_3, a_after3 = a_3, h_final = h_4, a_final = a_4)

home_ids <- pbp |> select(game_id, home_team_id) |> distinct()

game_quarters <- quarters |>
  inner_join(home_ids, by = "game_id") |>
  inner_join(tb, by = "game_id", relationship = "many-to-many") |>
  mutate(home = team_id == home_team_id,
         q1  = if_else(home, h1, a1),  q2  = if_else(home, h2, a2),
         q3  = if_else(home, h3, a3),  q4  = if_else(home, h4, a4),
         oq1 = if_else(home, a1, h1),  oq2 = if_else(home, a2, h2),
         oq3 = if_else(home, a3, h3),  oq4 = if_else(home, a4, h4),
         after3 = if_else(home, h_after3 - a_after3, a_after3 - h_after3),
         final  = if_else(home, h_final  - a_final,  a_final  - h_final),
         team   = team_location) |>
  filter(!team %in% ALLSTAR, game_date != CUP_FINAL) |>
  select(game_id, game_date, team, q1:q4, oq1:oq4, after3, final)

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

# 1. THE LEAD. season long fourth quarter surplus, 120 for atlanta against 49 for indiana
surplus <- game_quarters |>
  group_by(team) |>
  summarise(surplus = sum(q4) - sum(oq4), .groups = "drop")

p_surplus <- ggplot(surplus, aes(x = surplus, y = reorder(team, surplus))) +
  geom_segment(aes(x = 0, xend = surplus, yend = team, colour = team == ATL),
               linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
  geom_point(aes(colour = team == ATL, size = team == ATL)) +
  geom_text(aes(label = surplus, colour = team == ATL),
            hjust = ifelse(surplus$surplus >= 0, -0.45, 1.45), size = 4.2, show.legend = FALSE) +
  geom_vline(xintercept = 0, colour = INK, linewidth = 0.4) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.12, 0.14))) +
  labs(title = "atlanta has outscored teams by 120 points in fourth quarters",
       subtitle = "fourth quarter points scored minus allowed, whole season",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# 2. THE STAIRCASE. anchored at zero on purpose, which is why the steps look small.
# that is the honest picture and it sets up the debunk rather than fighting it.
atl_q <- game_quarters |>
  filter(team == ATL) |>
  summarise(across(q1:q4, mean)) |>
  pivot_longer(everything(), names_to = "qtr", values_to = "pts") |>
  mutate(qtr = recode(qtr, q1 = "1st", q2 = "2nd", q3 = "3rd", q4 = "4th"))

p_staircase <- ggplot(atl_q, aes(x = qtr, y = pts)) +
  geom_col(fill = HI, width = 0.62, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f", pts)), vjust = -0.6, colour = INK, size = 4.6) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.14))) +
  labs(title = "the atlanta dream staircase",
       subtitle = "average points scored by quarter",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

# 3. THE DEBUNK. how often a team actually climbs all four quarters in one game
ascending <- game_quarters |>
  group_by(team) |>
  summarise(games = sum(q1 < q2 & q2 < q3 & q3 < q4), .groups = "drop")

p_ascending <- ggplot(ascending, aes(x = games, y = reorder(team, games))) +
  geom_segment(aes(x = 0, xend = games, yend = team, colour = team == ATL),
               linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
  geom_point(aes(colour = team == ATL, size = team == ATL)) +
  geom_text(aes(label = games, colour = team == ATL), hjust = -0.8, size = 4.2, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
  scale_x_continuous(limits = c(0, NA), breaks = 0:3, expand = expansion(mult = c(0, 0.16))) +
  labs(title = "no team climbs all four quarters more than twice a season",
       subtitle = "games out of 40 where a team scored more in every quarter than the last",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# 4. THE THREE NAMED GAMES, quarter by quarter against the opponent
named <- game_quarters |>
  filter(team == ATL, game_date %in% as.Date(c("2026-05-09", "2026-06-24", "2026-07-29"))) |>
  select(game_date, q1:q4, oq1:oq4) |>
  pivot_longer(q1:oq4, names_to = "k", values_to = "pts") |>
  mutate(side = if_else(startsWith(k, "o"), "opponent", "atlanta"),
         qtr  = recode(gsub("^o?q", "", k), `1` = "1st", `2` = "2nd", `3` = "3rd", `4` = "4th"),
         label = recode(as.character(game_date),
                        "2026-05-09" = "5/9 at minnesota, won by 1",
                        "2026-06-24" = "6/24 vs golden state, lost by 11",
                        "2026-07-29" = "7/29 at dallas, won by 1"))

p_games <- ggplot(named, aes(x = qtr, y = pts, fill = side)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.64) +
  geom_text(aes(label = pts), position = position_dodge(width = 0.72),
            vjust = -0.5, colour = INK, size = 3.6) +
  facet_wrap(~label, nrow = 1) +
  scale_fill_manual(values = c(atlanta = HI, opponent = OPP), name = NULL) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
  labs(title = "what the fourth quarter actually looks like",
       subtitle = "points by quarter, atlanta against the opponent",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3),
        strip.text = element_text(colour = INK, size = 12))

# 5. THE GARBAGE TIME TEST. all games against only the ones still within 10 after three.
# top six teams only, otherwise thirty bars is unreadable on a phone.
gt <- game_quarters |>
  group_by(team) |>
  summarise(`all games` = mean(q4 - oq4),
            `still within 10 after 3` = mean((q4 - oq4)[abs(after3) <= 10]),
            .groups = "drop") |>
  slice_max(`all games`, n = 6) |>
  pivot_longer(-team, names_to = "cut", values_to = "margin") |>
  mutate(cut = factor(cut, levels = c("all games", "still within 10 after 3")))

p_filter <- ggplot(gt, aes(x = margin, y = reorder(team, margin), fill = cut)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.64) +
  geom_text(aes(label = sprintf("%.2f", margin)), position = position_dodge(width = 0.72),
            hjust = -0.2, colour = INK, size = 3.6) +
  scale_fill_manual(values = c(`all games` = OPP, `still within 10 after 3` = HI), name = NULL) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.2))) +
  labs(title = "atlanta stays first even after you throw out the blowouts",
       subtitle = "fourth quarter point differential per game, six best teams",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top")

# render one at a time in positron. for 9:16 size the plot pane tall and narrow.
p_surplus
p_staircase
p_ascending
p_games
p_filter
