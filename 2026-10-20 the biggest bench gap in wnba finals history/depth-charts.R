# charts for "the biggest bench gap in wnba finals history" (posted 10/20, after finals game 2)
# valkyries vs dream: the deepest bench in the wnba against the shallowest
# follower asks: balancing minutes across the bench vs stacking them (sadgirlmayaa),
# bench minutes in a shorter vs longer series (ajmchi)
#
# bench = players who didn't start that game. bench share = share of a team's minutes (or points)
# that went to the bench.
#
# data/finals-bench-history.csv: every finals team 1997-2025, bench share of minutes in the regular
# season, the playoffs before the finals, and the finals. from her hoop stats, except the 2024 finals,
# which come from wehoop. finals games = the games between the two teams in the season's last playoff game.
# wehoop and her hoop stats agree on the 2024 regular season within 0.1%.
#
# 2026 comes from wehoop (commissioner's cup final and all-star game dropped). wehoop rounds each
# player's minutes to the nearest whole minute, so 2026 shares can be off by a few tenths.
# re-run after each finals game and the 2026 finals numbers update on their own.

library(tidyverse)
library(wehoop)

DATA_DIR <- "data"

VALKS <- "#6F4BB2"
DREAM <- "#E31837"
GRAY  <- "#8A8A86"
LIGHT <- "#D6D2DE"
BLUE  <- "#5B95D6"
INK   <- "#010101"

# ---- history: finals teams 1997-2025 ----

history <- read_csv(file.path(DATA_DIR, "finals-bench-history.csv"), show_col_types = FALSE)

stopifnot(nrow(history) == 174, all(count(history, yr)$n == 6))

# ---- 2026 (wehoop) ----

drop_games <- load_wnba_schedule(seasons = 2026) |>
  filter(type_abbreviation %in% c("CC", "ALLSTAR")) |>   # commissioner's cup final, all-star game
  pull(game_id)

box <- load_wnba_player_box(seasons = 2026) |>
  filter(season_type %in% c(2, 3), !game_id %in% drop_games, !is.na(minutes), minutes > 0)

# every team, regular season
league <- box |>
  filter(season_type == 2) |>
  group_by(team = team_abbreviation) |>
  summarise(games = n_distinct(game_id),
            bench_min = sum(minutes[!starter]) / sum(minutes),
            bench_pts = sum(points[!starter]) / sum(points), .groups = "drop")

stopifnot(nrow(league) == 15, all(league$games == 44),
          league$team[which.max(league$bench_min)] == "GS", league$team[which.min(league$bench_min)] == "ATL",
          league$team[which.max(league$bench_pts)] == "GS")

# valkyries and dream by phase
now <- box |>
  filter(team_abbreviation %in% c("GS", "ATL")) |>
  mutate(phase = case_when(season_type == 2 ~ "reg",
                           game_date >= as.Date("2026-10-17") & opponent_team_abbreviation %in% c("GS", "ATL") ~ "finals",
                           TRUE ~ "playoffs")) |>
  group_by(team = team_abbreviation, phase) |>
  summarise(games = n_distinct(game_id),
            bench_min = sum(minutes[!starter]) / sum(minutes),
            bench_pts = sum(points[!starter]) / sum(points), .groups = "drop")

gs_reg  <- now$bench_min[now$team == "GS"  & now$phase == "reg"]
atl_reg <- now$bench_min[now$team == "ATL" & now$phase == "reg"]

# ---- every finals matchup, regular season ----

pairs <- history |>
  filter(phase == "reg") |>
  pivot_wider(id_cols = yr, names_from = champ, values_from = c(team, bench_min)) |>
  transmute(yr, champion = team_1, runner_up = team_0,
            deeper_team  = if_else(bench_min_1 > bench_min_0, team_1, team_0),
            other_team   = if_else(bench_min_1 > bench_min_0, team_0, team_1),
            deeper_bench = pmax(bench_min_1, bench_min_0),
            other_bench  = pmin(bench_min_1, bench_min_0),
            gap = deeper_bench - other_bench,
            deeper_won = bench_min_1 > bench_min_0)

gap_matchups <- pairs |>
  select(yr, deeper_team, other_team, deeper_bench, other_bench, gap) |>
  bind_rows(tibble(yr = 2026, deeper_team = "Golden State", other_team = "Atlanta",
                   deeper_bench = gs_reg, other_bench = atl_reg, gap = gs_reg - atl_reg)) |>
  arrange(desc(gap))

stopifnot(
  nrow(pairs) == 29,
  sum(pairs$deeper_won) == 12,                       # "the deeper bench has won 12 of 29 finals"
  pairs$yr[which.max(pairs$gap)] == 2006,            # old record: 2006 sacramento vs detroit
  gs_reg - atl_reg > max(pairs$gap)                  # "no 2 finals teams have ever been that far apart"
)

# ---- finals teams by phase ----

phase_avg <- history |>
  mutate(who = if_else(champ == 1, "champions", "runners-up")) |>
  group_by(who, phase) |>
  summarise(bench_min = mean(bench_min), .groups = "drop")

phases <- bind_rows(phase_avg,
                    now |> transmute(who = if_else(team == "GS", "valkyries 2026", "dream 2026"), phase, bench_min)) |>
  mutate(phase = factor(phase, c("reg", "playoffs", "finals"), c("regular season", "playoffs", "finals")),
         who = factor(who, c("valkyries 2026", "dream 2026", "champions", "runners-up")))

# ---- charts ----

theme_wnbadata <- function() {
  theme_minimal(base_size = 15) +
    theme(plot.title.position = "plot", plot.caption.position = "plot",
          plot.title    = element_text(face = "bold", colour = INK, size = 19),
          plot.subtitle = element_text(colour = GRAY, size = 12, margin = margin(b = 14)),
          plot.caption  = element_text(colour = GRAY, size = 11, hjust = 0, margin = margin(t = 14)),
          axis.text     = element_text(colour = INK, size = 12),
          axis.title    = element_text(colour = GRAY, size = 12),
          panel.grid.minor = element_blank(),
          plot.margin   = margin(18, 18, 14, 10))
}
pct <- \(x) paste0(round(100 * x), "%")
CAPTION_2026 <- "bench = players who didn't start that game  |  chart: @wnbadata\ndata: wehoop"
CAPTION_HIST <- "bench = players who didn't start that game  |  chart: @wnbadata\ndata: her hoop stats (1997-2025), wehoop (2024 finals, 2026)"
TEAM_COLORS  <- c(valkyries = VALKS, dream = DREAM)

# every team, 2026 regular season
plot_league <- function(col, title) {
  d <- league |>
    mutate(value = .data[[col]],
           who = case_when(team == "GS" ~ "valkyries", team == "ATL" ~ "dream", TRUE ~ "other"))
  ggplot(d, aes(x = value, y = fct_reorder(team, value), fill = who)) +
    geom_col(width = 0.75) +
    geom_text(aes(label = pct(value)), hjust = -0.2, colour = INK, size = 4.2,
              fontface = ifelse(d$who == "other", "plain", "bold")) +
    scale_fill_manual(values = c(valkyries = VALKS, dream = DREAM, other = LIGHT), guide = "none") +
    scale_x_continuous(limits = c(0, 0.5), labels = pct, expand = c(0, 0)) +
    labs(title = title, subtitle = "2026 regular season, every wnba team",
         caption = CAPTION_2026, x = NULL, y = NULL) +
    theme_wnbadata() +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 11))
}

# 1. the hook: valkyries vs dream, bench share of minutes
hook <- tibble(team = factor(c("valkyries", "dream"), c("valkyries", "dream")), bench = c(gs_reg, atl_reg))

p_hook <- ggplot(hook, aes(x = bench, y = fct_rev(team), fill = team)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = pct(bench)), hjust = 1.15, colour = "white", size = 7, fontface = "bold") +
  scale_fill_manual(values = TEAM_COLORS, guide = "none") +
  scale_x_continuous(limits = c(0, 0.5), labels = pct, expand = c(0, 0)) +
  labs(title = "share of minutes played by the bench", subtitle = "2026 regular season",
       caption = CAPTION_2026, x = NULL, y = NULL) +
  theme_wnbadata() +
  theme(axis.text.y = element_text(face = "bold", size = 15), panel.grid.major.y = element_blank())

p_league_minutes <- plot_league("bench_min", "share of minutes played by the bench")

# 2. the deeper bench has won 12 of 29 finals
record <- pairs |>
  count(result = if_else(deeper_won, "deeper team won", "deeper team lost")) |>
  mutate(result = factor(result, c("deeper team won", "deeper team lost")))

p_record <- ggplot(record, aes(x = n, y = fct_rev(result), fill = result)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n), hjust = 1.4, colour = "white", size = 8, fontface = "bold") +
  scale_fill_manual(values = c(`deeper team won` = BLUE, `deeper team lost` = GRAY), guide = "none") +
  scale_x_continuous(limits = c(0, 22), expand = c(0, 0)) +
  labs(title = "wnba finals, 1997-2025",
       subtitle = "deeper team = its bench played more of the regular season minutes",
       caption = CAPTION_HIST, x = "finals", y = NULL) +
  theme_wnbadata() +
  theme(axis.text.y = element_text(face = "bold", size = 14), panel.grid.major.y = element_blank())

# 3. everyone shortens the bench in the playoffs (faint lines = every finals team)
ghosts <- history |>
  mutate(team_season = paste(yr, team),
         phase = factor(phase, c("reg", "playoffs", "finals"), c("regular season", "playoffs", "finals")))

p_phases <- ggplot(phases, aes(x = phase, y = bench_min, colour = who, group = who)) +
  geom_line(data = ghosts, mapping = aes(x = phase, y = bench_min, group = team_season), inherit.aes = FALSE,
            colour = GRAY, alpha = 0.18, linewidth = 0.6) +
  geom_line(linewidth = 1.6) +
  geom_point(size = 4) +
  geom_text(data = filter(phases, phase == "finals"), aes(label = paste(who, pct(bench_min))),
            hjust = -0.1, size = 4.5, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = c(`valkyries 2026` = VALKS, `dream 2026` = DREAM, champions = INK, `runners-up` = GRAY),
                      guide = "none") +
  scale_y_continuous(limits = c(0, 0.52), labels = pct) +
  scale_x_discrete(expand = expansion(add = c(0.3, 1.6))) +
  labs(title = "share of minutes played by the bench",
       subtitle = "faint lines = every finals team, 1997-2025\nchampions and runners-up = their average  |  2026 finals = games played so far",
       caption = CAPTION_HIST, x = NULL, y = NULL) +
  theme_wnbadata() +
  theme(panel.grid.major.x = element_blank())

# 4. points: the valkyries' bench scores even more than it plays
minutes_vs_points <- now |>
  filter(phase == "reg") |>
  transmute(team = factor(if_else(team == "GS", "valkyries", "dream"), c("valkyries", "dream")),
            minutes = bench_min, points = bench_pts) |>
  pivot_longer(c(minutes, points), names_to = "share_of", values_to = "bench") |>
  mutate(share_of = factor(share_of, c("minutes", "points")))

p_points <- ggplot(minutes_vs_points, aes(x = share_of, y = bench, fill = team, alpha = share_of)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = pct(bench)), position = position_dodge(width = 0.75), vjust = -0.5,
            colour = INK, size = 5.5, fontface = "bold", alpha = 1) +
  scale_fill_manual(values = TEAM_COLORS, name = NULL) +
  scale_alpha_manual(values = c(minutes = 0.6, points = 1), guide = "none") +
  scale_y_continuous(limits = c(0, 0.52), labels = pct, expand = c(0, 0)) +
  labs(title = "how much of the team the bench is",
       subtitle = "share of minutes and points from players who didn't start, 2026 regular season",
       caption = CAPTION_2026, x = NULL, y = NULL) +
  theme_wnbadata() +
  theme(legend.position = "top", legend.justification = "left", panel.grid.major.x = element_blank(),
        axis.text.x = element_text(face = "bold", size = 14))

p_league_points <- plot_league("bench_pts", "share of points scored by the bench")

# ---- tables (percentages are out of 100) ----

p100 <- \(x) round(100 * x, 1)

# "no 2 finals teams have ever been that far apart": every finals matchup, deeper bench first, biggest gap first
t_gap_matchups <- gap_matchups |>
  mutate(across(c(deeper_bench, other_bench, gap), p100))
t_gap_matchups |> print(n = 30)

# ---- charts (in script order) ----
p_hook            # "40% of the valkyries' minutes went to players who didn't start ... for the dream it was 22%"
p_league_minutes  # "the most in the wnba / the least in the wnba"
p_record          # "the deeper bench has won 12 of 29 finals, basically a coin flip"
p_phases          # "finals teams go from an average 29% ... to about 23% in the finals"
p_points          # "44% of the valkyries' points came from players who didn't start ... the dream's bench scored 20%"
p_league_points   # "also the most in the league" (the dream are 14th, not last)
