# charts for "the dream win when angel makes a three" (posted 10/25, before finals game 5)
# the dream are 15-0 in 2026 when angel reese makes a 3. builds on jack maloney's cbs sports piece (10/23)
# with the finals losses, the score when her 3s went in, career counts and her long 2s turning into 3s.
#
# everything comes from wehoop (espn): wnba and college box scores for records and career totals,
# play by play for the score before each 3, the passers and shot distances.
# regular season + playoffs only; the commissioner's cup final and all-star games are dropped.
#
# known wehoop gaps:
# - the 2024 and 2025 play by play is missing 1 of her 3-point attempts each season, so attempt counts
#   come from the box scores and the shot-by-shot tables are short by those 2
# - espn rounds shot distances to the foot, so long 2s (16+ ft) here are 37 / 11 / 9. the video said
#   36 and 8, which came from a play by play source with exact distances

library(tidyverse)
library(wehoop)

REESE_ID <- 4433402   # her espn athlete id, the same in college and the wnba

DREAM <- "#E31837"
DREAM_BLUE <- "#5091CC"   # the dream's alternate color (wehoop team_alternate_color)
BLUE_TINT  <- "#C9DDF1"
GRAY  <- "#8A8A86"
LIGHT <- "#D6D2DE"
INK   <- "#010101"

md <- \(d) paste0(month(d), "/", day(d))

# ---- wnba box scores, 2024-26 ----

drop_games <- load_wnba_schedule(seasons = 2024:2026) |>
  filter(type_abbreviation %in% c("CC", "ALLSTAR")) |>   # commissioner's cup final, all-star games
  pull(game_id)

wnba_box <- load_wnba_player_box(seasons = 2024:2026) |>
  filter(athlete_id == REESE_ID, season_type %in% c(2, 3), !game_id %in% drop_games, !is.na(minutes), minutes > 0) |>
  transmute(game_id, season, game_date, season_type = if_else(season_type == 3, "playoffs", "regular season"),
            team = str_to_lower(team_location), opponent = str_to_lower(opponent_team_location),
            fg3m = three_point_field_goals_made, fg3a = three_point_field_goals_attempted,
            team_pts = team_score, opp_pts = opponent_team_score, win = team_winner)

reese <- wnba_box |>
  filter(season == 2026) |>
  mutate(made3 = fg3m > 0,
         finals = season_type == "playoffs" & game_date >= as.Date("2026-10-17"))

THROUGH <- md(max(reese$game_date))

# ---- play by play: every shot she took (not free throws) ----

# espn scores are after the play, so the margin before a make takes the shot's points back off
shot_rows <- \(pbp) {
  pbp |>
    filter(athlete_id_1 == REESE_ID, shooting_play, !str_detect(type_text, "Free Throw")) |>
    distinct(game_id, period_number, clock_display_value, text, .keep_all = TRUE) |>   # espn repeats a few plays
    mutate(home = team_id == home_team_id,
           opponent = str_to_lower(if_else(home, away_team_name, home_team_name)),
           three = str_detect(text, regex("three point", ignore_case = TRUE)),
           made = scoring_play,
           ft = as.numeric(str_match(text, "(\\d+)-foot")[, 2]),
           margin_before = if_else(home, home_score - away_score, away_score - home_score) - if_else(made, score_value, 0L),
           passer = str_to_lower(coalesce(str_match(text, "\\((.+) assists\\)")[, 2],
                                          str_match(text, "Assisted by (.+)\\.$")[, 2]))) |>
    transmute(game_id, season, game_date, qtr = period_number, clock = clock_display_value, opponent,
              three, made, ft, margin_before, passer)
}

wnba_shots <- load_wnba_pbp(seasons = 2024:2026) |>
  filter(season_type %in% c(2, 3), !game_id %in% drop_games) |>
  shot_rows() |>
  left_join(wnba_box |> select(game_id, season_type, team), by = "game_id")

# ---- college: maryland 2020-22, lsu 2022-24 ----

college_box <- load_wbb_player_box(seasons = 2021:2024) |>
  filter(athlete_id == REESE_ID, !is.na(minutes), minutes > 0) |>
  group_by(season, team = str_to_lower(team_location)) |>
  summarise(games = n(), fg3m = sum(three_point_field_goals_made), fg3a = sum(three_point_field_goals_attempted),
            .groups = "drop")

college_shots <- load_wbb_pbp(seasons = 2021:2024) |>
  filter(athlete_id_1 == REESE_ID) |>
  shot_rows() |>
  left_join(college_box |> select(season, team), by = "season")

# ---- checks ----

made_26 <- wnba_shots |> filter(season == 2026, three, made)

stopifnot(all(reese$win == (reese$team_pts > reese$opp_pts)),
          nrow(made_26) == sum(reese$fg3m),
          sum(college_shots$three & college_shots$made) == sum(college_box$fg3m),
          sum(college_shots$three) == sum(college_box$fg3a))

# ---- records ----

rec <- \(w, l) paste0(w, "-", l)

record <- reese |>
  group_by(made3) |>
  summarise(wins = sum(win), losses = sum(!win), .groups = "drop") |>
  mutate(label = if_else(made3, "made a 3", "didn't make a 3"))

record_split <- reese |>
  group_by(season_type, made3) |>
  summarise(wins = sum(win), losses = sum(!win), .groups = "drop") |>
  mutate(label = if_else(made3, "made a 3", "didn't make a 3"),
         season_type = factor(season_type, c("regular season", "playoffs")))

# every 2026 made 3, with the score right before it (for the "11 of 19 within 5 points" line)
makes <- made_26 |>
  arrange(game_date) |>
  mutate(close = abs(margin_before) <= 5,
         game = paste0(md(game_date), if_else(season_type == "playoffs", " (playoffs)", "")))

# every 3 she has taken, college + wnba, one row per shot
score_word <- \(m) case_when(m > 0 ~ paste("up", m), m < 0 ~ paste("down", -m), TRUE ~ "tied")

all_threes <- bind_rows(
  college_shots |> filter(three) |> mutate(level = "college"),
  wnba_shots |> filter(three) |>
    mutate(level = if_else(season_type == "playoffs", "wnba playoffs", "wnba"),
           team = if_else(team == "chicago", "sky", "dream"))
) |>
  arrange(game_date) |>
  transmute(date = game_date, level, team, vs = opponent, qtr, clock, score = score_word(margin_before),
            made, result = if_else(made, "make \U0001F3AF", "miss"), ft, passer = coalesce(passer, "-"))

# ---- theme ----

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
CAPTION <- paste0("2026 regular season + playoffs through ", THROUGH, "  |  chart: @wnbadata\ndata: wehoop")
CAPTION_CAREER <- "college = maryland 2020-22, lsu 2022-24  |  chart: @wnbadata\ndata: wehoop"

# ---- charts ----

# 1. the hook: 15-0 when she makes a 3, 22-15 when she doesn't
hook <- record |>
  pivot_longer(c(wins, losses), names_to = "result", values_to = "n") |>
  mutate(result = factor(result, c("losses", "wins")), label = factor(label, c("didn't make a 3", "made a 3")))

p_hook <- ggplot(hook, aes(x = n, y = label)) +
  geom_col(aes(fill = result), width = 0.6, position = "stack", colour = "white", linewidth = 0.8) +
  geom_text(data = record, aes(x = wins + losses, label = rec(wins, losses)),
            hjust = -0.2, colour = INK, size = 7, fontface = "bold") +
  scale_fill_manual(values = c(wins = DREAM, losses = LIGHT), guide = "none") +
  scale_x_continuous(limits = c(0, max(record$wins + record$losses) * 1.4), expand = c(0, 0)) +
  labs(title = "dream record, by angel reese 3s",
       subtitle = "games she played, wins (red) and losses (gray)",
       caption = CAPTION, x = "games", y = NULL) +
  theme_wnbadata() +
  theme(axis.text.y = element_text(face = "bold", size = 15), panel.grid.major.y = element_blank())

# 2. "their 2 finals losses are the 2 finals games she didn't make one"
finals <- reese |>
  filter(finals) |>
  arrange(game_date) |>
  mutate(game = paste("game", row_number()),
         result = if_else(win, "win", "loss"),
         label = paste0(result, "\n", team_pts, "-", opp_pts, "\n\n", fg3m, if_else(fg3m == 1, " three", " threes")))

p_finals <- ggplot(finals, aes(x = game, y = 1, fill = result)) +
  geom_tile(width = 0.92, height = 0.9, colour = "white", linewidth = 2) +
  geom_text(aes(label = label, colour = result), size = 6.5, fontface = "bold", lineheight = 1.1) +
  scale_fill_manual(values = c(win = DREAM, loss = LIGHT), guide = "none") +
  scale_colour_manual(values = c(win = "white", loss = INK), guide = "none") +
  labs(title = "2026 finals: result and angel reese 3s made", subtitle = "dream vs valkyries",
       caption = CAPTION, x = NULL, y = NULL) +
  theme_wnbadata() +
  theme(panel.grid = element_blank(), axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 15))

# 3. "atlanta is 9-0 in the regular season and 6-0 in the playoffs" as 15 W's, not bars
# one row per part of the season, so the chart fills the width at any plot size
win_dots <- reese |>
  filter(made3) |>
  arrange(game_date) |>
  mutate(part = factor(season_type, c("regular season", "playoffs"))) |>
  group_by(part) |>
  mutate(col = row_number() - 1) |>
  ungroup() |>
  mutate(y = if_else(part == "playoffs", -1.9, 0))

win_headers <- win_dots |>
  group_by(part) |>
  summarise(y = first(y) + 0.75, label = paste0(first(part), ": ", sum(win), "-", sum(!win)), .groups = "drop")

# circles drawn in plot units so they scale with the plot pane instead of overlapping
circle_pts <- \(d, r = 0.42, n = 60) {
  d |>
    select(game_date, part, x0 = col, y0 = y) |>
    cross_join(tibble(t = seq(0, 2 * pi, length.out = n))) |>
    mutate(x = x0 + r * cos(t), y = y0 + r * sin(t))
}

p_win_dots <- ggplot(win_dots, aes(x = col, y = y)) +
  geom_polygon(data = circle_pts(win_dots), aes(x = x, y = y, group = game_date, fill = part)) +
  geom_text(aes(label = if_else(win, "W", "L")), colour = "white", size = 6, fontface = "bold") +
  geom_text(aes(y = y - 0.6, label = md(game_date)), colour = GRAY, size = 3.3) +
  geom_text(data = win_headers, aes(x = -0.45, y = y, label = label, colour = part), hjust = 0,
            size = 5.5, fontface = "bold") +
  scale_fill_manual(values = c(`regular season` = DREAM_BLUE, playoffs = DREAM), guide = "none") +
  scale_colour_manual(values = c(`regular season` = DREAM_BLUE, playoffs = DREAM), guide = "none") +
  coord_fixed(xlim = c(-0.6, 8.6), ylim = c(-2.75, 1.05), expand = FALSE) +
  labs(title = "games angel reese made a 3", subtitle = "dream result and date",
       caption = CAPTION, x = NULL, y = NULL) +
  theme_wnbadata() +
  theme(panel.grid = element_blank(), axis.text = element_blank())

# 4. "she never made more than 4 in a season before 2026 / but this year: she's made 19"
career <- bind_rows(
  college_box |> mutate(league = "college", season_type = "regular season"),
  wnba_box |> group_by(season, team, season_type) |>
    summarise(games = n(), fg3m = sum(fg3m), fg3a = sum(fg3a), .groups = "drop") |> mutate(league = "wnba")
)

career_seasons <- career |>
  group_by(league, season, team) |>
  summarise(fg3m = sum(fg3m), fg3a = sum(fg3a), games = sum(games), .groups = "drop") |>
  arrange(season, league) |>   # lsu 2023-24 before the sky in 2024
  mutate(label = if_else(league == "college", paste0(team, "\n", season - 1, "-", str_sub(season, 3)),
                         paste0(if_else(team == "chicago", "sky", "dream"), "\n", season)),
         label = fct_inorder(label),
         now = season == 2026 & league == "wnba")

stopifnot(sum(career_seasons$fg3m[career_seasons$league == "college"]) == 5,
          sum(career_seasons$fg3m[career_seasons$league == "wnba" & !career_seasons$now]) == 7,
          max(career_seasons$fg3m[!career_seasons$now]) == 4)

# one bar per season; 2026 is split into regular season and playoffs
career_bars <- career |>
  left_join(career_seasons |> select(league, season, team, label, now), by = c("league", "season", "team")) |>
  mutate(part = case_when(!now ~ "before 2026",
                          season_type == "playoffs" ~ "2026 playoffs",
                          TRUE ~ "2026 regular season"),
         part = factor(part, c("2026 playoffs", "2026 regular season", "before 2026"))) |>
  group_by(label, part) |>
  summarise(fg3m = sum(fg3m), .groups = "drop")

p_career <- ggplot(career_bars, aes(x = label, y = fg3m, fill = part)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.8) +
  geom_text(data = career_seasons, aes(x = label, y = fg3m, label = fg3m), inherit.aes = FALSE,
            vjust = -0.5, colour = INK, size = 6, fontface = "bold") +
  scale_fill_manual(values = c(`2026 playoffs` = DREAM, `2026 regular season` = DREAM_BLUE, `before 2026` = GRAY),
                    breaks = c("2026 regular season", "2026 playoffs"), name = NULL) +
  scale_y_continuous(limits = c(0, max(career_seasons$fg3m) * 1.15), expand = c(0, 0)) +
  labs(title = "angel reese 3-pointers made by season",
       subtitle = paste0("regular season + playoffs, 2026 through ", THROUGH),
       caption = CAPTION_CAREER, x = NULL, y = NULL) +
  theme_wnbadata() +
  theme(panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 11, lineheight = 1),
        legend.position = "top", legend.justification = "left")

# 5. "she went 0 for 8 from 3 in may / but has since made 8 threes in the playoffs alone"
monthly <- reese |>
  mutate(span = if_else(season_type == "playoffs", "playoffs", str_to_lower(format(game_date, "%b")))) |>
  group_by(span) |>
  summarise(fg3m = sum(fg3m), fg3a = sum(fg3a), first = min(game_date), .groups = "drop") |>
  arrange(first) |>
  mutate(span = fct_inorder(span))

p_monthly <- ggplot(monthly, aes(x = span)) +
  geom_col(aes(y = fg3a), fill = BLUE_TINT, width = 0.7) +
  geom_col(aes(y = fg3m), fill = DREAM, width = 0.7) +
  geom_text(aes(y = fg3a, label = paste0(fg3m, " of ", fg3a)), vjust = -0.5, colour = INK, size = 5.5,
            fontface = "bold") +
  scale_y_continuous(limits = c(0, max(monthly$fg3a) * 1.15), expand = c(0, 0)) +
  labs(title = "angel reese 3-pointers by month, 2026",
       subtitle = "made (red) out of attempted (light blue)", caption = CAPTION, x = NULL, y = NULL) +
  theme_wnbadata() +
  theme(panel.grid.major.x = element_blank(), axis.text.x = element_text(face = "bold", size = 13))

# 6. "in 2024 she took 36 long 2s and 16 threes / but this year: 8 long 2s and 75 threes"
# 3-point attempts from the box scores; long 2s from play by play distances (espn rounds to the foot)
jumpers <- wnba_shots |>
  filter(!three) |>
  group_by(season) |>
  summarise(long2a = sum(ft >= 16, na.rm = TRUE)) |>
  left_join(wnba_box |> group_by(season) |> summarise(fg3a = sum(fg3a), games = n()), by = "season")

jumper_long <- jumpers |>
  pivot_longer(c(long2a, fg3a), names_to = "shot", values_to = "n") |>
  mutate(shot = factor(if_else(shot == "long2a", "long 2 attempts (16+ ft)", "3-point attempts"),
                       c("long 2 attempts (16+ ft)", "3-point attempts")),
         season_label = paste0(season, "\n", games, " games"))

p_jumpers <- ggplot(jumper_long, aes(x = season_label, y = n, fill = shot)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7, colour = "white", linewidth = 0.8) +
  geom_text(aes(label = n), position = position_dodge(width = 0.75), vjust = -0.5, colour = INK, size = 6,
            fontface = "bold") +
  scale_fill_manual(values = c(`long 2 attempts (16+ ft)` = DREAM_BLUE, `3-point attempts` = DREAM), name = NULL) +
  scale_y_continuous(limits = c(0, max(jumper_long$n) * 1.15), expand = c(0, 0)) +
  labs(title = "angel reese jump shot attempts by season",
       subtitle = "makes and misses, wnba regular season + playoffs",
       caption = CAPTION, x = NULL, y = "attempts") +
  theme_wnbadata() +
  theme(panel.grid.major.x = element_blank(), legend.position = "top", legend.justification = "left",
        axis.text.x = element_text(face = "bold", size = 13, lineheight = 1))

# 7. not in the script: 18 of her 19 threes were assisted, 10 of them by jordin canada
passers <- made_26 |>
  count(passer = coalesce(passer, "unassisted")) |>
  mutate(passer = fct_relevel(fct_reorder(passer, n), "unassisted"),   # unassisted sits at the bottom
         who = case_when(passer == "jordin canada" ~ "canada", passer == "unassisted" ~ "none", TRUE ~ "other"))

p_passers <- ggplot(passers, aes(x = n, y = passer, fill = who)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = n), hjust = -0.4, colour = INK, size = 6, fontface = "bold") +
  scale_fill_manual(values = c(canada = DREAM, other = DREAM_BLUE, none = LIGHT), guide = "none") +
  scale_x_continuous(limits = c(0, max(passers$n) * 1.2), expand = c(0, 0)) +
  labs(title = "assists on angel reese 3-pointers, 2026",
       subtitle = paste0(sum(!is.na(made_26$passer)), " of ", nrow(made_26), " made 3s were assisted"),
       caption = CAPTION, x = NULL, y = NULL) +
  theme_wnbadata() +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(face = "bold", size = 14))

# ---- tables ----

t_record <- record_split |> mutate(record = rec(wins, losses))
t_record

t_finals <- finals |> select(game, game_date, fg3m, fg3a, team_pts, opp_pts, win)
t_finals

# "11 of her 19 went in with the game within 5 points"
t_score <- makes |>
  mutate(score_when_it_went_in = case_when(margin_before <= -6 ~ "dream down 6+",
                                           abs(margin_before) <= 5 ~ "within 5 points",
                                           margin_before <= 14 ~ "dream up 6-14",
                                           TRUE ~ "dream up 15+"),
         score_when_it_went_in = factor(score_when_it_went_in,
                                        c("dream down 6+", "within 5 points", "dream up 6-14", "dream up 15+"))) |>
  count(score_when_it_went_in, name = "made_3s")
t_score

t_career <- career_seasons |> select(league, season, team, games, fg3m, fg3a)
t_career

t_monthly <- monthly |> select(span, fg3m, fg3a)
t_monthly

# every 3 she has made, college + wnba
t_threes_made <- all_threes |> filter(made) |> select(date, level, team, vs, qtr, clock, score, ft, passer)
t_threes_made |> print(n = Inf, width = Inf)

# every 3 she has taken, college + wnba
t_threes_taken <- all_threes |> select(date, level, team, vs, qtr, clock, score, result, ft, passer)
t_threes_taken |> print(n = Inf, width = Inf)

# ---- charts (in script order) ----
p_hook         # "if angel reese makes a 3, the dream win every single time" (15-0, and 22-15 when she doesn't)
p_finals       # "their 2 finals losses are the 2 finals games she didn't make one"
p_win_dots     # "atlanta is 9-0 in the regular season and 6-0 in the playoffs when angel makes a three"
p_career       # "she never made more than 4 in a season before 2026 / but this year: she's made 19"
p_monthly      # "she went 0 for 8 from 3 in may / but has since made 8 threes in the playoffs alone"
p_jumpers      # "in 2024 she took 36 long 2s and 16 threes / but this year: 8 long 2s and 75 threes" (shows 37 and 9, see top)
p_passers      # not in the script: 18 of 19 assisted, 10 by canada
