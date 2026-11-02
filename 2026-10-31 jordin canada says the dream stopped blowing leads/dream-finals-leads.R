# jordin canada after semis game 1 (10/4/2026): "last year we would get lots of leads and then we would
# lose them... this year we're able to finish games out"
# posted 10/31/2026, before finals game 7, so the data stops after game 6

library(tidyverse)
library(wehoop)

DATA_DIR <- "data"
dir.create(DATA_DIR, showWarnings = FALSE)
pbp_file <- file.path(DATA_DIR, "dream_pbp_2025_2026.csv")

DREAM <- "#E31837"
VALKS <- "#B38FCF"
GRAY  <- "#8A8A86"
INK   <- "#010101"
LIGHT <- "#E3E3E0"

# every dream game in 2025 and 2026 through finals game 6, regular season and playoffs, no all-star game or
# commissioner's cup final. scores are dream first
if (!file.exists(pbp_file)) {
  sched <- load_wnba_schedule(seasons = 2025:2026) |>
    transmute(game_id = as.integer(id), notes = notes_headline, sched_home = home_abbreviation,
              sched_home_score = as.integer(home_score), sched_away_score = as.integer(away_score))
  mascots <- espn_wnba_teams() |> transmute(opp = abbreviation, mascot = str_to_lower(mascot))

  load_wnba_pbp(seasons = 2025:2026) |>
    left_join(sched, by = "game_id") |>
    filter(season_type %in% c(2, 3), home_team_abbrev == "ATL" | away_team_abbrev == "ATL",
           game_date <= as.Date("2026-10-29"),
           !coalesce(notes, "") %in% c("AT&T WNBA All-Star Game", "WNBA Commissioner's Cup Championship")) |>
    # sort by game clock, not sequence: espn sometimes appends a play late (finals g2 has a q2 play after halftime)
    arrange(game_id, period_number, desc(clock_minutes * 60 + clock_seconds), sequence_number) |>
    transmute(game_id, season, season_type, date = game_date, notes,
              opp = if_else(home_team_abbrev == "ATL", away_team_abbrev, home_team_abbrev),
              period = period_number, sec_left = clock_minutes * 60 + clock_seconds,
              atl_score = if_else(home_team_abbrev == "ATL", home_score, away_score),
              opp_score = if_else(home_team_abbrev == "ATL", away_score, home_score),
              margin = atl_score - opp_score,
              sched_atl = if_else(sched_home == "ATL", sched_home_score, sched_away_score),
              sched_opp = if_else(sched_home == "ATL", sched_away_score, sched_home_score)) |>
    left_join(mascots, by = "opp") |>
    write_csv(pbp_file)
}

pbp <- read_csv(pbp_file, show_col_types = FALSE) |>
  mutate(minute = if_else(period <= 4, (period - 1) * 10, 40 + (period - 5) * 5) +
           (if_else(period <= 4, 600, 300) - sec_left) / 60)

# one row per game. the final score comes from the schedule when it has one (a few pbp finals are off)
games <- pbp |>
  group_by(game_id, season, season_type, date, notes, opp, mascot) |>
  summarise(max_lead = max(margin, 0),
            half = last(margin[period <= 2]),
            half_score = paste0(last(atl_score[period <= 2]), "-", last(opp_score[period <= 2])),
            q3 = last(margin[period <= 3]),
            atl_pts = coalesce(first(sched_atl), last(atl_score)),
            opp_pts = coalesce(first(sched_opp), last(opp_score)),
            .groups = "drop") |>
  mutate(final = atl_pts - opp_pts,
         score = paste0(atl_pts, "-", opp_pts),
         won = final > 0) |>
  arrange(date)

finals <- games |>
  filter(season == 2026, season_type == 3, opp == "GS") |>
  mutate(game = paste("game", row_number()))

playoffs <- games |>
  filter(season == 2026, season_type == 3) |>
  mutate(round = str_to_lower(str_remove(notes, "^WNBA ")) |>
           str_replace("first round", "1st round") |>
           str_replace(" - game ", " g"))


# ---- tables ----
# half = dream margin at halftime, q3 = going into the 4th, max_lead = biggest dream lead at any point

t_playoffs <- playoffs |> select(round, date, mascot, half_score, half, q3, max_lead, score, final)

t_records <- games |>
  group_by(season, season_type) |>
  summarise(games = n(), record = paste0(sum(won), "-", sum(!won)), .groups = "drop")

t_lost_led_half_2025 <- games |>
  filter(season == 2025, half > 0, !won) |>
  select(season_type, date, mascot, half_score, half, q3, max_lead, score, final)

t_2025_playoffs <- games |>
  filter(season == 2025, season_type == 3) |>
  select(date, mascot, half_score, half, q3, max_lead, score)

t_lost_led_half_2026 <- games |>
  filter(season == 2026, half > 0, !won) |>
  select(season_type, date, mascot, half_score, half, q3, max_lead, score, final)

t_blown <- bind_rows(t_lost_led_half_2025 |> mutate(season = 2025L),
                     t_lost_led_half_2026 |> mutate(season = 2026L)) |>
  relocate(season)

t_led_half <- games |>
  filter(half > 0) |>
  group_by(season, season_type) |>
  summarise(games = n(), record = paste0(sum(won), "-", sum(!won)), .groups = "drop")

t_playoffs_led_half <- playoffs |>
  filter(half > 0) |>
  select(round, date, mascot, half_score, half, score, final)

t_playoffs_trailed_half <- playoffs |>
  filter(half < 0) |>
  select(round, date, mascot, half_score, half, score, final)

# game 6: halftime, every time the lead hit 25, and every 4th quarter score with the lead at 10 or less
g6_scores <- pbp |>
  filter(date == as.Date("2026-10-29")) |>
  mutate(clock = sprintf("q%d %d:%02d", as.integer(period), as.integer(sec_left %/% 60), as.integer(sec_left %% 60)),
         score = paste0(atl_score, "-", opp_score))

t_g6 <- bind_rows(
  g6_scores |>
    filter(score != lag(score, default = "")) |>
    filter(margin >= 25 | (period == 4 & margin <= 10)),
  g6_scores |> filter(period == 2) |> slice_tail(n = 1) |> mutate(clock = "halftime")
) |>
  arrange(period, desc(sec_left)) |>
  select(clock, score, margin)

t_finals <- finals |> select(game, date, half_score, half, q3, max_lead, score, final)

t_finals_running <- finals |>
  mutate(total = cumsum(final), series = paste0(cumsum(won), "-", cumsum(!won))) |>
  select(game, score, final, total, series)

t_playoff_losses <- playoffs |>
  filter(!won) |>
  select(round, date, half_score, half, q3, max_lead, score)

t_finals_by_winner <- finals |>
  mutate(winner = if_else(won, "dream", "valkyries")) |>
  arrange(winner, desc(abs(final))) |>
  select(winner, game, score, margin = final)

# teams that outscored a playoff opponent and didn't win the series, 1997-2026, from her hoop stats box scores
t_series <- tribble(
  ~team,                             ~diff, ~series,                 ~dream,
  "2026 dream vs valkyries",            33, "3-3, game 7 sat",       TRUE,
  "2007 liberty vs shock",              18, "lost 1-2, 1st round",   FALSE,
  "2020 sun vs aces",                   14, "lost 2-3, semis",       FALSE,
  "2003 comets vs monarchs",            13, "lost 1-2, 1st round",   FALSE,
  "2008 sparks vs silver stars",        10, "lost 1-2, conf finals", FALSE,
  "2006 monarchs vs shock",              9, "lost 2-3, finals",      FALSE
)

# the 18: 2007 liberty vs the shock, liberty first, from her hoop stats box scores
t_2007_liberty <- tribble(
  ~game,    ~date,        ~where,    ~half_score, ~score,  ~margin, ~note,
  "game 1", "2007-08-24", "home",    "25-25",     "73-51",      22, "",
  "game 2", "2007-08-26", "detroit", "31-42",     "73-76",      -3, "",
  "game 3", "2007-08-28", "detroit", "33-23",     "70-71",      -1, "OT"
)

t_halftime_leader <- finals |>
  mutate(led_at_half = if_else(half > 0, "dream", "valkyries"),
         winner = if_else(won, "dream", "valkyries"),
         same = led_at_half == winner) |>
  select(game, half_score, led_at_half, score, winner, same)

# caption: record when leading by 10+, and every 10+ point lead lost
t_led_by_10 <- games |>
  filter(max_lead >= 10) |>
  group_by(season, season_type) |>
  summarise(games = n(), record = paste0(sum(won), "-", sum(!won)), .groups = "drop")

t_lost_led_by_10 <- games |>
  filter(max_lead >= 10, !won) |>
  select(season, season_type, date, mascot, max_lead, half, q3, score)


# ---- charts ----
theme_dream <- theme_minimal(base_size = 15) +
  theme(plot.title.position   = "plot",
        plot.caption.position = "plot",
        plot.title    = element_text(face = "bold", color = INK, size = 19),
        plot.subtitle = element_text(color = GRAY, size = 12, margin = margin(b = 14)),
        plot.caption  = element_text(color = GRAY, size = 11, hjust = 0, margin = margin(t = 14)),
        axis.text     = element_text(color = INK, size = 12),
        axis.title    = element_text(color = GRAY, size = 12),
        panel.grid.minor = element_blank(),
        plot.margin   = margin(18, 18, 14, 10))

win_fill <- scale_fill_manual(values = c(`TRUE` = DREAM, `FALSE` = VALKS), guide = "none")

p_playoff_results <- playoffs |>
  mutate(round = fct_inorder(round)) |>
  ggplot(aes(x = round, y = final, fill = won)) +
  geom_col(width = 0.65) +
  geom_hline(yintercept = 0, color = GRAY, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+d", final), vjust = if_else(final > 0, -0.5, 1.5)), color = INK, size = 4) +
  win_fill +
  scale_y_continuous(limits = c(-12, 30), breaks = seq(-10, 30, 10), labels = \(x) sprintf("%+d", x)) +
  labs(title = "dream 2026 playoff results",
       subtitle = "8-3. finals tied 3-3, game 7 saturday",
       caption = "chart: @wnbadata\ndata: wehoop",
       x = NULL, y = "dream final margin") +
  theme_dream +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

p_records <- games |>
  mutate(type = if_else(season_type == 2, "regular season", "playoffs"),
         type = factor(type, levels = c("regular season", "playoffs"))) |>
  group_by(season, type) |>
  summarise(wins = sum(won), losses = sum(!won), .groups = "drop") |>
  pivot_longer(c(wins, losses), names_to = "result", values_to = "n") |>
  mutate(result = factor(result, levels = c("wins", "losses"))) |>
  ggplot(aes(x = factor(season), y = n, fill = result)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = n), position = position_dodge(width = 0.75), vjust = -0.5, color = INK, size = 4.5) +
  facet_wrap(~ type, scales = "free_y") +
  scale_fill_manual(values = c(wins = DREAM, losses = LIGHT), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "dream record by season",
       caption = "chart: @wnbadata\ndata: wehoop",
       x = NULL, y = NULL) +
  theme_dream +
  theme(panel.grid = element_blank(), axis.text.y = element_blank(), legend.position = "top",
        strip.text = element_text(face = "bold", color = INK, size = 13))

p_blown <- t_blown |>
  group_by(season) |>
  mutate(panel = paste0(season, ": ", n(), " lost")) |>
  ungroup() |>
  mutate(game = paste(str_to_lower(format(date, "%b")), as.integer(format(date, "%d")), "vs", mascot,
                      if_else(season_type == 3, "(playoffs)", "")),
         game = fct_rev(fct_inorder(game)),
         label = paste0("up ", half, ", lost by ", -final)) |>
  ggplot(aes(x = half, y = game)) +
  geom_col(fill = DREAM, width = 0.6) +
  geom_text(aes(label = label), hjust = -0.08, color = INK, size = 4.4, fontface = "bold") +
  facet_grid(panel ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(limits = c(0, 22), breaks = c(0, 5, 10), expand = c(0, 0)) +
  labs(title = "dream games lost after leading at halftime",
       subtitle = "regular season and playoffs",
       caption = "chart: @wnbadata  |  data: wehoop",
       x = "halftime lead", y = NULL) +
  theme_dream +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, face = "bold", color = INK, size = 13, hjust = 1),
        panel.grid.major.y = element_blank(),
        panel.spacing.y = unit(1.2, "lines"))

p_playoff_half <- playoffs |>
  mutate(round = fct_rev(fct_inorder(round)),
         label = paste0(sprintf("%+d", half), " at half, ", if_else(won, "won", "lost"))) |>
  ggplot(aes(x = half, y = round, fill = won)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 0, color = GRAY, linewidth = 0.4) +
  geom_text(aes(label = label, hjust = if_else(half > 0, -0.08, 1.08)), color = INK, size = 4.2) +
  win_fill +
  scale_x_continuous(limits = c(-36, 36), breaks = seq(-20, 20, 10), labels = \(x) sprintf("%+d", x)) +
  labs(title = "dream halftime margin, 2026 playoffs",
       subtitle = "red = dream won",
       caption = "chart: @wnbadata\ndata: wehoop",
       x = "dream halftime margin", y = NULL) +
  theme_dream +
  theme(panel.grid.major.y = element_blank())

g6_path <- pbp |> filter(date == as.Date("2026-10-29"))
g6_marks <- tribble(
  ~minute, ~margin, ~label, ~hjust, ~vjust,
  20, finals$half[finals$game == "game 6"], "up 20 at half", 0.5, 2.2,
  g6_path$minute[g6_path$period == 4 & g6_path$margin == 8][1], 8, "up 8, 1:15 left", 1, 2
)

p_g6 <- ggplot(g6_path, aes(x = minute, y = margin)) +
  geom_hline(yintercept = 0, color = GRAY, linewidth = 0.4) +
  geom_step(color = DREAM, linewidth = 1) +
  geom_point(data = g6_marks, color = INK, size = 2.5) +
  geom_text(data = g6_marks, aes(label = label, hjust = hjust, vjust = vjust), color = INK, size = 4.5,
            fontface = "bold") +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40), labels = c("tip", "q2", "half", "q4", "end")) +
  scale_y_continuous(limits = c(-5, 30), breaks = seq(0, 25, 5), labels = \(x) sprintf("%+d", x)) +
  labs(title = "dream lead, finals game 6",
       subtitle = "dream 80, valkyries 69",
       caption = "chart: @wnbadata\ndata: wehoop",
       x = NULL, y = "dream lead") +
  theme_dream +
  theme(panel.grid.major.x = element_blank())

# labels go below the low points, right of game 2 and above the rest so they don't cover the line
p_running <- finals |>
  mutate(total = cumsum(final),
         series = paste0(cumsum(won), "-", cumsum(!won)),
         lab_vjust = case_when(game %in% c("game 1", "game 5") ~ 1.5, game == "game 2" ~ 1.1, TRUE ~ -0.5),
         lab_hjust = if_else(game == "game 2", 0, 0.5),
         lab_x = row_number() + if_else(game == "game 2", 0.1, 0)) |>
  ggplot(aes(x = game, y = total, group = 1)) +
  geom_hline(yintercept = 0, color = GRAY, linewidth = 0.4) +
  geom_line(color = DREAM, linewidth = 1) +
  geom_point(aes(fill = won), shape = 21, size = 4, color = "white", stroke = 1) +
  geom_text(aes(x = lab_x, label = paste0(sprintf("%+d", total), "\nseries ", series), vjust = lab_vjust,
                hjust = lab_hjust),
            color = INK, size = 3.8, lineheight = 0.9) +
  win_fill +
  scale_y_continuous(limits = c(-16, 46), breaks = seq(0, 40, 10), labels = \(x) sprintf("%+d", x)) +
  labs(title = "dream point differential in the finals, running total",
       subtitle = "label: running total and series score, dream first",
       caption = "chart: @wnbadata\ndata: wehoop",
       x = NULL, y = "dream total point differential") +
  theme_dream +
  theme(panel.grid.major.x = element_blank())

p_max_lead <- playoffs |>
  mutate(round = fct_rev(fct_inorder(round)),
         label = paste0("up ", max_lead, ", ", if_else(won, "won by ", "lost by "), abs(final))) |>
  ggplot(aes(x = max_lead, y = round, fill = won)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 10, color = GRAY, linewidth = 0.4, linetype = "dashed") +
  geom_text(aes(label = label), hjust = -0.08, color = INK, size = 4.2) +
  win_fill +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 25, 5), expand = c(0, 0)) +
  labs(title = "dream biggest lead in each 2026 playoff game",
       subtitle = "red = dream won. dashed line = 10 point lead",
       caption = "chart: @wnbadata\ndata: wehoop",
       x = "biggest dream lead", y = NULL) +
  theme_dream +
  theme(panel.grid.major.y = element_blank())

p_finals <- ggplot(finals, aes(x = game, y = final, fill = won)) +
  geom_col(width = 0.65) +
  geom_hline(yintercept = 0, color = GRAY, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+d", final), vjust = if_else(final > 0, -0.5, 1.5)),
            color = INK, size = 5, fontface = "bold") +
  win_fill +
  scale_y_continuous(limits = c(-12, 30), breaks = seq(-10, 30, 10), labels = \(x) sprintf("%+d", x)) +
  labs(title = "dream point differential by finals game",
       subtitle = "series tied 3-3",
       caption = "chart: @wnbadata\ndata: wehoop",
       x = NULL, y = "dream final margin") +
  theme_dream +
  theme(panel.grid.major.x = element_blank())

p_history <- t_series |>
  mutate(team = fct_rev(fct_inorder(team))) |>
  ggplot(aes(x = diff, y = team, fill = dream)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0("+", diff, "  (", series, ")")), hjust = -0.05, color = INK, size = 4.2) +
  scale_fill_manual(values = c(`TRUE` = DREAM, `FALSE` = LIGHT), guide = "none") +
  scale_x_continuous(limits = c(0, 64), breaks = seq(0, 30, 10), expand = c(0, 0)) +
  labs(title = "playoff series point differential",
       subtitle = "teams that outscored their opponent and didn't win the series, 1997-2026",
       caption = "chart: @wnbadata\ndata: her hoop stats",
       x = "point differential in the series", y = NULL) +
  theme_dream +
  theme(panel.grid.major.y = element_blank())

p_halftime <- finals |>
  pivot_longer(c(half, final), names_to = "when", values_to = "margin") |>
  mutate(when = factor(when, levels = c("half", "final"), labels = c("halftime", "final"))) |>
  ggplot(aes(x = game, y = margin, fill = won, alpha = when)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_hline(yintercept = 0, color = GRAY, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+d", margin), vjust = if_else(margin > 0, -0.5, 1.5), group = when),
            position = position_dodge(width = 0.75), color = INK, size = 3.8, alpha = 1) +
  win_fill +
  scale_alpha_manual(values = c(halftime = 0.45, final = 1), name = NULL) +
  scale_y_continuous(limits = c(-20, 30), breaks = seq(-20, 30, 10), labels = \(x) sprintf("%+d", x)) +
  labs(title = "dream halftime and final margin by finals game",
       subtitle = "the team leading at halftime has won all 6 games",
       caption = "chart: @wnbadata\ndata: wehoop",
       x = NULL, y = "dream margin") +
  theme_dream +
  theme(panel.grid.major.x = element_blank(), legend.position = "top")


# ---- index, in script order ----
p_playoff_results
p_records
p_blown
p_playoff_half
p_g6
p_running
p_max_lead
p_finals
p_history
p_halftime

t_playoffs
t_records
t_lost_led_half_2025
t_2025_playoffs
t_lost_led_half_2026
t_blown
t_led_half
t_playoffs_led_half
t_playoffs_trailed_half
t_g6
t_finals
t_finals_running
t_playoff_losses
t_finals_by_winner
t_series
t_2007_liberty
t_halftime_leader
t_led_by_10
t_lost_led_by_10
