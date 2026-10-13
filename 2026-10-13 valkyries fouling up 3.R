# the valkyries changed their mind about fouling up 3 (sequel to the 7/1/2026 video)
# nakase 9/30: "i didn't want to foul." 10/7: "now we foul, we trust our free throw shooters."
#
# league count since 2021: games where a team led by exactly 3 in the last 24 seconds of the 4th.
# fouled = a non-shooting foul by the team up 3 while still up 3 (fouled before the shot). the pbp take foul
# label misses some (hayes 10/2, gabby 10/7 are plain personal fouls), so any personal or take foul counts.
# wehoop pbp, clean games only (pbp final matches the schedule).

library(tidyverse)
library(wehoop)

VALKS <- "#6F4BB2"
GRAY  <- "#8A8A86"
INK   <- "#010101"

clock_secs <- function(x) {
  as.numeric(str_extract(x, "^\\d+")) * 60 + as.numeric(str_extract(x, "(?<=:)[\\d.]+"))
}

pbp <- load_wnba_pbp(seasons = 2021:most_recent_wnba_season()) |>
  filter(season_type %in% c(2, 3),
         !home_team_abbrev %in% c("EAST", "WEST"),                     # all-star games
         !str_detect(home_team_name, "^Team "), !str_detect(away_team_name, "^Team ")) |>
  mutate(seq = as.numeric(sequence_number),
         secs_left = clock_secs(clock_display_value),
         margin = home_score - away_score) |>
  arrange(game_id, seq)

# a season that fails to download drops silently and shrinks every count
stopifnot(n_distinct(pbp$season) == length(2021:most_recent_wnba_season()))

sched <- load_wnba_schedule(seasons = 2021:most_recent_wnba_season()) |>
  transmute(game_id = as.integer(id), sched_home = as.integer(home_score), sched_away = as.integer(away_score))

games <- pbp |>
  group_by(game_id) |>
  summarise(season = first(season), season_type = first(season_type), date = first(game_date),
            home = first(home_team_abbrev), away = first(away_team_abbrev),
            home_id = first(home_team_id), away_id = first(away_team_id),
            periods = max(period_number), final_home = last(home_score), final_away = last(away_score),
            .groups = "drop") |>
  left_join(sched, by = "game_id") |>
  mutate(clean = coalesce(final_home == sched_home & final_away == sched_away, FALSE))

# ---- the three valkyries endings (for the pbp screenshots) ----

valks_endings <- pbp |>
  filter(game_id %in% c(401918020, 401918294, 401918298), period_number == 4, secs_left <= 16) |>
  arrange(game_id, desc(secs_left), seq) |>
  transmute(date = game_date, clock = clock_display_value, away = away_score, home = home_score, text)

# ---- league count ----

# skip moments between free throws (after 1 of 2, 1 of 3, 2 of 3): up 3 only until the next free throw
# isn't a real up-3 situation
late <- pbp |>
  filter(period_number == 4, secs_left <= 24) |>
  mutate(ft = str_match(text, "free throw.*?(\\d) of (\\d)"),
         mid_ft = coalesce(as.integer(ft[, 2]) < as.integer(ft[, 3]), FALSE)) |>
  select(-ft)

# the first moment in the last 24 seconds where someone leads by exactly 3
up3 <- late |>
  filter(abs(margin) == 3, !mid_ft) |>
  group_by(game_id) |>
  slice_min(seq, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(game_id, leader = if_else(margin > 0, "home", "away"), first_up3 = seq)

# the first non-shooting foul by the team up 3, while still up 3
first_foul <- late |>
  inner_join(up3, by = "game_id") |>
  left_join(games |> select(game_id, home_id, away_id), by = "game_id") |>
  mutate(leader_id = if_else(leader == "home", home_id, away_id),
         lead = if_else(leader == "home", margin, -margin)) |>
  filter(seq >= first_up3, team_id == leader_id, lead == 3,
         type_text %in% c("Personal Foul", "Personal Take Foul", "Transition Take Foul")) |>
  group_by(game_id) |>
  slice_min(seq, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(game_id, foul_clock = clock_display_value, foul_text = text)

up3_games <- up3 |>
  left_join(games, by = "game_id") |>
  left_join(first_foul, by = "game_id") |>
  filter(clean) |>
  mutate(fouled = !is.na(foul_clock),
         leader_won = if_else(leader == "home", final_home > final_away, final_away > final_home),
         went_ot = periods > 4,
         outcome = case_when(went_ot ~ "went to OT", leader_won ~ "won in regulation", TRUE ~ "lost in regulation"))

# 245 games. fouled 14, won 13, 7 of them in 2026. didn't foul won 223 of 231 (97%)
fouled_vs_not <- up3_games |>
  group_by(fouled) |>
  summarise(games = n(), won = sum(leader_won), lost = sum(!leader_won), in_2026 = sum(season == 2026),
            win_pct = round(100 * mean(leader_won), 1), .groups = "drop")

# the 14. the one loss is liberty at sparks 6/21 (stewart's two accidental fouls)
fouled_games <- up3_games |>
  filter(fouled) |>
  arrange(date) |>
  transmute(date, game = paste(away, "at", home), up_3 = if_else(leader == "home", home, away),
            foul_clock, foul_text, outcome)

by_season <- up3_games |>
  group_by(season) |>
  summarise(games = n(), fouled = sum(fouled), .groups = "drop")

# ---- charts ----

wnba_win <- up3_games |>
  mutate(choice = if_else(fouled, "fouled up 3", "didn't foul")) |>
  group_by(choice) |>
  summarise(won = sum(leader_won), games = n(), .groups = "drop") |>
  mutate(win_pct = 100 * won / games,
         label = paste0(won, " of ", games, " (", round(win_pct), "%)"),
         choice = fct_relevel(choice, "fouled up 3"))

p_wnba_win <- ggplot(wnba_win, aes(x = win_pct, y = fct_rev(choice), fill = choice == "fouled up 3")) +
  geom_col(width = 0.6) +
  geom_text(aes(label = label), hjust = 1.08, colour = "white", size = 6, fontface = "bold") +
  scale_fill_manual(values = c(`TRUE` = VALKS, `FALSE` = GRAY), guide = "none") +
  scale_x_continuous(limits = c(0, 100), breaks = c(0, 50, 100), labels = \(x) paste0(x, "%"),
                     expand = c(0, 0)) +
  labs(title = "wnba teams up 3 in the last 24 seconds",
       subtitle = "win %, since 2021",
       caption = "regular season + playoffs, through the 2026 semifinals  |  chart: @wnbadata\ndata: wehoop",
       x = "won the game", y = NULL) +
  theme_minimal(base_size = 15) +
  theme(plot.title.position = "plot", plot.caption.position = "plot",
        plot.title    = element_text(face = "bold", colour = INK, size = 19),
        plot.subtitle = element_text(colour = GRAY, size = 12, margin = margin(b = 14)),
        plot.caption  = element_text(colour = GRAY, size = 11, hjust = 0, margin = margin(t = 14)),
        axis.text.y   = element_text(face = "bold", colour = INK, size = 14),
        axis.text.x   = element_text(colour = INK, size = 12),
        axis.title.x  = element_text(colour = GRAY, size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin   = margin(18, 18, 14, 10))

p_how_often <- ggplot(by_season, aes(x = factor(season))) +
  geom_col(aes(y = games), fill = "#D6D2DE", width = 0.7) +
  geom_col(aes(y = fouled), fill = VALKS, width = 0.7) +
  geom_text(aes(y = games, label = paste(fouled, "of", games)), vjust = -0.5, colour = INK,
            size = 5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 78), expand = c(0, 0)) +
  labs(title = "wnba teams almost never foul up 3",
       subtitle = "gray: games with a team up 3 in the last 24 seconds\npurple: the team up 3 fouled before the other team could shoot",
       caption = "regular season + playoffs, 2026 through the semifinals  |  chart: @wnbadata\ndata: wehoop",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 15) +
  theme(plot.title.position = "plot", plot.caption.position = "plot",
        plot.title    = element_text(face = "bold", colour = INK, size = 19),
        plot.subtitle = element_text(colour = GRAY, size = 12, margin = margin(b = 14)),
        plot.caption  = element_text(colour = GRAY, size = 11, hjust = 0, margin = margin(t = 14)),
        axis.text.x   = element_text(face = "bold", colour = INK, size = 13),
        axis.text.y   = element_blank(),
        panel.grid    = element_blank(),
        plot.margin   = margin(18, 18, 14, 10))

# waffle, like the july video: 1 square = 1 game. same square size for both groups, so the fouled row is
# small on purpose
PER_ROW <- 25
GAP     <- 3.6   # rows between the two blocks, room for the second header

waffle <- up3_games |>
  mutate(choice = factor(if_else(fouled, "fouled up 3", "didn't foul"), levels = c("fouled up 3", "didn't foul")),
         result = factor(case_when(went_ot ~ "OT", leader_won ~ "win", TRUE ~ "loss"),
                         levels = c("win", "OT", "loss"))) |>
  arrange(choice, result) |>
  group_by(choice) |>
  mutate(i = row_number() - 1, x = i %% PER_ROW, row = i %/% PER_ROW) |>
  ungroup() |>
  mutate(y = if_else(choice == "fouled up 3", -row, -(row + GAP)))

waffle_headers <- waffle |>
  group_by(choice) |>
  summarise(y = max(y) + 1.5,
            label = paste0(first(choice), ": won ", sum(leader_won), " of ", n(), "\n",
                           sum(result == "win"), " in regulation, ",
                           sum(result == "OT" & leader_won), " of ", sum(result == "OT"), " in OT"),
            .groups = "drop")

p_waffle <- ggplot(waffle, aes(x = x, y = y)) +
  geom_tile(aes(fill = result), width = 0.86, height = 0.86) +
  geom_text(data = waffle_headers, aes(x = (PER_ROW - 1) / 2, label = label), colour = INK,
            size = 5, fontface = "bold", lineheight = 0.95) +
  scale_fill_manual(values = c(win = "#5B95D6", OT = "#EDB443", loss = "#C8312B"), name = NULL) +
  coord_equal(clip = "off") +
  labs(title = "wnba teams up 3 in the last 24 seconds",
       subtitle = "1 square = 1 game, since 2021",
       caption = "fouled = fouled before the other team could shoot. regular season + playoffs, through the 2026 semifinals\nchart: @wnbadata  |  data: wehoop") +
  theme_void(base_size = 15) +
  theme(plot.title    = element_text(face = "bold", colour = INK, size = 19, hjust = 0.5),
        plot.subtitle = element_text(colour = GRAY, size = 13, hjust = 0.5, margin = margin(t = 4, b = 22)),
        plot.caption  = element_text(colour = GRAY, size = 9, hjust = 0.5, margin = margin(t = 14)),
        legend.position = "bottom",
        legend.text   = element_text(colour = INK, size = 13),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin   = margin(18, 18, 14, 18))

# ---- tables ----
print(valks_endings, n = Inf)   # last 16 seconds of 9/30, 10/2 and 10/7
fouled_vs_not                   # fouled 13-1 (7 in 2026), didn't 223-8
print(fouled_games, n = Inf)    # the 14
by_season                       # games with a team up 3 and how many fouled, per season

# ---- charts (in script order) ----
p_how_often    # "245 games... only 14 fouled before the shot... half this year"
p_wnba_win     # "won 13 of 14... didn't foul won 97%"
p_waffle       # same beat, july-video style
