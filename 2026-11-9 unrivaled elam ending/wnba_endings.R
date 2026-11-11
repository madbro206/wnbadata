# how WNBA games end (2026-11-9 unrivaled elam ending video)
#
# every number the charts use for the WNBA side comes from this script:
#   1. game winners: only 11 of 353 2026 games were won on a shot in the last 5 seconds
#   2. late fouling: the losing team fouled in the last minute of 46% of games, and won 17 of 1,018
#   3. how slow the end is: the last 2 minutes of close 2026 games took 13.5 real minutes,
#      and a whole 4th quarter took 32 minutes
#
# data: wehoop play by play, 2018-2026, regular season (incl. the commissioner's cup final) + playoffs.
# all-star games are dropped.
# heads up: wehoop still lists 2026 finals game 7 (ATL @ GS, 10/31) as scheduled with no play by play,
# so 2026 has 353 games here instead of 354. GS led the whole last 75 seconds of that game, so the 11 doesn't change.

library(dplyr)
library(tidyr)
library(lubridate)
library(wehoop)

pbp <- load_wnba_pbp(seasons = 2018:2026) |>
  filter(season_type %in% c(2, 3))

# all-star games are season_type 2 too, so keep only teams that played 10+ games that season
team_games <- pbp |>
  distinct(season, game_id, home_team_id, away_team_id) |>
  pivot_longer(c(home_team_id, away_team_id), values_to = "team_id") |>
  count(season, team_id) |>
  filter(n >= 10)

pbp <- pbp |>
  semi_join(team_games, by = c("season", "home_team_id" = "team_id")) |>
  semi_join(team_games, by = c("season", "away_team_id" = "team_id")) |>
  mutate(sec_left = as.numeric(clock_minutes) * 60 + as.numeric(clock_seconds))

# one row per game with the final score
games <- pbp |>
  group_by(game_id) |>
  summarise(season = first(season), date = first(game_date),
            home = first(home_team_abbrev), away = first(away_team_abbrev),
            home_id = first(home_team_id), away_id = first(away_team_id),
            home_score = max(home_score), away_score = max(away_score),
            last_q = max(qtr), .groups = "drop") |>
  filter(home_score != away_score) |>
  mutate(winner_id = if_else(home_score > away_score, home_id, away_id),
         margin = abs(home_score - away_score))

# every play, with the score from the eventual winner's side
plays <- pbp |>
  inner_join(select(games, game_id, winner_id, home_id, last_q), by = "game_id") |>
  arrange(game_id, game_play_number) |>
  mutate(winner_margin = if_else(winner_id == home_id, home_score - away_score, away_score - home_score))


# ---- 1. GAME WINNERS ----
# the game winner is the last time the winner went from tied/behind to ahead
game_winner <- plays |>
  group_by(game_id) |>
  mutate(prev = lag(winner_margin, default = 0)) |>
  filter(scoring_play, prev <= 0, winner_margin > 0) |>
  slice_tail(n = 1) |>
  ungroup() |>
  transmute(game_id, gw_q = qtr, gw_sec = sec_left, gw_text = text,
            gw_final_q = qtr == last_q,
            gw_ft = grepl("free throw", tolower(type_text)))

games <- left_join(games, game_winner, by = "game_id")

game_winners_by_season <- games |>
  group_by(season) |>
  summarise(games = n(),
            gw_last_24_sec = sum(gw_final_q & gw_sec <= 24 & !gw_ft, na.rm = TRUE),
            gw_last_5_sec  = sum(gw_final_q & gw_sec <= 5 & !gw_ft, na.rm = TRUE),
            gw_buzzer      = sum(gw_final_q & gw_sec <= 0.5 & !gw_ft, na.rm = TRUE))

# the 11 shots
gw_2026 <- games |>
  filter(season == 2026, gw_final_q, gw_sec <= 5, !gw_ft) |>
  select(date, away, home, away_score, home_score, gw_sec, gw_text)


# ---- 2. LATE FOULING ----
# a non-shooting foul by the team that was behind, in the last 60 seconds of the final period
foul_types <- c("Personal Foul", "PersonalFoul", "Personal Take Foul", "Away from Play Foul",
                "Away From Play Foul", "InBound Foul", "Inbound Foul", "Transition Take Foul")

# for fouls, team_id is the team that committed the foul
late_fouls <- plays |>
  filter(qtr == last_q, sec_left <= 60, sec_left > 0, type_text %in% foul_types) |>
  mutate(fouler_margin = if_else(team_id == home_id, home_score - away_score, away_score - home_score)) |>
  filter(fouler_margin < 0)

foul_games <- late_fouls |>
  group_by(game_id) |>
  summarise(fouls = n(), fouler = first(team_id), deficit = -first(fouler_margin), .groups = "drop") |>
  left_join(select(games, game_id, season, winner_id), by = "game_id") |>
  mutate(fouler_won = fouler == winner_id)

fouling <- games |>
  count(season, name = "games") |>
  left_join(foul_games |> group_by(season) |> summarise(foul_games = n(), fouler_won = sum(fouler_won)),
            by = "season")

fouling_total <- fouling |>
  summarise(games = sum(games), foul_games = sum(foul_games), fouler_won = sum(fouler_won)) |>
  mutate(pct_fouled = round(100 * foul_games / games), pct_won = round(100 * fouler_won / foul_games, 1))

# every game by final margin, split by whether there was late fouling (p_fouled_margin)
fouling_by_margin <- games |>
  mutate(fouled = game_id %in% foul_games$game_id) |>
  count(margin, fouled) |>
  pivot_wider(names_from = fouled, values_from = n, values_fill = 0) |>
  rename(fouled = `TRUE`, none = `FALSE`)


# ---- 3. HOW SLOW THE END IS ----
# real time from wallclock. games with a 10+ minute hole in the timestamps are dropped
# regulation games only
plays_wc <- plays |>
  filter(last_q == 4) |>
  mutate(wc = ymd_hms(wallclock, quiet = TRUE))

real_time <- function(q, from_sec) {
  plays_wc |>
    filter(qtr == q) |>
    group_by(game_id) |>
    summarise(start = suppressWarnings(max(wc[sec_left >= from_sec], na.rm = TRUE)),
              clock_start = suppressWarnings(min(sec_left[sec_left >= from_sec])),
              end = suppressWarnings(max(wc[type_text == "End Period"], na.rm = TRUE)),
              max_gap = suppressWarnings(max(diff(sort(as.numeric(wc))), na.rm = TRUE)),
              .groups = "drop") |>
    filter(is.finite(start), is.finite(end), max_gap < 600) |>
    mutate(real_min = as.numeric(difftime(end, start, units = "mins")),
           clock_min = clock_start / 60) |>
    filter(real_min > 0, real_min / clock_min < 20)
}

# the last 2 minutes on the clock (starting from the last play at 2:00 to 2:30 left)
last_2 <- real_time(4, 120) |>
  filter(clock_min >= 2, clock_min <= 2.5) |>
  left_join(select(games, game_id, season, margin), by = "game_id")

last_2_by_margin <- last_2 |>
  filter(season == 2026) |>
  mutate(final_margin = cut(margin, c(0, 3, 6, 10, Inf), labels = c("1 to 3", "4 to 6", "7 to 10", "11+"))) |>
  group_by(final_margin) |>
  summarise(games = n(), median_real_min = round(median(real_min), 1))

# the whole 4th quarter, from the first play of the 4th to the end of the 4th (overtime games count too)
# keeps games whose first logged 4th quarter play is within 15 seconds of 10:00, and drops anything
# under 15 or over 60 real minutes or with a 15+ minute hole in the timestamps
q4 <- plays |>
  filter(qtr == 4) |>
  mutate(wc = ymd_hms(wallclock, quiet = TRUE)) |>
  group_by(game_id, season) |>
  summarise(start = suppressWarnings(min(wc, na.rm = TRUE)),
            end = suppressWarnings(max(wc[type_text == "End Period"], na.rm = TRUE)),
            max_gap = suppressWarnings(max(diff(sort(as.numeric(wc))), na.rm = TRUE)),
            first_sec = max(sec_left), .groups = "drop") |>
  filter(is.finite(start), is.finite(end), first_sec >= 585) |>
  mutate(real_min = as.numeric(difftime(end, start, units = "mins"))) |>
  filter(real_min >= 15, real_min <= 60, max_gap < 900)

q4_by_season <- q4 |>
  group_by(season) |>
  summarise(games = n(), avg_real_min = round(mean(real_min), 1),
            min = round(min(real_min)), max = round(max(real_min)))


# ---- results ----
as.data.frame(game_winners_by_season)
as.data.frame(gw_2026)
as.data.frame(fouling)
as.data.frame(fouling_total)
as.data.frame(last_2_by_margin)
as.data.frame(q4_by_season)
