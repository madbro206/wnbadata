# does the team that scores first in overtime usually win?
#
# dewanna bonner told the dream in the huddle before OT of semis game 2 (10/7/2026, dream 101
# liberty 98) that "the team that usually scores first in overtime wins the game."
#
# wehoop play-by-play only, 2002 to now. first overtime period only: whoever puts the first
# points on the board in OT1, did they win the game?
#
# notes:
# - the wnba played two halves through 2005, so OT1 is period 3 before 2006 and period 5 after.
# - there is no play-by-play anywhere before 2002 (63 OT games 1997-2001 can't be checked).
# - older wehoop pbp is messy (scores that skip, OTs that don't start tied), so a game only
#   counts if the pbp is "clean": the last pbp score matches the schedule's final, OT1 starts
#   tied, and the side whose score went up matches the team_id on that play.
# - checked 10/10 against her hoop stats pbp for 2018+: all 77 clean games matched game by game.
# - needs the arrow package installed or the pre-2024 seasons come back all NA.

library(tidyverse)
library(wehoop)

seasons <- 2002:most_recent_wnba_season()

# ---- data ----

pbp <- load_wnba_pbp(seasons = seasons) |>
  filter(season_type %in% c(2, 3),                                   # regular season + playoffs
         !home_team_abbrev %in% c("EAST", "WEST"),                    # 2014 all-star game
         !str_detect(home_team_name, "^Team "), !str_detect(away_team_name, "^Team ")) |>
  mutate(reg_periods = if_else(season <= 2005, 2L, 4L),
         seq = as.numeric(sequence_number)) |>
  arrange(game_id, seq) |>
  group_by(game_id) |>
  mutate(home_diff = home_score - lag(home_score, default = 0),
         away_diff = away_score - lag(away_score, default = 0)) |>
  ungroup()

sched <- load_wnba_schedule(seasons = seasons) |>
  transmute(game_id = as.integer(id), sched_home = as.integer(home_score), sched_away = as.integer(away_score))

ot_games <- pbp |>
  group_by(game_id) |>
  summarise(season = first(season), season_type = first(season_type), game_date = first(game_date),
            away = first(away_team_abbrev), home = first(home_team_abbrev),
            away_team_id = first(away_team_id), home_team_id = first(home_team_id),
            away_city = first(away_team_name), home_city = first(home_team_name),   # abbrevs change (PHO/PHX)
            reg_periods = first(reg_periods), periods = max(period_number),
            final_home = last(home_score), final_away = last(away_score), .groups = "drop") |>
  filter(periods > reg_periods)

# first play in OT1 that changes the score
first_score <- pbp |>
  semi_join(ot_games, by = "game_id") |>
  filter(period_number == reg_periods + 1, home_diff > 0 | away_diff > 0) |>
  group_by(game_id) |>
  slice(1) |>
  ungroup() |>
  transmute(game_id,
            first_side = if_else(home_diff > 0, "home", "away"),
            team_id_side = case_when(team_id == home_team_id ~ "home", team_id == away_team_id ~ "away"),
            tied_at_start = home_score - home_diff == away_score - away_diff,
            first_pts = home_diff + away_diff,
            first_kind = case_when(str_detect(str_to_lower(text), "free throw") ~ "free throw",
                                   first_pts == 3 ~ "three",
                                   TRUE ~ "two"),
            first_play = text)

ot <- ot_games |>
  left_join(first_score, by = "game_id") |>
  left_join(sched, by = "game_id") |>
  mutate(clean = final_home == sched_home & final_away == sched_away &
                 tied_at_start & first_side == team_id_side,
         clean = coalesce(clean, FALSE),
         home_won = final_home > final_away,
         first_won = (first_side == "home") == home_won,
         era = cut(season, c(2001, 2005, 2012, 2017, 2100),
                   labels = c("2002-2005", "2006-2012", "2013-2017", "2018-now")),
         playoffs = if_else(season_type == 3, "playoffs", "regular season"))

cat("OT games in wehoop pbp:", nrow(ot), "| clean:", sum(ot$clean), "| dropped:", sum(!ot$clean), "\n")
ot |> count(era, clean) |> pivot_wider(names_from = clean, values_from = n, names_prefix = "clean_") |> print()

games <- ot |> filter(clean)

# ---- results ----

first_won_table <- function(df, ...) {
  df |>
    group_by(...) |>
    summarise(games = n(), first_scorer_won = sum(first_won),
              pct = round(100 * mean(first_won), 1),
              p_vs_coin_flip = signif(binom.test(first_scorer_won, games)$p.value, 2),
              .groups = "drop")
}

first_won_table(games)
first_won_table(games, era)
first_won_table(games, playoffs)
first_won_table(games, first_kind)

# ---- dewanna bonner's overtime games ----
# every OT game she played in, whoever scored first. "played in" = at least one play in the pbp
# with her name on it. her team that night = the team_id on most of her own plays (phoenix
# 2009-2019, connecticut 2020-2024, indiana then phoenix in 2025, phoenix then atlanta in 2026).
# that's 38 games and a 19-19 record, which matches her hoop stats and stathead.
# 3 of the 38 fail the clean check, so who scored first is filled in by hand:
#   2009-06-13 sac at phx: wehoop scores are broken, but le'coe willingham (phx) made the first OT basket
#   2020-09-09 phx at conn: her hoop stats pbp, phoenix scored first
#   2026-06-13 la at phx:   her hoop stats pbp, los angeles scored first
# results for all 38 come from the schedule, since the pbp final is off in 2 of the 3.

bonner_fixes <- tribble(
  ~game_id,   ~fixed_side, ~fixed_play,
  290613011,  "home",      "Le'Coe Willingham made Layup.",
  401230862,  "away",      "(her hoop stats pbp) phoenix scored first",
  401856989,  "away",      "(her hoop stats pbp) los angeles scored first"
)

bonner_plays <- pbp |>
  semi_join(ot, by = "game_id") |>
  filter(if_any(c(athlete_name_1, athlete_name_2, athlete_name_3), \(x) x %in% "DeWanna Bonner"))

bonner_team <- bonner_plays |>
  filter(athlete_name_1 %in% "DeWanna Bonner") |>
  count(game_id, team_id, sort = TRUE) |>
  group_by(game_id) |>
  slice(1) |>
  ungroup() |>
  select(game_id, bonner_team_id = team_id)

bonner_in_ot <- bonner_plays |>
  filter(period_number > reg_periods) |>
  distinct(game_id) |>
  mutate(played_in_ot = TRUE)

bonner <- ot |>
  inner_join(bonner_team, by = "game_id") |>
  left_join(bonner_in_ot, by = "game_id") |>
  left_join(bonner_fixes, by = "game_id") |>
  mutate(first_side = if_else(clean, first_side, fixed_side),
         first_play = if_else(clean, first_play, fixed_play),
         final_home = sched_home, final_away = sched_away, home_won = final_home > final_away,
         played_in_ot = coalesce(played_in_ot, FALSE),
         bonner_side = if_else(bonner_team_id == home_team_id, "home", "away"),
         team = if_else(bonner_side == "home", home_city, away_city),
         opponent = if_else(bonner_side == "home", away_city, home_city),
         score = if_else(bonner_side == "home", paste0(final_home, "-", final_away), paste0(final_away, "-", final_home)),
         won = (bonner_side == "home") == home_won,
         her_team_scored_first = first_side == bonner_side,
         she_scored_first = str_detect(first_play, "^DeWanna Bonner"))

stopifnot(!anyNA(bonner$first_side))   # every non-clean bonner game needs a hand fix

record <- function(df, ...) {
  df |>
    group_by(...) |>
    summarise(games = n(), record = paste0(sum(won), "-", sum(!won)),
              first_scorer_won = sum(first_won), .groups = "drop")
}

# her teams in overtime, all together
record(bonner)

# split by who scored first in OT. the "first_scorer_won" column is the bonner rule
record(bonner, her_team_scored_first)

# by team
record(bonner, team, her_team_scored_first)

# every game
bonner |>
  arrange(game_date) |>
  select(game_date, team, opponent, playoffs, score, won, her_team_scored_first, she_scored_first,
         played_in_ot, first_play) |>
  print(n = Inf, width = Inf)

# ---- last overtime instead of the first ----
# a different question: who scored first in the LAST overtime period (the one that decided it).
# single OT games are the same either way, so only the multi-OT games change. the first OT is
# the right reading of bonner's claim, since she said it before overtime started.
last_ot_first <- pbp |>
  semi_join(games |> filter(periods - reg_periods > 1), by = "game_id") |>
  group_by(game_id) |>
  filter(period_number == max(period_number), home_diff > 0 | away_diff > 0) |>
  slice_min(seq, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(game_id, last_side = if_else(home_diff > 0, "home", "away"))

games_last <- games |>
  left_join(last_ot_first, by = "game_id") |>
  mutate(multi_ot = periods - reg_periods > 1,
         last_won = if_else(multi_ot, (last_side == "home") == home_won, first_won))

games_last |>
  group_by(multi_ot) |>
  summarise(games = n(), first_ot_won = sum(first_won), last_ot_won = sum(last_won), .groups = "drop")

c(games = nrow(games_last), last_ot_first_scorer_won = sum(games_last$last_won))

# ---- comparison: tied with 5 minutes left in regulation ----
# is scoring first in OT worth more than breaking a tie with 5 minutes left in regulation?
# (morgulev et al. 2020 asked the same thing for the nba.) both are 5 minutes, tied, same teams
# and same strength gap, so if the numbers match, scoring first in OT is just a lead.
# "tied at 5:00" = the score after the last play with 5:00 or more left in the final regulation
# period (the 2nd half before 2006, the 4th quarter after). same clean check as the OT games.

clock_secs <- function(x) {
  as.numeric(str_extract(x, "^\\d+")) * 60 + as.numeric(str_extract(x, "(?<=:)[\\d.]+"))
}

final_period <- pbp |>
  filter(period_number == reg_periods) |>
  mutate(secs_left = clock_secs(clock_display_value))

tied_at_five <- final_period |>
  filter(secs_left >= 300) |>
  group_by(game_id) |>
  slice_max(seq, n = 1, with_ties = FALSE) |>
  ungroup() |>
  filter(home_score == away_score) |>
  select(game_id, tie_seq = seq, tie_score = home_score)

next_score <- final_period |>
  inner_join(tied_at_five, by = "game_id") |>
  filter(seq > tie_seq, home_diff > 0 | away_diff > 0) |>
  group_by(game_id) |>
  slice_min(seq, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(game_id,
            next_side = if_else(home_diff > 0, "home", "away"),
            team_id_side = case_when(team_id == home_team_id ~ "home", team_id == away_team_id ~ "away"),
            next_pts = home_diff + away_diff,
            next_kind = case_when(str_detect(str_to_lower(text), "free throw") ~ "free throw",
                                  next_pts == 3 ~ "three",
                                  TRUE ~ "two"),
            secs_left)

tied <- pbp |>
  semi_join(tied_at_five, by = "game_id") |>
  group_by(game_id) |>
  summarise(season = first(season), season_type = first(season_type),
            reg_periods = first(reg_periods), periods = max(period_number),
            final_home = last(home_score), final_away = last(away_score), .groups = "drop") |>
  inner_join(next_score, by = "game_id") |>
  left_join(sched, by = "game_id") |>
  mutate(clean = coalesce(final_home == sched_home & final_away == sched_away & next_side == team_id_side, FALSE),
         first_won = (next_side == "home") == (final_home > final_away),
         went_to_ot = periods > reg_periods,
         era = cut(season, c(2001, 2005, 2012, 2017, 2100),
                   labels = c("2002-2005", "2006-2012", "2013-2017", "2018-now")))

tied_games <- tied |> filter(clean)

cat("games tied at 5:00 of regulation:", nrow(tied), "| clean:", nrow(tied_games),
    "| went to OT:", sum(tied_games$went_to_ot), "\n")

# the team that scored next after the 5:00 tie: how often did they win?
first_won_table(tied_games)
first_won_table(tied_games, era)
first_won_table(tied_games, next_kind)

# OT vs regulation, side by side, and a test of whether the two rates differ
ot_vs_reg <- bind_rows(
  first_won_table(games) |> mutate(situation = "scored first in OT"),
  first_won_table(tied_games) |> mutate(situation = "broke a tie with 5:00 left")
) |>
  select(situation, games, first_scorer_won, pct)
ot_vs_reg

prop.test(ot_vs_reg$first_scorer_won, ot_vs_reg$games)

# ---- charts ----

DREAM <- "#C8102E"
GRAY  <- "#8A8A86"
FADED <- "#D6D2DE"
INK   <- "#010101"

theme_ot <- function() {
  theme_minimal(base_size = 15) +
    theme(plot.title.position   = "plot",
          plot.caption.position = "plot",
          plot.title    = element_text(face = "bold", colour = INK, size = 19),
          plot.subtitle = element_text(colour = GRAY, size = 13, margin = margin(b = 14)),
          plot.caption  = element_text(colour = GRAY, size = 11, hjust = 0, margin = margin(t = 14)),
          axis.text     = element_text(colour = INK, size = 13),
          panel.grid.minor   = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "#E3E3E0", linewidth = 0.3),
          plot.margin   = margin(18, 18, 14, 10))
}

# stacked bars: the highlighted part in red on the left, the rest in gray, a label on the right.
# `segments` has bar (in display order), hit (TRUE = red), n, label; `totals` has bar, total, right
stacked_bars <- function(segments, totals, title, subtitle, caption) {
  lvls <- unique(segments$bar)
  segments <- segments |> mutate(bar = fct_rev(factor(bar, levels = lvls)))
  totals   <- totals   |> mutate(bar = fct_rev(factor(bar, levels = lvls)))
  ggplot(segments, aes(x = n, y = bar, fill = hit)) +
    geom_col(width = 0.6, colour = "white", linewidth = 1) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5),
              colour = "white", size = 4.8, fontface = "bold") +
    geom_text(data = totals, aes(x = total, y = bar, label = right), inherit.aes = FALSE,
              hjust = -0.2, colour = INK, size = 6, fontface = "bold") +
    scale_fill_manual(values = c(`TRUE` = DREAM, `FALSE` = GRAY), guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
    coord_cartesian(clip = "off") +   # keeps the right-hand label from getting cut off
    labs(title = title, subtitle = subtitle, caption = caption, x = NULL, y = NULL) +
    theme_ot() +
    theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank(),
          plot.margin = margin(18, 30, 14, 10))
}

span <- sprintf("%d-%d", min(games$season), max(games$season))
caption_all <- "regular season + playoffs  |  chart: @wnbadata\ndata: wehoop"

# 1. beat 2: the team that scored first won 185 of 297
overall <- first_won_table(games)

p_overall <- stacked_bars(
  segments = tibble(bar = "team that scored first in OT", hit = c(TRUE, FALSE),
                    n = c(overall$first_scorer_won, overall$games - overall$first_scorer_won),
                    label = c(paste(overall$first_scorer_won, "won"),
                              paste(overall$games - overall$first_scorer_won, "lost"))),
  totals = tibble(bar = "team that scored first in OT", total = overall$games,
                  right = paste0(round(overall$pct), "%")),
  title = "the team that scores first in OT 'usually' wins",
  subtitle = sprintf("%d wnba overtime games, %s", overall$games, span),
  caption = caption_all)

# 2. beat 4: a three first vs a two first. free throws are left off on purpose: 2 made free
# throws first won 48% vs 64% for a two, same 2 points, so that split looks like noise
how <- first_won_table(games |> filter(first_kind != "free throw"), first_kind) |>
  mutate(group = factor(paste("first points were a", first_kind),
                        levels = c("first points were a three", "first points were a two")),
         label = sprintf("%.0f%%  (%d of %d)", pct, first_scorer_won, games))

p_how <- ggplot(how, aes(x = pct, y = fct_rev(group))) +
  geom_col(width = 0.6, fill = DREAM) +
  geom_vline(xintercept = 50, linetype = "dashed", colour = GRAY, linewidth = 0.6) +
  annotate("text", x = 50, y = 2.45, label = "coin flip", colour = GRAY, size = 4.5, hjust = -0.1, vjust = 0) +
  geom_text(aes(label = label), x = 2, hjust = 0, colour = "white", size = 4.8, fontface = "bold") +
  scale_x_continuous(limits = c(0, 100), labels = \(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(title = "scoring first with a three vs a two",
       subtitle = "how often the team that scored first in OT won",
       caption = caption_all, x = NULL, y = NULL) +
  theme_ot()

# 3. beat 5: the same 14 bonner games, counted 2 ways. 14 of her 19 OT wins came after her team
# scored first (74%), but her teams went 14-7 when they scored first (67%)
b_wins  <- bonner |> filter(won)
b_first <- bonner |> filter(her_team_scored_first)
both    <- sum(bonner$won & bonner$her_team_scored_first)

p_flip <- stacked_bars(
  segments = tibble(bar = rep(c(sprintf("her %d OT wins", nrow(b_wins)),
                                sprintf("her %d OT games scoring first", nrow(b_first))), each = 2),
                    hit = c(TRUE, FALSE, TRUE, FALSE),
                    n = c(both, nrow(b_wins) - both, both, nrow(b_first) - both),
                    label = c(paste(both, "scored first"), paste(nrow(b_wins) - both, "didn't"),
                              paste(both, "won"), paste(nrow(b_first) - both, "lost"))),
  totals = tibble(bar = c(sprintf("her %d OT wins", nrow(b_wins)),
                          sprintf("her %d OT games scoring first", nrow(b_first))),
                  total = c(nrow(b_wins), nrow(b_first)),
                  right = paste0(round(100 * both / c(nrow(b_wins), nrow(b_first))), "%")),
  title = "dewanna bonner's overtime games",
  subtitle = sprintf("%d overtime games, %d-%d", nrow(bonner), min(bonner$season), max(bonner$season)),
  caption = "regular season + playoffs  |  chart: @wnbadata\ndata: wehoop and her hoop stats")

# extra (on-screen hook option, not in the script): bonner's teams 14-7 / 5-12
bonner_rec <- bonner |>
  mutate(bar = if_else(her_team_scored_first, "her team scored first", "other team scored first")) |>
  group_by(bar) |>
  summarise(w = sum(won), l = sum(!won), .groups = "drop") |>
  arrange(desc(bar == "her team scored first"))

p_bonner <- stacked_bars(
  segments = bonner_rec |>
    reframe(bar = rep(bar, each = 2), hit = rep(c(TRUE, FALSE), n()),
            n = c(rbind(w, l)), label = paste(c(rbind(w, l)), rep(c("won", "lost"), n()))),
  totals = bonner_rec |> transmute(bar, total = w + l, right = paste0(w, "-", l)),
  title = "dewanna bonner's teams in overtime",
  subtitle = sprintf("record by who scored first in OT, %d games, %d-%d", nrow(bonner), min(bonner$season), max(bonner$season)),
  caption = "regular season + playoffs  |  chart: @wnbadata\ndata: wehoop and her hoop stats")

# 5. all 38 bonner OT games as a tibble, grouped by who scored first (flash on screen during beat 5)
bonner_games <- bonner |>
  arrange(desc(her_team_scored_first), game_date) |>
  transmute(date = game_date,
            team = str_to_lower(team),
            opponent = str_to_lower(opponent),
            scored_first = if_else(her_team_scored_first, "her team", "other team"),
            result = paste0(if_else(won, "W ", "L "), score,
                            if_else(periods - reg_periods > 1, paste0(" (", periods - reg_periods, "OT)"), "")))

# ---- chart index (in script order) ----
p_overall       # beat 2: the team that scored first won 185 of 297, 62%
p_how           # beat 4: three first 71%, two first 64%
cat("dewanna bonner's 38 overtime games, sorted by who scored first, then by date\n")   # beat 5, flash
print(bonner_games, n = Inf)                                                            # scored first 14-7, not 5-12
p_flip          # beat 5: bonner, 14 of 19 wins (74%) vs 14-7 scoring first (67%)
p_bonner        # extra, not in the script: bonner's teams 14-7 / 5-12 (on-screen hook option)
