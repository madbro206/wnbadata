# Fouling Up 3 in the WNBA
# Methodology adapted from: https://www.espn.com/nba/story/_/id/48582233/nba-playoffs-2026-foul-3-san-antonio-spurs-portland-trail-blazers-impact
# Uses wehoop play-by-play data to examine whether intentionally fouling when up 3
# late in the 4th quarter is advantageous in the WNBA.

if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, ggplot2, stringr)

# ---------------------------------------------------------------------------
# 1. Load Data
# ---------------------------------------------------------------------------
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp(season = c(2021:2026))
})
tictoc::toc()

# Regular season only
wnba_pbp <- wnba_pbp %>% filter(season_type == 2)

# Parse wallclock
wnba_pbp$wallclock <- as.POSIXct(wnba_pbp$wallclock, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Compute score differential (positive = home team leading)
wnba_pbp <- wnba_pbp %>%
  mutate(score_differential = home_score - away_score)

# ---------------------------------------------------------------------------
# 2. Identify "Up 3" Possessions in Final 24 Seconds of Q4
# ---------------------------------------------------------------------------
# score_differential: positive = home leading, negative = away leading

up3_possessions <- wnba_pbp %>%
  filter(
    period_number == 4,
    period_display_value == "4th Quarter",
    clock_minutes == 0,
    clock_seconds <= 24,
    abs(score_differential) == 3
  ) %>%
  mutate(
    leading_team_id  = ifelse(score_differential > 0, home_team_id, away_team_id),
    trailing_team_id = ifelse(score_differential > 0, away_team_id, home_team_id),
    time_remaining   = floor(clock_seconds)
  ) %>%
  # exclude stoppages/administrative events that have no team (no_play, period_end, etc.)
  filter(!is.na(team_id))

# ---------------------------------------------------------------------------
# 3. Flag Fouls Committed by the Leading Team (proxy for intentional foul up 3)
# ---------------------------------------------------------------------------
# type_text contains the event description; fouls by the leading team on the
# trailing team's possession signal a deliberate foul-up-3 strategy.

foul_keywords <- c("personal foul", "shooting foul", "offensive foul",
                   "flagrant", "loose ball foul", "foul")

foul_events <- wnba_pbp %>%
  filter(
    period_number == 4,
    period_display_value == "4th Quarter",
    clock_minutes == 0,
    clock_seconds <= 24,
    abs(score_differential) == 3,
    str_detect(tolower(type_text), paste(foul_keywords, collapse = "|"))
  ) %>%
  mutate(
    leading_team_id  = ifelse(score_differential > 0, home_team_id, away_team_id),
    trailing_team_id = ifelse(score_differential > 0, away_team_id, home_team_id),
    time_remaining   = floor(clock_seconds)
  ) %>%
  # team_id on a foul event = the team that committed the foul (the defender);
  # leading team committing the foul = deliberate foul-up-3 candidate (not 100% sure about intent)
  filter(team_id == leading_team_id)

data<- foul_events%>%select(game_date, type_text, text, away_score, home_score, clock_seconds, period)
print(data, n=23)

# ---------------------------------------------------------------------------
# 4. Foul Up 3 Rate by Time Remaining (replicating the ESPN bar chart)
# ---------------------------------------------------------------------------
# For each second bucket (0-24), compute: how often did the leading team foul
# out of all possessions where they were up 3 at that time?

foul_rate_by_second <- tibble(time_remaining = 0:24) %>%
  left_join(
    up3_possessions %>% count(time_remaining, name = "n_possessions"),
    by = "time_remaining"
  ) %>%
  left_join(
    foul_events %>% count(time_remaining, name = "n_fouls"),
    by = "time_remaining"
  ) %>%
  mutate(
    n_possessions = coalesce(n_possessions, 0L),
    n_fouls       = coalesce(n_fouls, 0L),
    foul_rate     = ifelse(n_possessions > 0, n_fouls / n_possessions, NA_real_)
  )

# Bar chart: Foul Up 3 Rate by Second
#NOT informative imo
ggplot(foul_rate_by_second, aes(x = time_remaining, y = foul_rate)) +
  geom_col(fill = "#6B7FD4", width = 0.7) +
  scale_x_continuous(breaks = 0:24) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.65)) +
  labs(
    title    = "Foul Up 3 Rate, 2021 to Present (WNBA)",
    x        = "Time Remaining at Start of Possession (Seconds)",
    y        = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.x = element_blank()
  )


# ---------------------------------------------------------------------------
# 5. Game Outcome by Strategy
# ---------------------------------------------------------------------------
# For each game where an up-3 situation occurred in the final 24 sec, determine:
#   - Did the leading team foul? (foul_up_3 strategy)
#   - What was the final outcome for the leading team?

# Get final scores: last play of the last period (lowest clock time in highest period)
final_scores <- wnba_pbp %>%
  group_by(game_id) %>%
  filter(period_number == max(period_number)) %>%
  arrange(clock_minutes, clock_seconds) %>%
  slice(1) %>%
  ungroup() %>%
  select(game_id, home_team_id, away_team_id,
         final_home_score = home_score, final_away_score = away_score)

# Games that went to OT (had any period beyond 4)
ot_games <- wnba_pbp %>%
  filter(period_number > 4) %>%
  distinct(game_id) %>%
  mutate(went_to_ot = TRUE)

# One row per up-3 game: use the FIRST event at <=24 seconds (entry point into the
# window) so we capture a clean game state before any possessions play out.
# This avoids confusion when both teams are up 3 at different moments in the window.
up3_games <- wnba_pbp %>%
  filter(
    period_number == 4,
    clock_minutes == 0,
    clock_seconds <= 24
  ) %>%
  mutate(score_differential = home_score - away_score) %>%
  filter(abs(score_differential) == 3) %>%          # any event showing 3-point lead
  group_by(game_id) %>%
  slice_max(clock_seconds, n = 1, with_ties = FALSE) %>%  # earliest moment it was 3
  ungroup() %>%
  mutate(
    leading_team_id  = ifelse(score_differential > 0, home_team_id, away_team_id),
    trailing_team_id = ifelse(score_differential > 0, away_team_id, home_team_id)
  ) %>%
  select(game_id, leading_team_id, trailing_team_id, clock_seconds,
         home_team_id, away_team_id)

# Flag whether a foul-up-3 occurred in that game
foul_up3_games <- foul_events %>%
  distinct(game_id) %>%
  mutate(fouled = TRUE)

# Join everything
outcomes <- up3_games %>%
  left_join(foul_up3_games, by = "game_id") %>%
  mutate(fouled = coalesce(fouled, FALSE)) %>%
  left_join(final_scores, by = c("game_id", "home_team_id", "away_team_id")) %>%
  left_join(ot_games, by = "game_id") %>%
  mutate(
    went_to_ot     = coalesce(went_to_ot, FALSE),
    leader_is_home = leading_team_id == home_team_id,
    leader_final   = ifelse(leader_is_home, final_home_score, final_away_score),
    trailer_final  = ifelse(leader_is_home, final_away_score, final_home_score),
    result = case_when(
      went_to_ot                  ~ "overtime",
      leader_final > trailer_final ~ "win",
      leader_final < trailer_final ~ "loss"
    ),
    strategy = ifelse(fouled, "Foul Up 3", "Play Defense")
  )

# Summary table
# Diagnostic: check for NA results (usually means final_scores join failed)
na_results <- outcomes %>% filter(is.na(result))
if (nrow(na_results) > 0) {
  cat("\n--- Games with missing result (join may have failed) ---\n")
  print(select(na_results, game_id, strategy, leader_final, trailer_final, went_to_ot))
}

strategy_summary <- outcomes %>%
  filter(!is.na(result)) %>%
  group_by(strategy) %>%
  summarise(
    outright_wins   = sum(result == "win",      na.rm = TRUE),
    outright_losses = sum(result == "loss",     na.rm = TRUE),
    overtime        = sum(result == "overtime", na.rm = TRUE),
    total           = n(),
    win_rate        = (outright_wins + overtime * 0.5) / total,
    .groups = "drop"
  )

print(strategy_summary)

# ---------------------------------------------------------------------------
# 6. Win Rate Comparison Bar Chart
# ---------------------------------------------------------------------------
ggplot(strategy_summary, aes(x = strategy, y = win_rate, fill = strategy)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = scales::percent(win_rate, accuracy = 0.1)),
            vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = c("Foul Up 3" = "#6B7FD4", "Play Defense" = "#A0A0A0")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1.05)) +
  labs(
    title = "Win Rate When Up 3 in Final 24 Seconds (WNBA, 2021–Present)",
    subtitle = "OT games treated as 50/50 | Regular season only",
    x = NULL, y = "Win Rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray50"))

#ggsave("foul_up3_winrate_wnba.png", width = 7, height = 6, dpi = 150)

# ---------------------------------------------------------------------------
# 7. Quick sanity checks
# ---------------------------------------------------------------------------
cat("\n--- Games with Up-3 possessions found ---\n") #under-estimate on total up 3 possessions
print(length(unique(up3_possessions$game_id)))

cat("\n--- Games with Foul-up-3 events found ---\n")
print(length(unique(foul_events$game_id)))

cat("\n--- Strategy outcomes ---\n")
print(strategy_summary)


#check foul up three loss scenario
outcomes %>%
  filter(strategy == "Foul Up 3", result == "loss") %>%
  select(game_id, clock_seconds, leader_final, trailer_final)

wnba_pbp %>% filter(game_id==401857011) %>%select(game_date, home_team_name, away_team_name)

#lib vs sparks 6/21/2026 nneka buzzer beater :)
#breanna stewart accidentally fouled around 22.6 sec left in 4th when libs were up 3
#and again at 10.3





#####
#sanity check- any comebacks in last 24 sec down exactly 3?

comebacks <- wnba_pbp %>%
  mutate(
    clock_secs = {
      parts <- strsplit(clock_display_value, ":")
      as.integer(sapply(parts, `[`, 1)) * 60 + as.integer(sapply(parts, `[`, 2))
    },
    home_diff = home_score - away_score
  ) %>%
  # Step 1: games where a team was down 3 in last 24s of Q4
  filter(period_number == 4, clock_secs <= 24, abs(home_diff) == 3) %>%
  group_by(game_id) %>%
  slice_min(clock_secs, n = 1) %>%          # earliest moment of being down 3
  ungroup() %>%
  mutate(trailing_team = if_else(home_diff == -3, "home", "away")) %>%
  # Step 2: attach final Q4 score
  left_join(
    wnba_pbp %>%
      filter(period_number == 4) %>%
      group_by(game_id) %>%
      slice_max(game_play_number, n = 1) %>%
      ungroup() %>%
      select(game_id, final_home = home_score, final_away = away_score),
    by = "game_id"
  ) %>%
  # Step 3: filter to actual comebacks (tied or won)
  filter(
    (trailing_team == "home" & final_home >= final_away) |
    (trailing_team == "away" & final_away >= final_home)
  )


gamez <- comebacks%>% select(game_date, text, home_team_name, away_team_name, final_home, final_away)
print(gamez, n=35)

# ---------------------------------------------------------------------------
# Unit Chart — one square per game, colored by outcome
# ---------------------------------------------------------------------------
make_units <- function(wins, losses, ot, ncols, label) {
  n <- wins + losses + ot
  tibble(
    result   = factor(c(rep("Win", wins), rep("OT", ot), rep("Loss", losses)),
                      levels = c("Win", "OT", "Loss")),
    strategy = label,
    idx      = seq_len(n),
    col      = (idx - 1) %% ncols + 1,
    row      = ceiling(idx / ncols)
  )
}

unit_data <- bind_rows(
  make_units(16, 1, 4,  ncols = 7,  label = "Defense Fouled Up 3 (possibly on accident)\n(21 games, 85.7% win rate)"),
  make_units(206, 0, 10, ncols = 24, label = "Played Normal Defense\n(216 games, 97.7% win rate)")
)

ggplot(unit_data, aes(x = col, y = -row, fill = result)) +
  geom_tile(color = "white", linewidth = 1.2) +
  facet_wrap(~strategy, scales = "free", ncol = 1) +
  scale_fill_manual(
    values = c("Win" = "#4A90D9", "OT" = "#F5A623", "Loss" = "#D0021B"),
    name   = NULL
  ) +
  labs(
    title    = "WNBA Defenses Up 3 with <24 Seconds Left",
    subtitle = "1 square = 1 game, 2021–Present",
    caption  = "OT win rate treated as 50/50 | data: wehoop | chart: @wnbadata"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle   = element_text(hjust = 0.5, color = "gray50", margin = margin(b = 8)),
    plot.caption    = element_text(hjust = 0.5, color = "gray60", size = 10),
    strip.text      = element_text(face = "bold", size = 13, margin = margin(b = 6, t = 10)),
    legend.position = "bottom",
    legend.key.size = unit(0.8, "cm"),
    plot.margin     = margin(12, 20, 12, 20)
  )
