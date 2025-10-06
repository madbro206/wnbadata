#Use wehoop package to download WNBA data
pacman::p_load(wehoop, dplyr, stringr, tictoc, progressr, ggplot2, knitr, tidyr, ggrepel)

tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp()
})
tictoc::toc()

# 1. Isolate free throw makes/misses using "text"
wnba_pbp_ft <- wnba_pbp %>%
  filter(str_detect(text, "makes free throw") | str_detect(text, "misses free throw")) %>%
  mutate(
    ft_made = str_detect(text, "makes free throw"),
    clock_seconds_remaining = as.numeric(clock_minutes) * 60 + as.numeric(clock_seconds)
  )

# 2. Filter for fourth quarter and get score margin for "clutch" FTs
wnba_pbp_ft <- wnba_pbp_ft %>%
  mutate(
    period_is_4 = period_number == 4,
    close_game = abs(home_score - away_score) <= 3,
    last_30s = period_is_4 & clock_seconds_remaining <= 30,
    clutch_scenario = last_30s & close_game
  )

# 3. Summarize FT% for clutch vs other
ft_clutch_vs_other <- wnba_pbp_ft %>%
  group_by(clutch_scenario) %>%
  summarise(
    made = sum(ft_made, na.rm = TRUE),
    attempts = n(),
    ft_pct = made / attempts,
    .groups = "drop"
  )

print(ft_clutch_vs_other)


# Filter clutch free throw attempts using the previous logic
clutch_free_throws <- wnba_pbp %>%
  filter(str_detect(text, "makes free throw|misses free throw")) %>%
  mutate(
    ft_made = str_detect(text, "makes free throw"),
    seconds_left = as.numeric(clock_minutes) * 60 + as.numeric(clock_seconds),
    clutch = period_number == 4 & seconds_left <= 30 & abs(home_score - away_score) <= 3
  ) %>%
  filter(clutch) %>%
  select(game_id, game_date, period_number, clock_display_value, text, ft_made, home_score, away_score)

# Print the clutch free throw attempts
print(clutch_free_throws, width = Inf, n=101)

