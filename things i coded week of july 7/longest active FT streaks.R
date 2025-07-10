#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, stringr, ggplot2)

#load data

#wnba full play by play
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp(season=c(2020:2025))
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(season=c(2020:2025))
})
tictoc::toc()


data <- wnba_pbp %>% 
  arrange(game_date, game_play_number) %>%  #make sure the log is in order
  group_by(athlete_id_1) %>%
  filter(str_detect(text, regex("Free Throw", ignore_case = TRUE))) %>%
  mutate(result = case_when(str_detect(text, "makes") ~ "Make",
        str_detect(text, "misses") ~ "Miss"),
        attempt = row_number())



data <- data %>%
  mutate(is_make = result == "Make")
        
data <- data %>%
  group_by(athlete_id_1) %>%
  mutate(streak_id = with(rle(is_make), rep(seq_along(lengths), lengths)))

make_streaks <- data %>%
  filter(is_make) %>%
  group_by(athlete_id_1, streak_id) %>%
  summarise(streak_length = n(), .groups = "drop")

longest_streaks <- make_streaks %>%
  group_by(athlete_id_1) %>%
  summarise(max_streak = max(streak_length), .groups = "drop")

active_streaks <- data %>%
  group_by(athlete_id_1) %>%
  filter(row_number() == n()) %>%  # last row per player
  filter(is_make) %>%             # only if last is a make
  left_join(make_streaks, by = c("athlete_id_1", "streak_id")) %>%
  select(athlete_id_1, streak_length) %>%
  arrange(desc(streak_length))

active_streaks


# Join with wnba_player_box to get athlete_display_name
active_streaks_with_names <- active_streaks %>%
  left_join(wnba_player_box, by = c("athlete_id_1" = "athlete_id")) %>%
  select(athlete_id_1, streak_length, athlete_display_name)

active_streaks_with_names <- active_streaks_with_names %>%
    distinct(athlete_id_1) %>%
    select(streak_length, athlete_display_name)
  
# After your join:
active_streaks_with_names <- active_streaks_with_names %>%
  distinct(athlete_id_1, .keep_all = TRUE)

active_streaks_with_names
