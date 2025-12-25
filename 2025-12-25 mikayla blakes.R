pacman::p_load(wehoop, dplyr, tidyr, tictoc, progressr, ggplot2, lubridate)

#load WNBA data
tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box(season=c(2007:2026))
})
tictoc::toc()

wbb_career_points <- wbb_player_box %>%
  filter(!is.na(points)) %>% #keep only games played
  filter(athlete_display_name!="Odyssey Sims") %>% #removing because many games are missing for some reason
  filter(athlete_display_name!="Jennie Simms") %>% 
  filter(athlete_display_name!="Randi Brown") %>% 
  group_by(athlete_id, athlete_display_name) %>%
  arrange(game_date, game_id, .by_group = TRUE) %>%  # ensure correct game order
  mutate(
    game_number = row_number(),           # career game index
    career_pts  = cumsum(points)          # cumulative career points
  ) %>%
  ungroup() %>%
  select(season, athlete_display_name, athlete_id, team_location, game_number, career_pts)


speed_to_1000 <- wbb_career_points %>%
  group_by(athlete_display_name, athlete_id) %>%
  filter(career_pts >= 1000) %>%
  slice_min(game_number, with_ties = FALSE) %>%  # first time they cross 1000
  ungroup() %>%
  select(athlete_display_name, athlete_id, game_number, career_pts)

wbb_career_points %>%filter(athlete_display_name == "Kevi Luper")

summary_stats <- speed_to_1000 %>%
  summarize(
    median_games = median(game_number),
    mean_games   = mean(game_number),
    min_games    = min(game_number),
    max_games    = max(game_number)
  )
summary_stats

#idk why the data misses edd
elena_row <- tibble::tibble(
  athlete_display_name = "Elena Delle Donne",
  athlete_id          = 1,   # or a fake/id you choose
  game_number         = 38L,
  career_pts          = 1000L
)

kevi_row <- tibble::tibble(
  athlete_display_name = "Kevi Luper",
  athlete_id          = 2,   # or a fake/id you choose
  game_number         = 42L,
  career_pts          = 1000L
)

speed_to_1000_incl_elena <- bind_rows(speed_to_1000, elena_row)
speed_to_1000_incl_elena <- bind_rows(speed_to_1000_incl_elena, kevi_row)

speed_to_1000_incl_elena %>% filter(game_number<=46)%>% arrange(game_number) %>% select(athlete_display_name, game_number, career_pts)

ggplot(speed_to_1000, aes(x = game_number)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  geom_vline(
aes(xintercept = 93),
    color = "red", size = 1.2
  ) +
  labs(
    title = "How Fast Do Players Reach\n1,000 Career Points?",
    subtitle ="NCAAW Since 2006-07",
    x = "Games to reach 1,000 points",
    y = "Number of players",
    caption ="data: wehoop | chart: @wnbadata"
  ) +
  theme_minimal()
