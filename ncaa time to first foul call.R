pacman::p_load(wehoop, dplyr, glue, tictoc, progressr)

tictoc::tic()
progressr::with_progress({
  wbb_pbp <- wehoop::load_wbb_pbp()
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box()
})
tictoc::toc()

#find first foul for each player each game
result2 <- wbb_pbp %>%
  filter(type_text == "PersonalFoul") %>%
  group_by(game_id, athlete_id_1) %>%
  arrange(qtr, desc(clock_display_value)) %>%
  slice(1) %>%
  select(game_id, athlete_id_1, text, game_date, qtr, clock_display_value) %>%
  arrange(qtr, desc(clock_display_value))
  
  
#find the time elapsed before each first foul by each player each game
result2 <- result2 %>%
  mutate(
    minutes = as.numeric(sub(":.*", "", clock_display_value)),
    seconds = as.numeric(sub(".*:", "", clock_display_value)),
    time_elapsed = ifelse(qtr == 1, 10 - minutes - seconds/60,
                         ifelse(qtr == 2, 20 - minutes - seconds/60,
                                ifelse(qtr == 3, 30 - minutes - seconds/60,
                                       ifelse(qtr == 4, 40 - minutes - seconds/60, NA))))
  )

print(result2, n=25)

#include team_locations in dataframe
result2_loc <- result2 %>%
  group_by(athlete_id_1) %>%
  left_join(wbb_player_box %>% distinct(athlete_id, team_location), by = c("athlete_id_1" = "athlete_id")) %>%
  ungroup()



#find avg time elapsed for each players' first foul of the game, sort by smallest to biggest
result2_summary <- result2_loc %>%
  group_by(athlete_id_1) %>%
    filter(n() >= 5) %>% #only players that have fouled in 5 or more games
  summarize(Player=first(text), team_location=first(team_location), avg_time_elapsed = mean(time_elapsed, na.rm = TRUE)) %>%
  arrange(avg_time_elapsed)

print(result2_summary, n=500)


#quickest team fouls (I'm not sure if this is doing what I wanted it to do)
result2_teams <- result2_loc %>%
  group_by(team_location) %>%
    filter(n() >= 12) %>% #only teams that have fouled 12 or more times
  summarize(team_location=first(team_location), avg_time_elapsed = mean(time_elapsed, na.rm = TRUE)) %>%
  arrange(avg_time_elapsed)

print(result2_teams, n=100)


#top 55 quickest fouls
print(result2, n=55)

#soniyah reed
print(subset(result2, text=="Foul on Soniyah Reed."), n=22)

#uw
print(subset(result2_summary, team_location=="Washington"), n=22)

#cc
print(subset(result2, text=="Foul on Caitlin Clark."), n=22)