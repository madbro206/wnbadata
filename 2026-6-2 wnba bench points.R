if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::pkg_install(c("wehoop", "dplyr", "glue", "progressr", "tictoc", "ggplot2", "patchwork"))
library(dplyr)

tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box()
})
tictoc::toc()


data <- wnba_player_box %>%
  filter(starter==FALSE, season_type==2, minutes>0) %>%
  group_by(athlete_id, athlete_display_name, team_name) %>%
  summarize(bench_pts = sum(points, na.rm=TRUE), bench_games =n(), mins = sum(minutes, na.rm=TRUE), p_per_nonstart =bench_pts/bench_games, p_per_min=bench_pts/mins) %>%
  filter(bench_pts >0) %>%
  arrange(desc(p_per_nonstart))

data %>% select(athlete_display_name, bench_pts, bench_games, p_per_nonstart)

#overall points per minute
wnba_player_box %>%
  filter(season_type==2) %>%
  group_by(athlete_id, athlete_display_name, team_name) %>%
  summarize(pts = sum(points, na.rm=TRUE), games =n(), mins = sum(minutes, na.rm=TRUE), p_per_min=pts/mins) %>%
  filter(pts >0) %>%
  arrange(desc(p_per_min)) %>%
  select(athlete_display_name, team_name, pts, mins, p_per_min)



ppg<- wnba_player_box %>%
  filter(season_type==2, minutes>0) %>%
  group_by(athlete_display_name, team_name) %>%
  summarize(points = sum(points, na.rm=TRUE), games =n(), ppg=points/games) %>%
  filter(points >0) %>%
  arrange(desc(ppg))

print(ppg, n=15)
