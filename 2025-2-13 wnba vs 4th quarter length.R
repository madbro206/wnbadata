#Use wehoop package to download NCAAW data
#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, ggplot2, lubridate)

#load data
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp(season=2024)
})
tictoc::toc()

##############################################################################################################
#find length of 4th quarter (realtime from tip to final buzzer)

# Convert the wallclock variable to POSIXct datetime format
wnba_pbp$wallclock <- as.POSIXct(wnba_pbp$wallclock, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")

#filter to fourth quarter and regular season only
fourth_pbp <- wnba_pbp %>%
    filter(period==4 & season_type==2 & home_team_name != "Team WNBA" & away_team_name != "Team USA")

#group by game_id and calculate the length of fourth quarter
fourth_lengths <- fourth_pbp %>%
  filter(!is.na(wallclock)) %>%
  group_by(game_id) %>%
  summarize(
    game_date = first(game_date),
    home_team_name = first(home_team_mascot),
    away_team_name = first(away_team_mascot),
    fourth_length = if(n() >= 2) difftime(max(wallclock, na.rm = TRUE), min(wallclock, na.rm = TRUE), units = "mins") else NA_real_,
    start_time = min(wallclock, na.rm = TRUE),
    end_time = max(wallclock, na.rm = TRUE)
    #valid_wallclock_count = sum(!is.na(wallclock))
  ) %>%
  arrange(desc(fourth_length))

#eliminate outliers (bad wallclock data on these games)
fourth_lengths <- subset(fourth_lengths, fourth_length < 100)

print(fourth_lengths, n=10)


#storm vs fever may 22 (longest fourth quarter)
subset(fourth_pbp, game_id==401620237)$clock_display_value




#boxplot with jitter
#convert difftime to numeric minutes
fourth_lengths$fourth_length <- as.numeric(fourth_lengths$fourth_length, units = "mins")

#plot
ggplot(fourth_lengths, aes(y = fourth_length, x = factor(""))) +
  geom_boxplot(fill = "lightblue", width = 0.3, outlier.shape = NA) +
  geom_jitter(width = 0.1, color = "darkorange", alpha = 0.7, size = 1.5) +
  coord_cartesian(ylim = c(20, 50)) +
  labs(title = "4th Quarter Length by Game",
      subtitle = "WNBA 2024 Regular Season",
       y = "Duration (Minutes)",,
      caption="data: wehoop | graphic: @wnbadata",
       x = NULL) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 50, 10)) +
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_blank())



#misc
max(fourth_lengths$fourth_length)
var(fourth_lengths$fourth_length)
#subset(fourth_pbp, game_id==401620407)$clock_display_value
#subset(fourth_pbp, game_id==401620407)$wallclock


#unrivaled fourth quarters
unrivaled <- read.csv("~/Desktop/unrivaled_fourths.csv", header = TRUE)

#convert minutes to decimals
unrivaled <- unrivaled %>%
  filter(game != "" & length_mins != "") %>%
  mutate(decimal_mins = case_when(
    length_mins == "" ~ NA_real_,
    TRUE ~ period_to_seconds(ms(length_mins)) / 60
  ))

mean(unrivaled$decimal_mins, na.rm=TRUE)
min(unrivaled$decimal_mins, na.rm=TRUE)

#boxplot for Unrivaled
ggplot(unrivaled, aes(y = decimal_mins, x = factor(""))) +
  geom_boxplot(fill = "lightblue", width = 0.3, outlier.shape = NA) +
  geom_jitter(width = 0.1, color = "purple", alpha = 0.7, size = 1.5) +
  coord_cartesian(ylim = c(0, 20)) +
  labs(title = "4th Quarter Length by Game",
      subtitle = "Unrivaled Through 2/8/25",
       y = "Duration (Minutes)",,
      caption="data: unrivaled.basketball | graphic: @wnbadata",
       x = NULL) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_blank())