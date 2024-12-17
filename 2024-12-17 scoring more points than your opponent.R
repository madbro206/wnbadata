#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR)

#load data
#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season=c(2003:2024))
})

tictoc::toc()

#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(season=c(2003:2024))
})
tictoc::toc()


#equivalent in NCAAW:
tictoc::tic()
progressr::with_progress({
  wbb_team_box <- wehoop::load_wbb_team_box(season=c(2020:2025))
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box(season=c(2020:2025))
})
tictoc::toc()

#NCAA players who scored more points than their opponent in a game since 2020 season
combined_data <- wbb_player_box %>%
  inner_join(wbb_team_box, by = c("game_id", "game_date", "team_id", "opponent_team_id"))

high_scorers <- combined_data %>%
  filter(points > opponent_team_score.x) %>%
  select(game_id, game_date, athlete_display_name, team_display_name.x, points, opponent_team_display_name.x, opponent_team_score.x) %>%
  arrange(desc(game_date))

print(high_scorers, n=28)

#WNBA players who scored more points than their opponent in a game
combined_data2 <- wnba_player_box %>%
  inner_join(wnba_team_box, by = c("game_id", "game_date", "team_id", "opponent_team_id"))

high_scorers2 <- combined_data2 %>%
  filter(points > opponent_team_score.x) %>%
  select(game_id, game_date, athlete_display_name, team_display_name.x, points, opponent_team_display_name.x, opponent_team_score.x)

print(high_scorers2)

############################# visualizations ###################################
library(ggplot2)
library(lubridate)

high_scorers <- high_scorers %>%
  mutate(game_date = as.Date(game_date))

all_performances <- wbb_player_box %>%
  inner_join(wbb_team_box, by = c("game_id", "game_date", "team_id", "opponent_team_id")) %>%
  select(game_id, game_date, athlete_display_name, team_display_name.x, 
         points, opponent_team_display_name.x, opponent_team_score.x) %>%
  mutate(game_date = as.Date(game_date))

ggplot(all_performances, aes(x = points, y = opponent_team_score.x)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  geom_point(data = high_scorers, aes(x = points, y = opponent_team_score.x), color = "red", size = 1.5) +
  labs(title = "Player Points vs. Opposing Team Score",
       x = "Player Points", y = "Opposing Team Score") +
  theme_minimal() +
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(0, max(max(all_performances$points), max(all_performances$opponent_team_score.x)))) +
  scale_y_continuous(limits = c(0, max(max(all_performances$points), max(all_performances$opponent_team_score.x))))

########################### viz with heads ############################

library(ggplot2)
library(ggimage)
library(dplyr)

# First, let's join the headshot URLs to our high_scorers dataframe
high_scorers_with_images <- high_scorers %>%
  left_join(wbb_player_box %>% select(athlete_display_name, athlete_headshot_href) %>% distinct(),
            by = "athlete_display_name")

# Now, let's create the plot
plot <- ggplot(all_performances, aes(x = points, y = opponent_team_score.x)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  geom_image(data = high_scorers_with_images, 
             aes(image = athlete_headshot_href),
             size = 0.035) +
  labs(title = "Player Points vs. Opposing Team Score",
       x = "Player Points", y = "Opposing Team Score") +
  theme_minimal() +
  #coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(0, max(max(all_performances$points), max(all_performances$opponent_team_score.x)))) +
  scale_y_continuous(limits = c(0, max(max(all_performances$points), max(all_performances$opponent_team_score.x))))

ggsave("high_resolution_plot.png", plot, width = 12, height = 12, dpi = 300)