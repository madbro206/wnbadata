#home win percentage cumulative by date

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, tidyverse, ggplot2)

#wbb team boxscores
tictoc::tic()
progressr::with_progress({
  wbb_team_box <- wehoop::load_wbb_team_box(season=2023) #ncaa 2022-23 season
})
tictoc::toc()

home_games <- wbb_team_box %>% 
	filter(team_home_away =="home") %>%
	arrange(game_date) %>%
	mutate(cumulative_win_percentage = 100*cumsum(ifelse(team_winner, 1, 0)) / row_number())


ggplot(home_games, aes(x = game_date, y = cumulative_win_percentage)) +
  #geom_line() + #this gives the real line but it's kinda ugly
  geom_smooth(method = "loess", se = FALSE, col="purple") +  #smoothed line without confidence interval
  geom_hline(yintercept = 50, linetype = "dashed", color = "blue") +  #line at 50% for reference
  labs(title = "NCAAWBB Home Win Percentage To-Date, 2022-23 Season",
       x = "Game Date",
       y = "Cumulative Win Percentage") +
  theme_minimal()
