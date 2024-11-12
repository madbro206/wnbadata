#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr,)

#load data NCAAW
tictoc::tic()
progressr::with_progress({
  wbb_pbp <- wehoop::load_wbb_pbp()
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_team_box <- wehoop::load_wbb_team_box()
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box()
})
tictoc::toc()

#############################################################################################################
#sort to pre-tip stats

pre_tip <- wbb_pbp %>%
	filter(period==1 & clock_display_value=="10:00") %>%
	select(game_id, game_date, period, clock_display_value, away_score, home_score, text, away_team_name, home_team_name)
	
print(pre_tip, n=27)

pre_tip <- wbb_pbp %>%
  filter(period == 1 & clock_display_value == "10:00") %>%
  select(game_id, game_date,  away_team_name, home_team_name) %>%
  arrange(game_id, game_date) %>%  # Add this line to sort if needed
  distinct(game_id, .keep_all = TRUE)

#how many games have there been?
length(unique(wbb_pbp$game_id))

#how many pretip games?
length(unique(pre_tip$game_id))

subset(wbb_pbp, game_play_number==1)$clock_display_value
##############################################################################################################
#chart of 0-0, 0-1, and 0-2 starts
library(ggplot2)

data <- data.frame(
  category = c("0-0", "0-1", "0-2"),
  value = c(365, 5, 2)
)
ggplot(data, aes(x = category, y = value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = value), vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(title = "Score at 10:00 in the first quarter",
       subtitle = "NCAA wbb 2024 through 11/10",
       x = "Score",
       y = "Frequency") +
  ylim(0, max(data$value) * 1.1)