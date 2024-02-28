#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr)


#equivalent in NCAAW:
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


#find scores for each game, larger score first
scores <- wbb_team_box %>%
  distinct(game_id, .keep_all = TRUE) %>%#record only 1 score for each game
  mutate(winning_score = pmax(team_score, opponent_team_score), losing_score = pmin(team_score, opponent_team_score)) %>%
  select(winning_score, losing_score, team_location, opponent_team_location, game_date) 
  
#Arrange the data by game_date within each group of winning_score and losing_score
scores1 <- scores %>%
  group_by(winning_score, losing_score) %>%
  arrange(desc(game_date)) %>%
  #Keep only the first row (most recent) within each group
  slice(1) %>%
  ungroup()




#plot
library(ggplot2)

ggplot(scores, aes(x = losing_score, y = winning_score)) +
  geom_point(shape = 15, size = 1, color="blue") + #shape 15 squares
  labs(x = "Losing Score", y = "Winning Score") +
  ggtitle("NCAA Scorigami 2023-24") +
  coord_equal()
  
#score that never happened
subset(wbb_team_box, team_score==75 & opponent_team_score==76)
subset(wbb_team_box, team_score==49 & opponent_team_score==50)

#most popular scores
subset(frequency, frequency==17)
subset(scores, winning_score==65 & losing_score==59)
subset(scores, winning_score==66 & losing_score==59)


#plot showing frequency
frequency <- scores %>%
  group_by(losing_score, winning_score) %>%
  summarize(frequency = n()) 
 
  
ggplot(frequency, aes(x = losing_score, y = winning_score, fill = frequency)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "Losing Score", y = "Winning Score", fill = "Frequency") +
  ggtitle("NCAA Scorigami 2023-24")



#interactive plot
library(ggplot2)
library(plotly)

# Assuming 'scores' dataframe contains the data
p <- ggplot(scores, aes(x = losing_score, y = winning_score, text = paste("(", losing_score, ", ", winning_score, ")"))) +
  geom_point(shape = 15, size = 1, color="blue") + # Shape 15 corresponds to squares
  labs(x = "Losing Score", y = "Winning Score") +
  ggtitle("NCAA Scorigami 2023-24") +
  coord_equal()

ggplotly(p, tooltip = "text")



#interactive and showing frequency
frequency <- scores %>%
  group_by(losing_score, winning_score) %>%
  summarize(frequency = n())
  
#add to show latest instance of each score
combined_data <- scores1 %>%
	left_join(frequency, by=c("losing_score"="losing_score", "winning_score"="winning_score"))


p <- ggplot(combined_data, aes(x = losing_score, y = winning_score, fill = frequency, text = paste("Score:", winning_score, "-", losing_score, "Frequency: ", frequency, "Most recent:", game_date, team_location, "vs", opponent_team_location))) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "Losing Score", y = "Winning Score", fill = "Frequency") +
  ggtitle("NCAA Scorigami 2023-24")+
  coord_equal()

ggplotly(p, tooltip = "text")