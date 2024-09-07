#Use wehoop package to download WNBA data
#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, ggplot2)

#load data
#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box()
})

tictoc::toc()

team_box <- wnba_team_box %>% 
	filter(season_type==2)%>% #regular season
	filter(team_name!="Team USA" & team_name!="Team WNBA")%>% #not all star game
	select(game_date, team_name, assists, total_rebounds, team_winner)

summary(team_box)
##############################################################################################################
# logistic regression model, not accounting for confounding factors
model <- glm(team_winner ~ assists + total_rebounds, data = team_box, family = binomial())

summary(model)

##############################################################################################################
#probability by reb & assist heatmap (Chatgpt to generate initial plot)
# Create a grid of assists and total_rebounds values
new_data <- expand.grid(assists = seq(min(team_box$assists), max(team_box$assists), length.out = 100),
                        total_rebounds = seq(min(team_box$total_rebounds), max(team_box$total_rebounds), length.out = 100))

# Predict probabilities for the grid
new_data$predicted_prob <- predict(model, newdata = new_data, type = "response")

# Plot heatmap
ggplot(new_data, aes(x = assists, y = total_rebounds, fill = predicted_prob)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red", name = "Win Probability") +
  labs(title = "Predicted Probability of Team Win by Reb/Ast Total",
  	   subtitle= "logistic regression on 2024 WNBA data through 9/6",
  	   caption= "source: stats.wnba.com | graphic: @wnbadata",
       x = "Assists",
       y = "Total Rebounds") +
  theme_minimal()


##############################################################################################################
#save hires version

plot <- ggplot(new_data, aes(x = assists, y = total_rebounds, fill = predicted_prob)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red", name = "Win Probability") +
  labs(title = "Predicted Probability of Team Win by Reb/Ast Total",
       subtitle= "logistic regression on 2024 WNBA data through 9/6",
       caption= "source: stats.wnba.com | graphic: @wnbadata",
       x = "Assists",
       y = "Total Rebounds") +
  theme_minimal()

# Save the plot as a high-resolution image
ggsave("~/Desktop/heatmap.png", plot = plot, width = 10, height = 8, dpi = 300)
