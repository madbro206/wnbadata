library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)

scores <- read.csv("~/Desktop/wehoop/2024-10-16 scores/wnba_scores.csv", header = TRUE)

# Calculate frequency and proportion of each total game score
score_distribution <- scores %>%
  group_by(totalcombined) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

ggplot(score_distribution, aes(x = totalcombined, y = proportion)) +
  geom_point() +
  labs(title = "Distribution of Total Game Scores",
       x = "Total Score",
       y = "Proportion of Games") +
  theme_minimal()



score_distribution <- score_distribution %>%
	arrange(desc(proportion))
	
print(score_distribution, n=142)


subset(scores, totalcombined==146)

install.packages("plotly")
library(plotly)

p <- ggplot(score_distribution, aes(x = totalcombined, y = proportion, 
                                    text = paste("Score:", totalcombined, 
                                                 "<br>Proportion:", round(proportion, 4)))) +
  geom_point() +
  labs(title = "Distribution of Total Game Scores",
       x = "Total Score",
       y = "Proportion of Games") +
  theme_minimal()
  
  interactive_plot <- ggplotly(p, tooltip = "text")
interactive_plot