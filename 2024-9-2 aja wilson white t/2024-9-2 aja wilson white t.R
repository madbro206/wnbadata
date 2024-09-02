#aja wilson white t shirt games

library(tidyverse)
library(ggdag)
library(dagitty)
library(ggplot2)
library(dplyr)
library(tidyr)
set.seed(1234)

#create causal diagram of aja wilson's white t shirts vs her scoring
#just for fun
my_dag <- dagify(
  points ~ white_tee,
  minutes ~ white_tee,
  points ~ minutes,
  exposure= "white_tee",
  outcome = "points"
  ) 

ggdag(my_dag) + 
	ggtitle("Causal Diagram: A'ja Wilson's White T-Shirts vs. Scoring") + 
	theme_minimal()
	
#aja wilson gamelogs from https://www.basketball-reference.com/wnba/players/w/wilsoa01w/gamelog/2024/
#white tee data inserted by me	
data <- read.csv("~/Desktop/wehoop/2024-9-2 aja wilson white t/aja_white_tee.csv", header = TRUE)

data <- data %>% 
  drop_na(Rk)
  
  
  
#CHART  

# Convert MP to minutes
data <- data %>% 
  mutate(MP = as.numeric(sub(":.+", "", MP)) + as.numeric(sub(".+:", "", MP))/60)

# Create scatter plot
ggplot(data, aes(x = MP, y = PTS, fill = factor(white_tee), color = factor(white_tee))) +
  geom_point(shape = 21, size = 3) +  # shape 21 allows fill and color to be specified separately
  labs(
    title = "A'ja Wilson Minutes vs Points in White Tee Games",
    subtitle = "2024 season through 9/1",
    caption = "source: basketball-reference.com | graphic: @wnbadata",
    x = "Minutes Played (MP)",
    y = "Points (PTS)",
    fill = "White Tee",
    color = "White Tee"
  ) +
  scale_fill_manual(values = c("black", "white"), labels = c("no", "yes")) +  # Custom labels for fill
  scale_color_manual(values = c("black", "black"), labels = c("no", "yes")) +  # Custom labels for color
  theme_minimal()




#SUMMARISE DATA
# Convert MP to minutes (if needed)
data <- data %>% 
  mutate(MP = as.numeric(sub(":.+", "", MP)) + as.numeric(sub(".+:", "", MP))/60)

# Create the new summary dataframe
summary_df <- data %>%
  group_by(white_tee) %>%
  summarise(
    games_played = n(),
    fg = sum(FG, na.rm = TRUE),
    fga = sum(FGA, na.rm = TRUE),
    avg_MP = mean(MP, na.rm = TRUE),
    avg_PTS = mean(PTS, na.rm = TRUE),
    avg_TRB = mean(TRB, na.rm = TRUE),
    avg_BLK = mean(BLK, na.rm = TRUE),
    avg_GmSc = mean(GmSc, na.rm=TRUE)
  ) %>%
  mutate(FGp=fg/fga)

summary_df


#TABLE
library(knitr)

# Create a summary table with average points, games played, and average total rebounds
summary_table <- summary_df %>%
  select(white_tee, games_played, avg_PTS, avg_TRB) %>%
  mutate(
    white_tee = ifelse(white_tee == 0, "No", "Yes")
  )

# Display the table
kable(
  summary_table,
  col.names = c("White Tee", "Games Played", "Average Points", "Average Rebounds"),
  caption = "A'ja Wilson ppg white tee vs not"
)




#bayesian things
m <- MASS::fitdistr(data$FG., dbeta, start = list(shape1 = 1, shape2 = 10))

alpha0 <- m$estimate[1]
beta0 <- m$estimate[2]

data_eb <- data %>%
    mutate(eb_estimate = (FG + alpha0) / (FGA + alpha0 + beta0))

ggplot(data_eb, aes(x = FG., y = eb_estimate, color = factor(white_tee))) +
  geom_point() +
  labs(
    title = "Scatter Plot of fg% vs empirical bayes fg%",
    x = "field goal percentage",
    y = "eb estimate",
    color = "White Tee"
  ) +
  scale_color_manual(values = c("blue", "red")) + # Customize colors if needed
  theme_minimal()

