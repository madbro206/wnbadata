#Use wehoop package to download wnba data

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, tidyr, ggplot2, teamcolors)

tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box()
})
tictoc::toc()

###########################################################################
#general setup
wnba_team_box <- wnba_team_box %>%
	filter(team_location != "Team USA" & team_location !="Team WNBA")

w_teams <-unique(wnba_team_box$team_location)

w_key <- cbind(team_location= w_teams, key=c(1:12))

pac <- wnba_team_box %>%
	filter(team_winner==TRUE) %>% #take one instance of each game only
	select(team_location, team_score, opponent_team_location, opponent_team_score)
	
#include key in dataframe
pac <- merge(pac, w_key, by.x = "team_location", by.y = "team_location", all.x = TRUE)
pac <- merge(pac, w_key, by.x = "opponent_team_location", by.y = "team_location", all.x = TRUE)

#rename columns
colnames(pac)[which(names(pac) == "key")] <- "team_key"
colnames(pac)[which(names(pac) == "key.x")] <- "team_key"
colnames(pac)[which(names(pac) == "key.y")] <- "opponent_team_key"

#select columns
pac <- pac %>%
  select(team_location, team_score, opponent_team_location, opponent_team_score, team_key, opponent_team_key)
  
  
###########################################################################
#rank based on wins/losses only

#create adjacency matrix
#12x12 matrix filled with zeroes
result_matrix <- matrix(0, nrow = 12, ncol = 12)

# Set row and column names for the matrix
rownames(result_matrix) <- colnames(result_matrix) <- 1:12

# Iterate over rows of pac dataframe
for (i in 1:nrow(pac)) {
  # Get team_key and opponent_team_key for current row
  team_key <- pac$team_key[i]
  opponent_team_key <- pac$opponent_team_key[i]
  
  # Increment the corresponding entry in the matrix
  result_matrix[opponent_team_key, team_key] <- result_matrix[opponent_team_key, team_key] + 1
}

#normalize the rows of result_matrix
normalized_matrix <- t(apply(result_matrix, 1, function(row) row / sum(row)))

#actually I need the transpose of this
normalized_matrix <- t(normalized_matrix)

#calculate eigenvalues
eigen(normalized_matrix)$values
eigen(normalized_matrix)$vectors[,1]

pagerank <- Re(eigen(normalized_matrix)$vectors[,1]/sum(eigen(normalized_matrix)$vectors[,1]))

ranked_teams <- cbind(w_teams, pagerank)

#convert pagerank column to numeric
pagerank_numeric <- as.numeric(ranked_teams[, 2])

# Order ranked_teams by pagerank
ranked_teams <- ranked_teams[order(pagerank_numeric), ]

ranked_teams <- as.data.frame(ranked_teams)
ranked_teams$pagerank <- as.numeric(ranked_teams$pagerank)
ranked_teams <- ranked_teams %>% 
  arrange(desc(pagerank))

ranked_teams

###########################################################################
#data from all three power rankings

rankings <- read.csv("~/Desktop/wehoop/2024-8-26 wnba pagerank update/rankings.csv", header=TRUE)

df_long <- rankings %>%
  pivot_longer(cols = starts_with("rank"), 
               names_to = "rank_type", 
               values_to = "value")

#colors
colors <- subset(teamcolors, league=="wnba")

df_long$rank_type <- factor(df_long $rank_type, c("rank1", "rank2", "rank3"))


library(dplyr)
library(ggplot2)

# Join the df_long and colors dataframes on the team name
df_long <- df_long %>%
  left_join(colors, by = c("team" = "location"))

# Create the plot with the primary colors from the colors dataframe
ggplot(df_long, aes(x = rank_type, y = value, group = team, color = primary)) +
  geom_line(size = 2, linejoin = "mitre") +  # For lines
  geom_point(size = 4, stroke = 0) +         # For points
  scale_y_continuous(limits = c(0, .2)) +
  scale_x_discrete(labels = c("7/9", "AS Break", "8/26")) +
  scale_color_identity() +  # Use the actual color values
  scale_alpha_manual(values = c(0.15, 1)) +
  labs(
    title = "WNBADATA Power Rankings Over Time",
    subtitle= "thanks, google pagerank",
    caption = "@wnbadata"
  ) +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 30, 20, 20),
    plot.background = element_rect(fill = "grey90", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


