#Use wehoop package to download NCAAW data
#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr)

tictoc::tic()
progressr::with_progress({
  wbb_team_box <- wehoop::load_wbb_team_box()
})
tictoc::toc()

###########################################################################
#general setup
big_teams= c("USC", "UCLA", "Oregon", "Washington", "Maryland", "Ohio State", "Illinois", "Michigan", "Michigan State", "Indiana", "Iowa", "Nebraska", "Minnesota", "Wisconsin", "Purdue", "Rutgers","Northwestern", "Penn State")

#select only in-conference BIG games, before tournament
big1 <- wbb_team_box[ wbb_team_box$team_location %in% big_teams & wbb_team_box$opponent_team_location %in% big_teams & wbb_team_box$game_date < as.Date("2025-03-04"),]

#select only in-conference BIG games, incl tournament
#big1 <- wbb_team_box[wbb_team_box$team_location %in% big_teams & wbb_team_box$opponent_team_location %in% big_teams,]

big_key <- cbind(team_location= big_teams, key=c(1:18))

big <- big1 %>%
	filter(team_winner==TRUE) %>% #take one instance of each game only
	select(team_location, team_score, opponent_team_location, opponent_team_score)
	
#include key in dataframe
big <- merge(big, big_key, by.x = "team_location", by.y = "team_location", all.x = TRUE)
big <- merge(big, big_key, by.x = "opponent_team_location", by.y = "team_location", all.x = TRUE)

#rename columns
colnames(big)[which(names(big) == "key")] <- "team_key"
colnames(big)[which(names(big) == "key.x")] <- "team_key"
colnames(big)[which(names(big) == "key.y")] <- "opponent_team_key"

#select columns
big <- big %>%
  select(all_of(c(
    "team_location",
    "team_score", 
    "opponent_team_location",
    "opponent_team_score", 
    "team_key",
    "opponent_team_key"
  )))

###########################################################################
#rank based on wins/losses only

#create adjacency matrix
#12x12 matrix filled with zeroes
result_matrix <- matrix(0, nrow = 18, ncol = 18)

#set row and column names for the matrix
rownames(result_matrix) <- colnames(result_matrix) <- 1:18

#iterate over rows of big dataframe
for (i in 1:nrow(big)) {
  # Get team_key and opponent_team_key for current row
  team_key <- big $team_key[i]
  opponent_team_key <- big $opponent_team_key[i]
  
  #increment the corresponding entry in the matrix
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

ranked_teams <- cbind(big_teams, pagerank)

#convert pagerank column to numeric
pagerank_numeric <- as.numeric(ranked_teams[, 2])

#order ranked_teams by pagerank
ranked_teams <- ranked_teams[order(pagerank_numeric), ]

ranked_teams <- as.data.frame(ranked_teams)
ranked_teams$pagerank <- as.numeric(ranked_teams$pagerank)
ranked_teams <- ranked_teams %>% 
  arrange(desc(pagerank))
  
print(ranked_teams)

#make plot
plot(ranked_teams$pagerank, type = "p", pch = 19, 
     ylab = "Team", xlab = "Ranking", main = "Team Rankings")
text(ranked_teams$pagerank, ranked_teams$team_location, 
     labels = ranked_teams$team_location, pos = 4, cex = 0.7, xpd = TRUE)




###############################################################################
#based on final scores (both sides)
big_all <- big1 %>% #this time take both instances of all games
	select(team_location, team_score, opponent_team_location, opponent_team_score)
	
#include key in dataframe
big_all <- merge(big_all, big_key, by.x = "team_location", by.y = "team_location", all.x = TRUE)
big_all <- merge(big_all, big_key, by.x = "opponent_team_location", by.y = "team_location", all.x = TRUE)

#rename columns
colnames(big_all)[which(names(big_all) == "key")] <- "team_key"
colnames(big_all)[which(names(big_all) == "key.x")] <- "team_key"
colnames(big_all)[which(names(big_all) == "key.y")] <- "opponent_team_key"

#select columns
big_all <- big_all %>%
  select(all_of(c(
    "team_location",
    "team_score", 
    "opponent_team_location",
    "opponent_team_score", 
    "team_key",
    "opponent_team_key"
  )))
  
#empty 18x18 matrix
score_matrix <- matrix(0, nrow = 18, ncol = 18)

# Set row and column names for the matrix
rownames(score_matrix) <- colnames(score_matrix) <- 1:18

# Iterate over rows
for (i in 1:nrow(big_all)) {
  # Get team_key and opponent_team_key for current row
  team_key <- big_all $team_key[i]
  opponent_team_key <- big_all $opponent_team_key[i]
  
  # Increment the corresponding entry in the matrix
  score_matrix[opponent_team_key, team_key] <- score_matrix[opponent_team_key, team_key]+ big_all $team_score[i]
}
score_matrix

#normalize the rows
score_matrix <- t(apply(score_matrix, 1, function(row) row / sum(row)))

#actually I need the transpose of this
score_matrix <- t(score_matrix)


eigen(score_matrix)$values
eigen(score_matrix)$vectors[,1]

pagerank2 <- Re(eigen(score_matrix)$vectors[,1]/sum(eigen(score_matrix)$vectors[,1]))

ranked_teams2 <- cbind(big_teams, pagerank2)

# Order ranked_teams by pagerank
ranked_teams2 <- as.data.frame(ranked_teams2)
ranked_teams2$pagerank2 <- as.numeric(ranked_teams2$pagerank2)
ranked_teams2 <- ranked_teams2 %>% 
  arrange(desc(pagerank2))


print(ranked_teams2)



###############################################################################
#all ncaa d1

ncaa <- wbb_team_box %>% 
  group_by(team_location) %>% #only d1 teams remain in team_location
  filter(n() >= 5) %>%
  ungroup()

ncaa <- ncaa %>% 
  group_by(opponent_team_location) %>% #only d1 teams remain in team_location
  filter(n() >= 5) %>%
  ungroup()


ncaa_key <- cbind(team_location=unique(ncaa$team_location), key=c(1:360))

ncaa <- ncaa %>%
	filter(team_winner==TRUE) %>% #take one instance of each game only
	select(team_location, team_score, opponent_team_location, opponent_team_score)
	
#include key in dataframe
ncaa <- merge(ncaa, ncaa_key, by.x = "team_location", by.y = "team_location", all.x = TRUE)
ncaa <- merge(ncaa, ncaa_key, by.x = "opponent_team_location", by.y = "team_location", all.x = TRUE)

#rename columns
colnames(ncaa)[which(names(ncaa) == "key")] <- "team_key"
colnames(ncaa)[which(names(ncaa) == "key.x")] <- "team_key"
colnames(ncaa)[which(names(ncaa) == "key.y")] <- "opponent_team_key"

#select columns
ncaa <- ncaa %>%
  select(team_location, team_score, opponent_team_location, opponent_team_score, team_key, opponent_team_key)

#d1 teams only  
#remove rows with NA in opponent_team_key
ncaa <- ncaa[complete.cases(ncaa$opponent_team_key), ]

  
#rank based on wins/losses only

#create adjacency matrix
ncaa_matrix <- matrix(0, nrow = 360, ncol = 360)

# Set row and column names for the matrix
rownames(ncaa_matrix) <- colnames(ncaa_matrix) <- 1:360

# Iterate over rows of pac dataframe
for (i in 1:nrow(ncaa)) {
  # Get team_key and opponent_team_key for current row
  team_key <- ncaa$team_key[i]
  opponent_team_key <- ncaa$opponent_team_key[i]
  
  # Increment the corresponding entry in the matrix
  ncaa_matrix[opponent_team_key, team_key] <- ncaa_matrix[opponent_team_key, team_key] + 1
}
ncaa_matrix

#normalize the rows of result_matrix
ncaa_matrix <- t(apply(ncaa_matrix, 1, function(row) row / sum(row)))

#actually I need the transpose of this
ncaa_matrix <- t(ncaa_matrix)

#calculate eigenvalues
eigen(ncaa_matrix)$values
eigen(ncaa_matrix)$vectors[,1] #too small????

#convert pac_key to a dataframe
ncaa_key <- as.data.frame(ncaa_key, stringsAsFactors = FALSE)
colnames(ncaa_key) <- c("team_location", "key")


pagerank4 <- Re(eigen(ncaa_matrix)$vectors[,1]/sum(eigen(ncaa_matrix)$vectors[,1]))

ranked_teams4 <- cbind(ncaa_key$team_location, pagerank4)

#convert pagerank column to numeric
pagerank_numeric <- as.numeric(ranked_teams4[, 2])

# Order ranked_teams by pagerank
ranked_teams4 <- ranked_teams4[order(pagerank_numeric), ]
ranked_teams4 <- as.data.frame(ranked_teams4)
names(ranked_teams4) <- c("team_location", "pagerank")
ranked_teams4$pagerank <- as.numeric(ranked_teams4$pagerank)

###############################################################################
#former pac-12 teams only
pac_teams= c("Stanford", "California", "Utah", "Colorado", "USC", "UCLA", "Oregon", "Oregon State", "Washington", "Washington State", "Arizona", "Arizona State")

pac_rank <- ranked_teams4[ranked_teams4$team_location %in% pac_teams,]


################################################################################
#graph
library(igraph)
library(png)

graph <- graph_from_adjacency_matrix(result_matrix, mode = "directed", weighted = TRUE)

#convert pac_key to a dataframe
big_key <- as.data.frame(big_key, stringsAsFactors = FALSE)
colnames(big_key) <- c("team_location", "key")

# Set the vertex names of the graph using pac_key
V(graph)$name <- big_key$team_location

# Plot the graph
plot(graph,
      layout = layout_with_kk,
      vertex.color = "lightblue",
      vertex.frame.color = "lightblue",
      vertex.label.color = "black",
      vertex.label.cex = 1,
      vertex.label.dist = 0.5,
      vertex.size = 15,
      edge.arrow.size = 0.1,
      edge.width = 0.5,
      edge.color = "darkgray"
  )
  