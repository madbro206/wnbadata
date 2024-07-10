#Use wehoop package to download NCAAW data
#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr)

tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp()
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box()
})
tictoc::toc()

###########################################################################
#general setup
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


###############################################################################


#make pie
pie(ranked_teams$pagerank, labels = ranked_teams$pac_teams, main = "PageRank Distribution of WNBA Teams 2024")


#make plot
plot(ranked_teams$pagerank, type = "p", pch = 19, 
     ylab = "Team", xlab = "Ranking", main = "Team Rankings")
text(ranked_teams$pagerank, ranked_teams$team_location, 
     labels = ranked_teams$team_location, pos = 4, cex = 0.7, xpd = TRUE)



###############################################################################
#old stuff (variable names not correct) below
###############################################################################

#based on final scores (both sides)

pac1 <- wbb_team_box[ #select only in-conference pac-12 games
  wbb_team_box$team_location %in% pac_teams &
  wbb_team_box$opponent_team_location %in% pac_teams,
]

pac_all <- pac1 %>% #this time take both instances of all games
	select(team_location, team_score, opponent_team_location, opponent_team_score)
	
#include key in dataframe
pac_all <- merge(pac_all, pac_key, by.x = "team_location", by.y = "team_location", all.x = TRUE)
pac_all <- merge(pac_all, pac_key, by.x = "opponent_team_location", by.y = "team_location", all.x = TRUE)

#rename columns
colnames(pac_all)[which(names(pac_all) == "key")] <- "team_key"
colnames(pac_all)[which(names(pac_all) == "key.x")] <- "team_key"
colnames(pac_all)[which(names(pac_all) == "key.y")] <- "opponent_team_key"

#select columns
pac_all <- pac_all %>%
  select(team_location, team_score, opponent_team_location, opponent_team_score, team_key, opponent_team_key)


#empty 12x12 matrix
score_matrix <- matrix(0, nrow = 12, ncol = 12)

# Set row and column names for the matrix
rownames(score_matrix) <- colnames(score_matrix) <- 1:12

# Iterate over rows
for (i in 1:nrow(pac_all)) {
  # Get team_key and opponent_team_key for current row
  team_key <- pac_all$team_key[i]
  opponent_team_key <- pac_all$opponent_team_key[i]
  
  # Increment the corresponding entry in the matrix
  score_matrix[opponent_team_key, team_key] <- score_matrix[opponent_team_key, team_key]+ pac_all$team_score[i]
}
score_matrix

#normalize the rows
score_matrix <- t(apply(score_matrix, 1, function(row) row / sum(row)))

#actually I need the transpose of this
score_matrix <- t(score_matrix)


eigen(score_matrix)$values
eigen(score_matrix)$vectors[,1]

pagerank2 <- Re(eigen(score_matrix)$vectors[,1]/sum(eigen(score_matrix)$vectors[,1]))

ranked_teams2 <- cbind(pac_teams, pagerank2)

# Order ranked_teams by pagerank
ranked_teams2 <- as.data.frame(ranked_teams2)
ranked_teams2$pagerank <- as.numeric(ranked_teams2$pagerank2)
ranked_teams2 <- ranked_teams2 %>% 
  arrange(desc(pagerank2))
ranked_teams2

graph2 <- graph_from_adjacency_matrix(score_matrix, mode = "directed", weighted = TRUE)
plot(graph2, 
     layout = layout_with_fr, 
     vertex.color = "light blue",
     vertex.frame.color = "black",
     vertex.label.color = "black", 
     vertex.size = 30,
     edge.arrow.mode = 2,  # Adjust arrow mode (see explanation below)
     edge.arrow.size = 0.5,  # Adjust arrow size as needed
     edge.color = "black")
     

###############################################################################
#based on final score differential

pac1 <- wbb_team_box[ #select only in-conference pac-12 games
  wbb_team_box$team_location %in% pac_teams &
  wbb_team_box$opponent_team_location %in% pac_teams,
]

pac_all <- pac1 %>% #this time take both instances of all games
	select(team_location, team_score, opponent_team_location, opponent_team_score)
	
#include key in dataframe
pac_all <- merge(pac_all, pac_key, by.x = "team_location", by.y = "team_location", all.x = TRUE)
pac_all <- merge(pac_all, pac_key, by.x = "opponent_team_location", by.y = "team_location", all.x = TRUE)

#rename columns
colnames(pac_all)[which(names(pac_all) == "key")] <- "team_key"
colnames(pac_all)[which(names(pac_all) == "key.x")] <- "team_key"
colnames(pac_all)[which(names(pac_all) == "key.y")] <- "opponent_team_key"

#select columns
pac_all <- pac_all %>%
  select(team_location, team_score, opponent_team_location, opponent_team_score, team_key, opponent_team_key)


#mpty 12x12 matrix
diff_matrix <- matrix(0, nrow = 12, ncol = 12)

# Set row and column names for the matrix
rownames(diff_matrix) <- colnames(diff_matrix) <- 1:12

# Iterate over rows
for (i in 1:nrow(pac_all)) {
  # Get team_key and opponent_team_key for current row
  team_key <- pac_all$team_key[i]
  opponent_team_key <- pac_all$opponent_team_key[i]
  
  # Increment the corresponding entry in the matrix
  diff_matrix[opponent_team_key, team_key] <- diff_matrix[opponent_team_key, team_key]+ pac_all$team_score[i]-pac_all$opponent_team_score[i]
}

#no negative entries
diff_matrix[diff_matrix < 0]<- 0

#normalize the rows
diff_matrix <- t(apply(diff_matrix, 1, function(row) row / sum(row)))

#actually I need the transpose of this
diff_matrix <- t(diff_matrix)


eigen(diff_matrix)$values
eigen(diff_matrix)$vectors[,1]

pagerank3 <- Re(eigen(diff_matrix)$vectors[,1]/sum(eigen(diff_matrix)$vectors[,1]))

ranked_teams3 <- cbind(pac_teams, pagerank3)

# Order ranked_teams by pagerank
ranked_teams3 <- ranked_teams3[order(as.numeric(ranked_teams3[, 2])), ]
ranked_teams3 <- as.data.frame(ranked_teams3)
ranked_teams3$pagerank <- as.numeric(ranked_teams3$pagerank3)
ranked_teams3 <- ranked_teams3 %>% 
  arrange(desc(pagerank3))
ranked_teams3



###############################################################################
#draw graphs
library(igraph)
library(png)

graph <- graph_from_adjacency_matrix(result_matrix, mode = "directed", weighted = TRUE)

#convert pac_key to a dataframe
pac_key <- as.data.frame(pac_key, stringsAsFactors = FALSE)
colnames(pac_key) <- c("team_location", "key")

# Set the vertex names of the graph using pac_key
V(graph)$name <- pac_key$team_location

# Plot the graph
plot(graph, 
     layout = layout_with_fr, 
     vertex.color = "light blue",
     vertex.frame.color = "black",
     vertex.label.color = "black", 
     vertex.size = 30,
     edge.arrow.mode = 2,  # Adjust arrow mode (see explanation below)
     edge.arrow.size = 0.5,  # Adjust arrow size as needed
     edge.color = "black")
     
     
     
#graph with images
layout <- layout_with_fr(graph)

# Plot the graph
plot(graph, 
     layout = layout, 
     vertex.color = "light blue",
     vertex.frame.color = "black",
     vertex.label.color = "black", 
     vertex.size = 30,
     edge.arrow.mode = 2,  # Adjust arrow mode (see explanation below)
     edge.arrow.size = 0.5,  # Adjust arrow size as needed
     edge.color = "black")

# Overlay images on the vertices
# Specify the directory where the images are located
image_directory <- "~/Desktop/wehoop/pagerank_youtube_video/"

for (i in 1:vcount(graph)) {
  img <- readPNG(paste0(image_directory, V(graph)$name[i], ".png"))  # Read the image
  rasterImage(img, layout[i, 1] - 0.5, layout[i, 2] - 0.5, layout[i, 1] + 0.5, layout[i, 2] + 0.5)
}


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

#since south carolina is undefeated, add a 1 in its row so matrix is not singular
#51
ncaa_matrix[51,51] <- 1

#normalize the rows of result_matrix
ncaa_matrix <- t(apply(ncaa_matrix, 1, function(row) row / sum(row)))

#actually I need the transpose of this
ncaa_matrix <- t(ncaa_matrix)

#calculate eigenvalues
eigen(ncaa_matrix)$values
eigen(ncaa_matrix)$vectors[,1] #too small????


graph2 <- graph_from_adjacency_matrix(ncaa_matrix, mode = "directed", weighted = TRUE)

#convert pac_key to a dataframe
ncaa_key <- as.data.frame(ncaa_key, stringsAsFactors = FALSE)
colnames(ncaa_key) <- c("team_location", "key")

# Set the vertex names of the graph using pac_key
V(graph2)$name <- ncaa_key $team_location

# Plot the graph
plot(graph2, 
     layout = layout_with_fr, 
     vertex.color = "light blue",
     vertex.size = 1,
     edge.arrow.mode = 2,  # Adjust arrow mode (see explanation below)
     edge.arrow.size = 0.5,  # Adjust arrow size as needed
     edge.color = "black")


#??

pagerank4 <- Re(eigen(ncaa_matrix)$vectors[,1]/sum(eigen(ncaa_matrix)$vectors[,1]))

ranked_teams4 <- cbind(ncaa_key$team_location, pagerank4)

#convert pagerank column to numeric
pagerank_numeric <- as.numeric(ranked_teams[, 2])

# Order ranked_teams by pagerank
ranked_teams <- ranked_teams[order(pagerank_numeric), ]