pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, tidyr, ggrepel, factoextra)

#player per-game data 
unrivaled <- read.csv("~/Desktop/unrivaled_player_box.csv", header = TRUE)

# select (numeric) columns that we care about, classic statline data
numeric_columns <- c("PTS", "REB", "AST", "STL", "BLK")
unrivaled_numeric <- unrivaled[, numeric_columns]

#scale the numeric data
scaled_data <- scale(unrivaled_numeric)

############ find optimal number of clusters ##############
#finding optimal k

#elbow method
fviz_nbclust(unrivaled_numeric, kmeans, method = "wss")

#silhouette method
fviz_nbclust(unrivaled_numeric, kmeans, method = "silhouette")

#gap statistic method
fviz_nbclust(unrivaled_numeric, kmeans, method = "gap_stat")


################# create clusters #############
kmeans_result <- kmeans(scaled_data, centers = 2)
unrivaled$cluster <- kmeans_result$cluster

############### summary ###################
#how many rows in each cluster
table(unrivaled$cluster)

# Calculate means for specific columns by cluster and round to 2 decimal places
cluster_means <- aggregate(cbind(PTS, REB, AST, STL, BLK) ~ cluster, 
                         data = unrivaled, 
                         FUN = function(x) round(mean(x), 2))

cluster_means

cluster_stats <- unrivaled %>%
  group_by(cluster) %>%
  summarise(across(c(PTS, REB, AST, STL, BLK),
                  list(mean = ~round(mean(.), 2),
                       sd = ~round(sd(.), 2))))
cluster_stats


# Reshape the data from wide to long format
long_data <- unrivaled %>%
  select(cluster, PTS, REB, AST, STL, BLK) %>%
  pivot_longer(cols = c(PTS, REB, AST, STL, BLK),
               names_to = "Statistic",
               values_to = "Value")

# Create the box plot with overlapping boxes
ggplot(long_data, aes(x = Statistic, y = Value, fill = factor(cluster))) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7) +
  labs(title = "Box Plot of Statistics by Cluster",
       x = "Statistic",
       y = "Value",
       fill = "Cluster") +
  theme_minimal()
############# plots ##################
cbp1 <- c("#2e5dcb", "#7700e6")

#choose two variables to plot clusters in
ggplot(unrivaled, aes(x = PTS, y = REB, color = factor(cluster))) +
  geom_point() +
  scale_color_manual(values = cbp1) +
  geom_text_repel(aes(label = Player), size = 3, max.overlaps = 10) +
  scale_x_continuous(breaks = seq(0, max(unrivaled$PTS), by = 5)) +
  scale_y_continuous(breaks = seq(0, max(unrivaled$REB), by = 5)) +
  labs(title = "K-means Clustering of Unrivaled Player Games",
      subtitle = "Through 1/24",
       x = "Points",
       y = "Rebounds",
       color = "Cluster") +
  theme_minimal()


ggplot(unrivaled, aes(x = PTS, y = AST, color = factor(cluster))) +
  geom_point() +
  scale_color_manual(values = cbp1) +
  geom_text_repel(aes(label = Player), size = 3, max.overlaps = 10) +
  scale_x_continuous(breaks = seq(0, max(unrivaled$PTS), by = 5)) +
  scale_y_continuous(breaks = seq(0, max(unrivaled$REB), by = 5)) +
  labs(title = "K-means Clustering of Unrivaled Players",
       subtitle = "Through 1/24",
       x = "Points",
       y = "Assists",
       color = "Cluster") +
  theme_minimal()

ggplot(unrivaled, aes(x = REB, y = AST, color = factor(cluster))) +
  geom_point() +
  scale_color_manual(values = cbp1) +
  geom_text_repel(aes(label = Player), size = 3, max.overlaps = 10) +
  scale_x_continuous(breaks = seq(0, max(unrivaled$PTS), by = 5)) +
  scale_y_continuous(breaks = seq(0, max(unrivaled$REB), by = 5)) +
  labs(title = "K-means Clustering of Unrivaled Players",
       subtitle = "Through 1/24",
       x = "Rebounds",
       y = "Assists",
       color = "Cluster") +
  theme_minimal()





#crazy 3d plot with pts, reb, ast
# Using scatterplot3d (static plot)
library(scatterplot3d)

# Create 3D scatter plot
scatterplot3d(unrivaled$PTS, 
              unrivaled$REB, 
              unrivaled$AST,
              color = cbp1[unrivaled$cluster], 
              pch = 16,                       # solid circle point type
              main = "Unrivaled Player Games Through 1/24",
              xlab = "Points",
              ylab = "Rebounds",
              zlab = "Assists")

# Using plotly (interactive plot)
library(plotly)

plot_ly(data = unrivaled, 
        x = ~PTS, 
        y = ~REB, 
        z = ~AST,
        color = ~factor(cluster),
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 5)) %>%
        layout(scene = list(
          xaxis = list(title = "Points"),
          yaxis = list(title = "Rebounds"),
          zaxis = list(title = "Assists")))


subset(unrivaled, cluster==2)
