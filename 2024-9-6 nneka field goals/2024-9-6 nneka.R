library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
	
#data https://www.basketball-reference.com/wnba/players/o/ogwumnn01w.html
data <- read.csv("~/Desktop/wehoop/2024-9-5 nneka field goals/nneka.csv", header = TRUE)

# Assuming your data is already in a data frame called 'data'

# Calculate misses
data$Misses <- data$X3PA - data$X3P

# Select relevant columns
chart_data <- data[, c("Year", "X3P", "Misses")]

# Reshape data to long format
long_data <- pivot_longer(chart_data, cols = c("X3P", "Misses"), names_to = "Type", values_to = "Count")

a <- ggplot(long_data, aes(x = factor(Year), y = Count, fill = Type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("X3P" = "#7bb34e", "Misses" = "#f88158"),
                    labels = c("X3P" = "Makes", "Misses" = "Misses")) +
  labs(caption = "source: basketball-reference | graphic: @wnbadata",
       x = "Season",
       y = "3-pointers attempted",
       fill = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
data$X3P. <- as.numeric(data$X3P.)

# Plot 3pt percentages
b <-ggplot(data , aes(x=Year, y=X3P.)) +
      geom_vline(aes(xintercept = as.numeric(Year)), color = "lightgray", linetype = "solid") +
    geom_line() +
    geom_point()+
    geom_text(aes(label = round(X3P., 2)), vjust = -0.5, size = 2.3) +  # Add labels
    labs(title="Nneka's 3-pointers by season", y="3pt percentage", x=NULL)+
  scale_x_discrete(labels = as.character(data$Year)) +  # Ensure all years are labeled
   scale_y_continuous(limits = c(0, NA)) +  # Start y-axis at 0
     theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# display chart (patchwork package)
b/a