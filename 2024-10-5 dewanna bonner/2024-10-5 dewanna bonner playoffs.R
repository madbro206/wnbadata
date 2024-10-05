library(ggplot2)
library(dplyr)
library(tidyr)
library(ggtext)

data <- read.csv("~/Desktop/wehoop/2024-10-5 dewanna bonner/bonner_playoffs.csv")

plot_data <- data %>%
  select(Year, G, PTS, TRB, AST)

plot_data$Year <- as.numeric(as.character(plot_data$Year))

p <- ggplot(plot_data, aes(x = Year)) +
  geom_line(aes(y = PTS, color = "Points"), size = 1.5) +
  geom_line(aes(y = TRB, color = "Rebounds"), size = 1.5) +
  geom_line(aes(y = AST, color = "Assists"), size = 1.5) +
  geom_col(aes(y = G, fill = "Games Played"), alpha = 0.4) +
  
  # Set up dual y-axes
  scale_y_continuous(
    name = "Per Game Average",
    sec.axis = sec_axis(~./1, name = "Games Played")
  ) +
  
  # Set the x-axis scale
  scale_x_continuous(breaks = 2009:2024, labels = 2009:2024) +
  
  # Customize colors
  scale_color_manual(values = c("Points" = "orange", "Rebounds" = "blue", "Assists" = "dark gray")) +
  scale_fill_manual(values = c("Games Played" = "gray")) +
  
  # Customize theme and labels
  theme_minimal(base_size = 14) +  # Increase base font size
  labs(
    title = "DeWanna Bonner in the Playoffs",
    subtitle = "<span style='color:orange;'>Points</span>, <span style='color:blue;'>Rebounds</span>, and <span style='color:gray;'>Assists</span> per game",
    x = "Year",
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = rel(1.5), face = "bold"),  # 1.5 times the base size
    plot.subtitle = element_markdown(size = rel(1.1)),  # 1.1 times the base size
    axis.title = element_text(size = rel(1.2)),  # 1.2 times the base size
    axis.text = element_text(size = rel(1)),  # Same as base size
    axis.title.y.right = element_text(color = "gray", size = rel(1.2)),
    axis.text.y.right = element_text(color = "gray"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(1)),
    legend.position = "none"  # Remove the legend
  )

# Display the plot
print(p)