#starting code: https://r-graph-gallery.com/136-stacked-area-chart.html

library(ggplot2)
library(dplyr)
library(lemon)
library(ggtext)
library(teamcolors)
 
# create data
time <- as.numeric(rep(seq(1,7),each=7))  # x Axis
value <- runif(49, 10, 100)               # y Axis
group <- rep(LETTERS[1:7],times=7)        # group, one shape per group
data <- data.frame(time, value, group)

# stacked area chart
ggplot(data, aes(x=time, y=value, fill=group)) + 
    geom_area()

data <- data  %>%
  group_by(time, group) %>%
  summarise(n = sum(value)) %>%
  mutate(percentage = n / sum(n))

# Plot
ggplot(data, aes(x=time, y=percentage, fill=group)) + 
    geom_area(alpha=0.6 , linewidth =1, colour="black")

###############################################################################################################
#stacked bar chart, turned out to not be very interesting

#load assist data
assists <- read.csv("~/Desktop/wehoop/2024-8-17 assists by team/wnba_team_assists.csv", header=TRUE)

#wnbacolors
colors <- subset(teamcolors, league=='wnba')

data2 <- assists  %>%
  filter(Year >=2018) %>% #starting in 2018 because then there's no name changes hehe
  group_by(Year, Team) %>%
  summarise(n = sum(Ast)) %>%
  mutate(percentage = n / sum(n))
  
data3 <- data2 %>% 
	left_join(colors, by = c("Team" = "name"))
	
team_colors <- setNames(data3$secondary, data3$Team)

# plot with team colors
ggplot(data3, aes(x=Year, y=percentage, fill=Team)) + 
    geom_area(alpha=0.6, linewidth=1, colour="black") + 
   # geom_text(aes(label=Team, color=Team), position=position_nudge(x = 0.1),  size=3, hjust=0) +
    scale_fill_manual(values = team_colors) + 
    labs(
    title = "WNBA team assists as percentage of total league assists",
    subtitle = "regular season only",
    caption = "Source: basketball-reference.com | Graphic: @wnbadata | Adapted from original at r-graph-gallery.com"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.margin = margin(20, 30, 20, 20),
    plot.background = element_rect(fill = "grey90", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(size = 12), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_markdown(size = 16, lineheight = 1.1, color = "grey20"),
    plot.subtitle = element_markdown(size = 14, lineheight = 1.1, color = "grey40", margin = margin(0, 0, 20, 0)),
    plot.caption = element_markdown(size = 8, color = "grey60", margin = margin(20, 0, 0, 0),)
  ) +theme_minimal()
    
###############################################################################################################
#team assists vs wins

data5 <- read.csv("~/Desktop/wehoop/2024-8-17 assists by team/wnba_team_assists_2024.csv", header=TRUE)

data5 <- data5 %>% 
	left_join(colors, by = c("TEAM" = "name"))

# Update the 'mascot' column where the value is "Vegas Aces"
data5$mascot[data5 $mascot == "Vegas Aces"] <- "Aces"

ggplot(data5, aes(x = AST, y = WIN., label=mascot, color=secondary)) +
  geom_point(aes(color=primary)) +
  geom_text(vjust = -1, size=3.5) + 
  labs(title = "Relationship Between Team AST Win Percentage", subtitle = "WNBA Teams, 2024 Season",
  x = "AST Per Game", y = "Win Percentage", caption= "stats.wnba.com | @wnbadata") +
  scale_color_identity() + # Use the exact colors specified in 'primary'
  theme_minimal() +
  theme(legend.position="none")




#team assist percentage vs wins
data4 <- read.csv("~/Desktop/wehoop/2024-8-17 assists by team/team_ast_pct.csv", header=TRUE)

#join with colors
data4 <- data4 %>% 
	left_join(colors, by = c("TEAM" = "name"))

# Update the 'mascot' column where the value is "Vegas Aces"
data4$mascot[data4$mascot == "Vegas Aces"] <- "Aces"

#calculate win percentage
data4$win_percentage <- data4$W / data4$GP 
data4$FGM..AST <- data4$FGM..AST/100

#percentage fgm assisted vs win percentage 2024
ggplot(data4, aes(x = FGM..AST, y = win_percentage, label=mascot, color=secondary)) +
  geom_point(aes(color=primary)) +
  geom_text(vjust = -1, size=3.5) + 
  labs(title = "Relationship Between FGM Assisted and Win Percentage", subtitle = "WNBA Teams, 2024 Season",
  x = "FGM Assisted (%)", y = "Win Percentage", caption= "stats.wnba.com | @wnbadata") +
  scale_color_identity() + # Use the exact colors specified in 'primary'
  theme_minimal() +
  theme(legend.position="none")

