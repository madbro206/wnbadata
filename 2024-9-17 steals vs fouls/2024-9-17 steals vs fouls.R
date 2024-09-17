library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(ggprepel)
	
#data retrieved sept 16 2024 https://www.basketball-reference.com/wnba/years/2024_play_by_play.html
data <- read.csv("~/Desktop/wehoop/2024-9-17 steals vs fouls/steals_vs_fouls.csv", header = TRUE)

data$def_nonshooting_foul <- data$PF-data$Shoot_foul-data$Off_foul

data <- data %>%
	arrange(desc(data$MP))

ggplot(data, aes(x=STL, y=def_nonshooting_foul, size=MP)) + 
    geom_point(alpha=0.5, color='#f88158') +
    scale_size_continuous(range=c(0.1, 8), breaks=c(200,400,600,800,1000)) +
    geom_text_repel(
        aes(label = Player),
        data = subset(data, 
                      STL > quantile(STL, 0.95) | 
                      STL < quantile(STL, 0.05) |
                      def_nonshooting_foul > quantile(def_nonshooting_foul, 0.95) |
                      def_nonshooting_foul < quantile(def_nonshooting_foul, 0.05)),
        size = 3,
        box.padding = 0.5,
        point.padding = 0.5,
        force = 2
    ) +
    labs(title="Player steals vs non-shooting defensive fouls",
         x="Total Steals",
         y="Total Non-Shooting Defensive Fouls",
         subtitle="2024 WNBA season through 9/15",
         size="Minutes played",
         caption="stats: basketball-reference | graphic: @wnbadata") +
    theme_minimal() +
    coord_fixed(ratio = 1, xlim = c(0, 85), ylim = c(0, 85)) +
    scale_x_continuous(breaks = seq(0, 85, by = 10)) +
    scale_y_continuous(breaks = seq(0, 85, by = 10))
    
cor(data$STL, data$def_nonshooting_foul)
cor(data$MP, data$def_nonshooting_foul)
cor(data$STL, data$MP)