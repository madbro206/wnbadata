#inspo and original code: https://github.com/gkaramanis/FIBA-Basketbal-World-Cup/blob/master/R/shooting-stats.R

library(tidyverse)
library(ggtext)
library(lemon)
library(teamcolors)

totalstats <- read.csv("~/Desktop/wehoop/2024-08-20 is it easier to shoot in france?/gw_shot_stats.csv", header=TRUE)

shot_stats <- totalstats %>%
  select(Year, freethrows_percentage, twopoints_percentage, threepoints_percentage) %>% 
  gather("type", "percentage", freethrows_percentage, twopoints_percentage, threepoints_percentage) %>% 
  mutate(
    team_color = case_when(#colors for highlighting specific teams
      Year == "2023" ~ "#2C5234", #storm green
      Year == "Paris_Olympics" ~ "#17548C", #french blue
      Year == "w_career" ~ "#FFA500", #wnba orange
      TRUE ~ "grey60" #make all the rest grey
    )
  )

shot_stats$type <- factor(shot_stats$type, c("freethrows_percentage", "twopoints_percentage", "threepoints_percentage"))
 
ggplot(shot_stats, aes(x = type, y = percentage, group = Year, color = team_color, alpha = (team_color != "grey60"))) +
  geom_pointline(stroke = 0, linesize = 4, size = 5, linejoin = "mitre") +
  # geom_point() +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  scale_x_discrete(labels = c("Free Throws", "Two-pointers", "Three-pointers")) +
  scale_color_identity() +
  scale_alpha_manual(values = c(0.15, 1)) +
  labs(
    title = "Is it easier to shoot in France?",
    #subtitle = "Gabby Williams shooting in <span style='color:#236192'>**the 2024 Paris Olympics (6 games)**</span>, <span style='color:#236192'>**the 2023 WNBA Season (10 games)**</span>, and <span style='color:#236192'>**her WNBA career (135 games)**</span>",
    caption = "Source: basketball-reference.com & fiba.basketball | Adapted from original by Georgios Karamanis | @wnbadata"
  ) +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 30, 20, 20),
    plot.background = element_rect(fill = "grey90", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()#,
    #plot.title = element_markdown(size = 16, lineheight = 1.1, color = "grey20"),
    #plot.subtitle = element_markdown(size = 14, lineheight = 1.1, color = "grey40", margin = margin(0, 0, 20, 0)),
    #plot.caption = element_markdown(size = 6, color = "grey60", margin = margin(20, 0, 0, 0)
  )
  
