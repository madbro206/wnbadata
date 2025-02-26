#Use wehoop package to download NCAAW data
#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, BasketballAnalyzeR, ggplot2, lubridate)

#unrivaled fourth quarters
unrivaled <- read.csv("~/Desktop/unrivaled_1v1.csv", header = TRUE)

#one of my column names had a typo in it :/
names(unrivaled)[names(unrivaled) == "h_sore"] <- "h_score"

#convert minutes to decimals
unrivaled <- unrivaled %>%
  filter(round != "" & length != "") %>%
  mutate(decimal_mins = case_when(
    length == "" ~ NA_real_,
    TRUE ~ period_to_seconds(ms(length)) / 60
  ))

mean(unrivaled$decimal_mins, na.rm=TRUE)
min(unrivaled$decimal_mins, na.rm=TRUE)

#boxplot for Unrivaled
ggplot(unrivaled, aes(y = decimal_mins, x = factor(""))) +
  geom_boxplot(fill = "lightblue", width = 0.3, outlier.shape = NA) +
  geom_jitter(width = 0.1, color = "purple", alpha = 0.7, size = 1.5) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(title = "Unrivaled 1v1 Game Lengths",
      subtitle = "not including final (different format)",
       y = "Duration (Minutes)",,
      caption="data: unrivaled.basketball | graphic: @wnbadata",
       x = NULL) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_blank())


#bubble plot
ggplot(unrivaled, aes(x=v_score, y=h_score, size = decimal_mins, color = round)) +
    geom_point(alpha=0.7)



unrivaled %>%
  mutate(
    higher_score = pmax(h_score, v_score),
    lower_score = pmin(h_score, v_score),
    score_source = if_else(h_score > v_score, "Home", "Away")
  ) %>%
  ggplot(aes(x = higher_score, y = lower_score, 
              size = decimal_mins, 
              color = interaction(round, score_source))) +
  geom_point(alpha = 0.7) +
  labs(x = "Higher Score", y = "Lower Score", 
        color = "Round & Score Source",
        size = "Minutes Played")
  


        
#total score vs game length scatterplot
unrivaled <- unrivaled %>% 
  mutate(total_score = h_score + v_score)

ggplot(unrivaled, aes(x = total_score, y = decimal_mins)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", color = "firebrick", se = FALSE) +  # <--关键修改
  labs(x = "Combined Score", 
       y = "Game Duration (Minutes)",
       title = "Unrivaled 1v1 Scores vs Game Length") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  scale_x_continuous(breaks = seq(12, 20, 1)) +
  theme(
    panel.grid = element_line(color = "grey90"),  # Keep gridlines
    panel.background = element_rect(fill = "white")  # White background
  )


        