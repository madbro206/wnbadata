#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

#this loads all the packages I need at once, i could instead just do "library(wehoop)" for each package
pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, ggrapel)

#load data
#wnba full play by play
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp()
})
tictoc::toc()

#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box()
})
tictoc::toc()

#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box()
})
tictoc::toc()

wnba_active <- subset(wnba_player_box, did_not_play == FALSE & active == TRUE)
wnba_active$plus_minus <- as.numeric(as.character(wnba_active$plus_minus))

model <- lm(
  plus_minus ~ points + rebounds + assists + steals + blocks + turnovers + fouls,
  data = wnba_active
)
summary(model)


coefs <- coef(model)
scaled_coefs <- coefs / coefs["points"]
print(scaled_coefs)




# Assuming your model is called 'model' and your data is 'wnba_active'
wnba_active$predicted_plus_minus <- predict(model, newdata = wnba_active)

player_summary <- wnba_active %>%
  group_by(athlete_display_name) %>%
  summarize(
    actual_plus_minus = mean(plus_minus, na.rm = TRUE),
    predicted_plus_minus = mean(predicted_plus_minus, na.rm = TRUE)
  )


ggplot(wnba_active, aes(x = predicted_plus_minus, y = plus_minus)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(
      x = "Predicted Plus-Minus",
      y = "Actual Plus-Minus",
      title = "Predicted vs. Actual Plus-Minus by Player"
    ) +
    theme_minimal()
  

player_summary$residual <- player_summary$actual_plus_minus - player_summary$predicted_plus_minus
threshold <- 2 * sd(player_summary$residual, na.rm = TRUE)
outliers <- subset(player_summary, abs(residual) > threshold)

ggplot(player_summary, aes(x = predicted_plus_minus, y = actual_plus_minus)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_text_repel(
    data = outliers,
    aes(label = athlete_display_name),
    color = "blue",
    size = 3,
    max.overlaps = 10
  ) +
  labs(
    x = "Predicted (w/ box score stats) Plus-Minus",
    y = "Actual Plus-Minus (avg)",
    title = "Predicted vs. Actual Plus-Minus by Player",
    subtitle="WNBA 2025 through July 14"
  ) +
  theme_minimal() +
  coord_fixed(ratio = .3)