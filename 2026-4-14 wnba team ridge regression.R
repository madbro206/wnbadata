#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html
# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

#this loads all the packages I need at once, i could instead just do "library(wehoop)" for each package
pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, ggplot2, glmnet)

#load data
#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(season=c(2016:2025))
})
tictoc::toc()

wnba_active <- subset(wnba_player_box, did_not_play == FALSE & active == TRUE)
wnba_active$plus_minus <- as.numeric(as.character(wnba_active$plus_minus))

#regression with regularization
#matrix
X <- model.matrix(
  plus_minus ~ points + rebounds + assists + steals + blocks + turnovers + fouls,
  data = wnba_active
)[, -1]

y <- wnba_active$plus_minus

#cross-validated ridge
set.seed(123)
ridge_cv <- cv.glmnet(
  X, y,
  alpha = 0, #0 = ridge
  nfolds = 10,
  standardize = TRUE
)

#best lambda
ridge_cv$lambda.min

#final model at best lambda
ridge_model <- glmnet(X, y, alpha = 0, lambda = ridge_cv$lambda.min)

#convert coefs to a numeric named vector
coefs <- coef(ridge_model)
coefs_vec <- as.numeric(coefs)
names(coefs_vec) <- rownames(coefs)

#scale
scaled_coefs <- coefs_vec / coefs_vec["points"]
scaled_coefs


#get predictions
#use the same design matrix for prediction
ridge_preds <- predict(
  ridge_model,
  newx = X,
  s = ridge_cv$lambda.min,   #same lambda used to fit ridge model
  type = "response"
)

#predict() returns a matrix; coerce to numeric vector
wnba_active$predicted_plus_minus_ridge <- as.numeric(ridge_preds)

player_summary_ridge <- wnba_active %>%
  group_by(athlete_display_name) %>%
  summarize(
    actual_plus_minus = mean(plus_minus, na.rm = TRUE),
    predicted_plus_minus = mean(predicted_plus_minus_ridge, na.rm = TRUE)
  )

#plot predicted vs actual plus minus
ggplot(wnba_active, aes(x = predicted_plus_minus_ridge, y = plus_minus)) +
  geom_point(size=0.8, color= "#6821f5", alpha=0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Predicted Plus-Minus",
    y = "Actual Plus-Minus",
    title = "Predicted vs. Actual Plus-Minus by Player (Ridge Regression)",
    subtitle ="each point represents one player game, WNBA 2016-2025"
  ) +
  theme_minimal()


#coefficient plot
coef_df <- data.frame(
  stat = names(scaled_coefs),
  scaled = as.numeric(scaled_coefs)
) %>%
  filter(stat != "(Intercept)")

ggplot(coef_df, aes(x = reorder(stat, scaled), y = scaled)) +
  geom_col(fill = "#B897D4") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(
    aes(label = round(scaled, 2)),
    hjust = ifelse(coef_df$scaled >= 0, -0.1, 1.1)
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = ~ c(-max(abs(.)) * 1.1, max(abs(.)) * 1.1)
  ) +
  labs(
    x = "Box Score Stat",
    y = "Effect Relative to Points",
    title = "Player box score impact toward plus minus",
    subtitle = "scaled so that points = 1"
  ) +
  theme_minimal()

