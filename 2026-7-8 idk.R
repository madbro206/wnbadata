# ============================
# Libraries
# ============================
library(wehoop)
library(progressr)
library(tictoc)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)

# ============================
# 0. Load data
# ============================
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season = c(2021:2026))
})
tictoc::toc()

wnba_team_box_efg <- wnba_team_box %>%
  mutate(
    efg = (field_goals_made + 0.5 * three_point_field_goals_made) / field_goals_attempted
  )

game_level <- wnba_team_box_efg |>
  select(
    game_id, game_date,
    team_id, team_name, team_abbreviation,
    team_home_away, team_score, efg,
    total_turnovers, offensive_rebounds, defensive_rebounds,
    free_throws_made, free_throws_attempted
  ) |>
  pivot_wider(
    id_cols = c(game_id, game_date),
    names_from = team_home_away,
    values_from = c(
      team_id, team_name, team_abbreviation,
      team_score, efg,
      total_turnovers, offensive_rebounds, defensive_rebounds, 
      free_throws_made, free_throws_attempted
    ),
    names_sep = "_"
  ) |>
  mutate(
    margin_home = team_score_home - team_score_away,
    efg_diff_home = efg_home - efg_away,
    to_diff_home = total_turnovers_home - total_turnovers_away,
    orb_diff_home = offensive_rebounds_home - offensive_rebounds_away,
    drb_diff_home = defensive_rebounds_home - defensive_rebounds_away,
    fta_diff_home = free_throws_attempted_home - free_throws_attempted_away,
    ftm_diff_home = free_throws_made_home - free_throws_made_away
  )

cor_efg_margin <- cor(
  game_level$efg_diff_home,
  game_level$margin_home,
  use = "complete.obs"
)

cor_efg_margin

ggplot(game_level, aes(x = efg_diff_home, y = margin_home)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Home eFG% - Away eFG%",
    y = "Home margin (points)",
    title = "Game-level relationship between eFG% differential and margin"
  )


lm_fit <- lm(
  margin_home ~ efg_diff_home + 
    to_diff_home + 
    orb_diff_home + 
    drb_diff_home+ 
    fta_diff_home +
    factor(team_id_home) + 
    factor(team_id_away),
  data = game_level
)

summary(lm_fit)





#dream vs valkyries july 4, 2026
this_game <- game_level |>
  filter(game_id == 401857039)

this_game |>
  select(efg_diff_home, margin_home,
         to_diff_home, orb_diff_home, fta_diff_home)


         this_game <- this_game |>
          mutate(
            fitted_margin = predict(lm_fit, newdata = this_game),
            residual = margin_home - fitted_margin
          )
        this_game |> select(margin_home, fitted_margin, residual)



#add fitted values and residuals for all games
game_level_with_resid <- game_level |>
  mutate(
    fitted_margin = predict(lm_fit, newdata = game_level),
    residual = margin_home - fitted_margin,
    abs_resid = abs(residual)
  )



# summary
efg_summary <- game_level |>
  summarize(
    mean_diff   = mean(efg_diff_home, na.rm = TRUE),
    sd_diff     = sd(efg_diff_home, na.rm = TRUE),
    median_diff = median(efg_diff_home, na.rm = TRUE),
    min_diff    = min(efg_diff_home, na.rm = TRUE),
    max_diff    = max(efg_diff_home, na.rm = TRUE)
)

efg_summary


dream_valks <- game_level |>
  filter(game_id == 401857039) 

dream_valks$efg_diff_home

#how much is dream game an outliar
dream_z <- (dream_valks$efg_diff_home - efg_summary$mean_diff) /
  efg_summary$sd_diff
dream_z


ggplot(game_level, aes(x = efg_diff_home)) +
  geom_histogram(bins = 30, color = "white") +
  geom_vline(
    xintercept = dream_valks$efg_diff_home,
    color = "red", linewidth = 1.2
  ) +
  annotate(
    "text",
    x = dream_valks$efg_diff_home,
    y = 150, 
    label = "ATL vs GSV\n≈ 1 SD worse\nthan typical\neFG% gap",
    color = "red",
    hjust = 1.1, 
    vjust = 1,
    size = 4
  ) +
  labs(
    x = "Home eFG% − Away eFG%",
    y = "Number of games",
    title = "Distribution of eFG% differentials in WNBA games",
    subtitle = "WNBA team box scores, 2021–2026",
    caption = "data: wehoop | chart: @wnbadata"
  ) +
  theme_minimal()


ggplot(wnba_team_box_efg, aes(x = efg)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    x = "Team eFG% (single game)",
    y = "Number of team-games",
    title = "Distribution of single-game eFG% in WNBA"
  ) +
  theme_minimal()


dream_row <- wnba_team_box_efg |>
  filter(game_id == 401857039, team_name == "Dream") 

dream_efg <- dream_row$efg
dream_efg

ggplot(wnba_team_box_efg, aes(x = efg)) +
  geom_histogram(bins = 30, color = "white") +
  geom_vline(
    xintercept = dream_efg,
    color = "red", linewidth = 1.2
  ) +
  labs(
    x = "Team eFG% (single game)",
    y = "Number of team-games",
    title = "Distribution of single-game eFG% in WNBA"
  ) +
  theme_minimal()


efg_overall_summary <- wnba_team_box_efg |>
  summarize(
    mean_efg   = mean(efg, na.rm = TRUE),
    sd_efg     = sd(efg, na.rm = TRUE),
    median_efg = median(efg, na.rm = TRUE)
  )

dream_z_efg <- (dream_efg - efg_overall_summary$mean_efg) /
  efg_overall_summary$sd_efg

dream_percentile <- mean(wnba_team_box_efg$efg <= dream_efg, na.rm = TRUE)

dream_percentile





four_factors_table <- tibble::tibble(
  factor = c(
    "eFG% advantage",
    "Turnover advantage",
    "Offensive rebound advantage",
    "Defensive rebound advantage",
    "Free-throw attempt advantage"
  ),
  points_of_margin = c(
    1.05,  # 105.45 * 0.01
    1.14,  # |−1.13852| per extra TO
    0.76,
    0.46,
    0.26
  )
)

four_factors_table