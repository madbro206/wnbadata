library(dplyr)
library(stringr)
library(ggplot2)
library(wehoop)
library(lubridate)
library(ggrepel)

tictoc::tic()
progressr::with_progress({
  wnba_player_box <- load_wnba_player_box(season=c(2022:2026))
})
tictoc::toc()

# Now aggregate with clean names
Pbox <- wnba_player_box %>%
  group_by(athlete_display_name) %>%
  summarise(
    GP = sum(minutes > 0, na.rm = TRUE), 
    MIN = sum(minutes, na.rm = TRUE), 
    M3P = sum(three_point_field_goals_made, na.rm = TRUE), 
    A3P = sum(three_point_field_goals_attempted, na.rm = TRUE), 
    P3 = 100 * M3P / A3P
  ) %>%
  rename(Player = athlete_display_name) %>%
  as.data.frame()


Pbox_2026 <- wnba_player_box %>%
  filter(season==2026) %>%
  group_by(athlete_display_name) %>%
  summarise(
    GP = sum(minutes > 0, na.rm = TRUE), 
    MIN = sum(minutes, na.rm = TRUE), 
    M3P = sum(three_point_field_goals_made, na.rm = TRUE), 
    A3P = sum(three_point_field_goals_attempted, na.rm = TRUE), 
    P3 = 100 * M3P / A3P
  ) %>%
  rename(Player = athlete_display_name) %>%
  as.data.frame()


cat("\n===== TOP 10 BY RAW 3P% 2026 =====\n")
Pbox_2026 %>%
  select(Player, GP, M3P, A3P, P3) %>%
  arrange(desc(P3)) %>%
  head(10) %>%
  print()


# ===== EMPIRICAL BAYES MODEL =====
# Step 1: Estimate league-wide prior from all players
# Total makes and attempts across all players
league_total_3m <- sum(Pbox$M3P, na.rm = TRUE)
league_total_3a <- sum(Pbox$A3P, na.rm = TRUE)
league_avg_3p <- league_total_3m / league_total_3a

cat("\nLeague averages WNBA 2022-2026:\n")
cat(sprintf("Total 3PM: %d, Total 3PA: %d\n", league_total_3m, league_total_3a))
cat(sprintf("League average 3p%%: %.2f%%\n", league_avg_3p * 100))

# Set prior strength (pseudo-attempts)
# Higher = more shrinkage toward league average
# Lower = trust individual data more
N0 <- 75  

# Prior parameters for Beta distribution
alpha0 <- league_avg_3p * N0
beta0 <- (1 - league_avg_3p) * N0

cat(sprintf("\nPrior: Beta(%.2f, %.2f)\n", alpha0, beta0))
cat(sprintf("This is like assuming everyone starts with %.0f attempts at %.1f%%\n", 
            N0, league_avg_3p * 100))


# Step 2: Calculate posterior for each player
Pbox_eb <- Pbox_2026 %>%
  mutate(
    # Posterior parameters
    alpha_post = alpha0 + M3P,
    beta_post = beta0 + (A3P - M3P),
    
    # Posterior mean (empirical Bayes estimate)
    P3_EB = 100 * alpha_post / (alpha_post + beta_post),
    
    # Credible interval (90%)
    P3_lower = 100 * qbeta(0.05, alpha_post, beta_post),
    P3_upper = 100 * qbeta(0.95, alpha_post, beta_post),
    
    # How much shrinkage happened?
    shrinkage = P3 - P3_EB
  ) %>%
  arrange(desc(P3_EB))

# Step 3: Compare naive vs empirical Bayes
Pbox_eb$P3_EB<-round(Pbox_eb$P3_EB,2)
Pbox_eb$P3<-round(Pbox_eb$P3,2)


cat("\n===== TOP 10 FROM 3 BY EMPIRICAL BAYES =====\n")
Pbox_eb %>%
  select(Player, GP, M3P, A3P, P3_EB) %>%
  head(10) %>%
  print()

cat("\n===== BOTTOM 10 FROM 3 BY EMPIRICAL BAYES =====\n")
Pbox_eb %>%
  arrange(P3_EB) %>%
  select(Player, GP, M3P, A3P, P3_EB) %>%
  head(10) %>%
  print()


cat("\n===== ADJUSTED PERCENTAGES OF RAW TOP 10 =====\n")
Pbox_eb %>%
  arrange(desc(P3)) %>%
  select(Player, GP, M3P, A3P, P3_EB) %>%
  head(10) %>%
  print()

cat("\n===== BIGGEST SHRINKAGE (overrated by naive method) =====\n")
Pbox_eb %>%
  arrange(desc(shrinkage)) %>%
  select(Player, A3P, P3, P3_EB, shrinkage) %>%
  head(10) %>%
  print()


#Visualization - shrinkage effect
ggplot(Pbox_eb %>% filter(A3P > 0), 
       aes(x = A3P, y = shrinkage, color = A3P)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_gradient(low = "red", high = "darkblue", 
                       name = "3PA") +
  geom_text_repel(data = Pbox_eb %>% filter(abs(shrinkage) > 30 | A3P > 20),
                  aes(label = Player), size = 3, max.overlaps = 15) +
  labs(
    title = "Low-Volume Shooters Get Pulled Toward League Average",
    subtitle = sprintf("Prior = %.0f attempts at %.1f%%", N0, league_avg_3p * 100),
    x = "2026 Three-Point Attempts",
    y = "Shrinkage (Raw % - EB %)",
    caption = "Players above zero were overrated by raw%; below zero underrated | chart: @wnbadata | data: wehoop"
  ) +
  theme_minimal()




rank_comparison <- Pbox_eb %>%
  mutate(
    rank_raw = rank(-P3, ties.method = "first"),
    rank_eb = rank(-P3_EB, ties.method = "first"),
    rank_change = rank_raw - rank_eb
  ) %>%
  arrange(rank_eb) %>%
  select(Player, A3P, M3P, P3, rank_raw, P3_EB, rank_eb, rank_change) %>%
  head(15)

print(rank_comparison)
