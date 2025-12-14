library(dplyr)
library(stringr)
library(ggplot2)
library(wehoop)
library(lubridate)

tictoc::tic()
progressr::with_progress({
  wnba_player_box <- load_wnba_player_box(season=c(2006:2025))
})
tictoc::toc()

breeze <- c("Aari McDonald", "Kate Martin", "Cameron Brink", "Dominique Malonga", "Rickea Jackson", "Paige Bueckers")

hive <- c("Monique Billings", "Saniya Rivers", "Natisha Hiedeman", "Ezi Magbegor", "Sonia Citron", "Kelsey Mitchell")

laces <- c("Jackie Young", "Alyssa Thomas", "Naz Hillmon", "Maddy Siegrist", "Jordin Canada", "Brittney Sykes")

owls <- c("Napheesa Collier", "Aaliyah Edwards", "Rebecca Allen", "Skylar Diggins", "Marina Mabrey", "Rachel Banham")

mist <-c("Breanna Stewart", "Arike Ogunbowale", "Veronica Burton", "Alanna Smith", "Li Yueru", "Allisha Gray")

phantom <- c("Tiffany Hayes", "Natasha Cloud", "Dana Evans", "Aliyah Boston", "Kiki Iriafen", "Kelsey Plum")

rose <- c("Shakira Austin", "Lexie Hull", "Sug Sutton", "Azura Stevens", "Chelsea Gray", "Kahleah Copper")

vinyl <- c("Brittney Griner", "Rae Burrell", "Erica Wheeler", "Dearica Hamby","Rhyne Howard","Courtney Williams")

unrivaled_players <- c(breeze, hive, laces, owls, mist, phantom, rose, vinyl)
# Create a standardization function for player names
standardize_name <- function(name) {
  name %>%
    str_trim() %>%  # Remove leading/trailing spaces
    str_replace_all("-Smith", "") %>%  # Skylar Diggins-Smith -> Skylar Diggins
    str_replace_all("-", " ") %>%  # Handle other hyphenated names
    str_squish()  # Remove extra internal spaces
}

# Standardize in the raw data BEFORE aggregating
wnba_player_box_clean <- wnba_player_box %>%
  mutate(athlete_display_name = standardize_name(athlete_display_name))

# Standardize Unrivaled roster names too
unrivaled_players_clean <- standardize_name(unrivaled_players)

# Now aggregate with clean names
Pbox <- wnba_player_box_clean %>%
  group_by(athlete_display_name) %>%
  summarise(
    GP = sum(minutes > 0, na.rm = TRUE), 
    MIN = sum(minutes, na.rm = TRUE), 
    FTM = sum(free_throws_made, na.rm = TRUE), 
    FTA = sum(free_throws_attempted, na.rm = TRUE), 
    FTp = 100 * FTM / FTA
  ) %>%
  rename(Player = athlete_display_name) %>%
  filter(Player %in% unrivaled_players_clean) %>%
  as.data.frame()

# ===== NAIVE METHOD =====
# Simple: who has the best career FT%?
cat("\nTop 10 by raw FT%:\n")
Pbox %>%
  arrange(desc(FTp)) %>%
  select(Player, FTM, FTA, FTp) %>%
  head(10)

# ===== EMPIRICAL BAYES MODEL =====
# Step 1: Estimate league-wide prior from all players
# Total makes and attempts across all Unrivaled players
league_total_ftm <- sum(Pbox$FTM, na.rm = TRUE)
league_total_fta <- sum(Pbox$FTA, na.rm = TRUE)
league_avg_ftp <- league_total_ftm / league_total_fta

cat("\nLeague averages (all Unrivaled players' WNBA history):\n")
cat(sprintf("Total FTM: %d, Total FTA: %d\n", league_total_ftm, league_total_fta))
cat(sprintf("League average FT%%: %.2f%%\n", league_avg_ftp * 100))

# Set prior strength (pseudo-attempts)
# Higher = more shrinkage toward league average
# Lower = trust individual data more
# Common choice: 100-200 for basketball FT data
N0 <- 150  

# Prior parameters for Beta distribution
alpha0 <- league_avg_ftp * N0
beta0 <- (1 - league_avg_ftp) * N0

cat(sprintf("\nPrior: Beta(%.2f, %.2f)\n", alpha0, beta0))
cat(sprintf("This is like assuming everyone starts with %.0f attempts at %.1f%%\n", 
            N0, league_avg_ftp * 100))


# Step 2: Calculate posterior for each player
Pbox_eb <- Pbox %>%
  mutate(
    # Posterior parameters
    alpha_post = alpha0 + FTM,
    beta_post = beta0 + (FTA - FTM),
    
    # Posterior mean (empirical Bayes estimate)
    FTp_EB = 100 * alpha_post / (alpha_post + beta_post),
    
    # Credible interval (90%)
    FTp_lower = 100 * qbeta(0.05, alpha_post, beta_post),
    FTp_upper = 100 * qbeta(0.95, alpha_post, beta_post),
    
    # How much shrinkage happened?
    shrinkage = FTp - FTp_EB
  ) %>%
  arrange(desc(FTp_EB))

# Step 3: Compare naive vs empirical Bayes
Pbox_eb$FTp_EB<-round(Pbox_eb$FTp_EB,2)
Pbox_eb$FTp<-round(Pbox_eb$FTp,2)
cat("\n===== TOP 10 BY EMPIRICAL BAYES =====\n")
Pbox_eb %>%
  select(Player, FTA, FTp, FTp_EB) %>%
  head(10) %>%
  print()

cat("\n===== BIGGEST SHRINKAGE (overrated by naive method) =====\n")
Pbox_eb %>%
  filter(FTA >= 50) %>%  # Only players with reasonable sample
  arrange(desc(shrinkage)) %>%
  select(Player, FTA, FTp, FTp_EB, shrinkage) %>%
  head(10) %>%
  print()


# Step 4: Visualization - shrinkage effect
ggplot(Pbox_eb, aes(x = FTA, y = shrinkage)) +
  geom_point(aes(color = FTp), size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_gradient2(low = "red", mid = "yellow", high = "green", 
                        midpoint = 80, name = "Raw FT%") +
  labs(
    title = "Empirical Bayes Shrinkage",
    subtitle = "Low-attempt players get pulled toward league average",
    x = "Career FT Attempts",
    y = "Shrinkage (Raw FT% - EB FT%)",
    caption = sprintf("Prior strength: %.0f pseudo-attempts", N0)
  ) +
  theme_minimal()


# Step 5: Confidence intervals plot
ggplot(Pbox_eb %>% head(15), aes(x = reorder(Player, FTp_EB), y = FTp_EB)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = FTp_lower, ymax = FTp_upper), width = 0.2) +
  geom_point(aes(y = FTp), color = "red", alpha = 0.5, size = 2) +  # Raw % in red
  coord_flip() +
  labs(
    title = "Top 15 Players: EB Estimate with 90% Credible Intervals",
    subtitle = "Black = EB estimate, Red = raw percentage",
    x = NULL,
    y = "Free Throw %"
  ) +
  theme_minimal()


# ===== VIZ 1: Histogram of raw FT% with Beta prior overlay =====

# Create a sequence for the prior curve
x_seq <- seq(0, 1, length.out = 1000)
prior_density <- dbeta(x_seq, alpha0, beta0)

# Scale the prior density to match histogram scale
# (histogram is in count, density is in probability)
prior_density_scaled <- prior_density * nrow(Pbox) * 0.01  # Adjust 0.01 based on binwidth

ggplot(Pbox, aes(x = FTp/100)) +
  geom_histogram(aes(y = after_stat(count)), 
                 binwidth = 0.02, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_line(data = data.frame(x = x_seq, y = prior_density_scaled),
            aes(x = x, y = y), 
            color = "red", linewidth = 1.5) +
  geom_vline(xintercept = league_avg_ftp, 
             linetype = "dashed", color = "darkred", linewidth = 1) +
  annotate("text", x = league_avg_ftp + 0.03, y = Inf, 
           label = sprintf("League Avg: %.1f%%", league_avg_ftp * 100),
           vjust = 1.5, color = "darkred", size = 4) +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0.55, 0.95)) +
  labs(
    title = "Distribution of Career FT% Among Unrivaled Players",
    subtitle = sprintf("Red curve: Beta(%.1f, %.1f) prior from empirical data", alpha0, beta0),
    x = "Free Throw Percentage",
    y = "Number of Players",
    caption = sprintf("Prior strength = %.0f pseudo-attempts", N0)
  ) +
  theme_minimal(base_size = 13)


# ===== VIZ 2: Density plot with prior overlay (smoother version) =====

ggplot(Pbox, aes(x = FTp/100)) +
  geom_density(fill = "skyblue", alpha = 0.5, linewidth = 1) +
  stat_function(fun = dbeta, args = list(shape1 = alpha0, shape2 = beta0),
                color = "red", linewidth = 1.5, linetype = "solid") +
  geom_vline(xintercept = league_avg_ftp, 
             linetype = "dashed", color = "darkred", linewidth = 1) +
  annotate("text", x = league_avg_ftp - 0.04, y = Inf, 
           label = sprintf("Prior mean: %.1f%%", league_avg_ftp * 100),
           vjust = 1.5, color = "darkred", size = 4.5, fontface = "bold") +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0.55, 0.95)) +
  labs(
    title = "Empirical Bayes Prior vs. Observed Distribution",
    subtitle = "Blue = actual player distribution | Red = Beta prior we're using",
    x = "Free Throw Percentage",
    y = "Density"
  ) +
  theme_minimal(base_size = 13)


# ===== BONUS: Side-by-side comparison table for TikTok =====

comparison_table <- Pbox_eb %>%
  arrange(desc(FTp)) %>%
  mutate(Rank_Raw = row_number()) %>%
  arrange(desc(FTp_EB)) %>%
  mutate(Rank_EB = row_number(),
         Rank_Change = Rank_Raw - Rank_EB) %>%
  select(Player, FTA, Rank_Raw, Rank_EB, Rank_Change, FTp, FTp_EB) %>%
  head(15)

print(comparison_table)

# Who benefits most from EB?
cat("\nBiggest movers UP (EB likes them more than raw %):\n")
comparison_table %>%
  arrange(desc(Rank_Change)) %>%
  head(5) %>%
  print()

cat("\nBiggest fallers (EB trusts them less):\n")
comparison_table %>%
  arrange(Rank_Change) %>%
  head(5) %>%
  print()

# ===== VIZ 4: Attempts vs Uncertainty (interval width) =====

Pbox_eb <- Pbox_eb %>%
  mutate(interval_width = FTp_upper - FTp_lower)

ggplot(Pbox_eb, aes(x = FTA, y = interval_width)) +
  geom_point(aes(color = FTp_EB), size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_gradient2(low = "red", mid = "yellow", high = "green",
                        midpoint = 80, name = "EB FT%") +
  scale_x_log10() +
  labs(
    title = "More Attempts = More Certainty",
    subtitle = "90% credible interval width shrinks with sample size",
    x = "Career FT Attempts (log scale)",
    y = "90% Credible Interval Width (%)",
    caption = "Each point is one Unrivaled player"
  ) +
  theme_minimal(base_size = 13)
