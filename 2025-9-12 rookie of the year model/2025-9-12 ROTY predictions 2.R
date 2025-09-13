#WNBA rookie of the year predictor

#my original wnba dpoy predictor inspired by https://www.linkedin.com/pulse/predicting-nba-defensive-player-year-dpoy-using-data-science-khatkar-0uh6c/?trackingId=SFEQ2rO2f9PhJGv67uirwQ%3D%3D
library(rvest)
library(purrr)
library(dplyr)

#####
#get player rookie season stats
#####



#####
#ROY data
#####

url <- "https://www.basketball-reference.com/wnba/awards/mvp.html"
page <- read_html(url)
mvp_table <- html_table(page, fill = TRUE)[[1]]  


# Extract the first row as header
new_header <- unlist(mvp_table[1, ])

# Remove the first row and set the names
mvp_table_fixed <- mvp_table[-1, ] %>%
  # Assign new column names
  setNames(new_header) %>%
  # Optionally convert to tibble again
  as_tibble()

# Convert to tibble with correct types if necessary
mvp_table_fixed <- as_tibble(mvp_table_fixed)

# Now you can extract the Year and Player columns
mvp_key <- mvp_table_fixed %>%
  select(Year, Player) %>%
  mutate(
    Year = as.numeric(Year),
    Player = as.character(Player)
  ) %>%
  rename(Season = Year)



# Add dpoy indicator to joined_table
joined_table <- joined_table %>%
  mutate(mvp = 0) %>%                                     # default to 0
  left_join(mvp_key %>% mutate(mvp = 1),                 # flag mvp winners with 1
            by = c("Player", "Season")) %>%
  mutate(mvp = ifelse(is.na(mvp.y), mvp.x, mvp.y)) %>% # replace NA with 0
  select(-mvp.x, -mvp.y)                                 # drop duplicates



#####
#logistic regression with player stats only
#####

mvp_model <- glm(mvp ~ G.x + MP.x + WS + PER + eFG + usg + ORtg + DRtg + FG + FGp + P3+ P3p + P2p + FT + FTp + AST + TOV + BLK + STL + TRB + PF + PTS + team_ORtg+team_DRtg+team_efg+team_tov+team_ORB+team_FT_ratio+def_efg+def_tov_pct+opp_drb_pct+def_FT_ratio, data = total_table, family=binomial)

summary(mvp_model)


total_table <- total_table %>%
  mutate(mvp_prob = predict(mvp_model, newdata = total_table, type = "response")) %>%
  group_by(Season) %>%
  mutate(mvp_prob_norm = mvp_prob / sum(mvp_prob, na.rm=TRUE) * 100) %>%
  arrange(desc(mvp_prob_norm)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

accuracy_check <- total_table %>%
  group_by(Season) %>%
  filter(Season != 2025) %>% #2025 season isn't done yet, so can't be checked
  slice_max(mvp_prob_norm, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  summarise(
    correct = sum(mvp == 1),
    total = n(),
    accuracy = correct / total * 100
  )

cat("✅ Model correctly predicted MVP in", accuracy_check$correct, "of", accuracy_check$total, "seasons.\n")
cat("🎯 Accuracy:", round(accuracy_check$accuracy, 1), "%\n")


total_table %>%
  filter(Season == 2025) %>%
  select(Player, Team, WS, mvp_prob_norm) %>%
  arrange(desc(mvp_prob_norm)) %>%
  head(10)

#####################
#get accurate seasons
###############
# Filter seasons except 2025 and get the top predicted DPOY by normalized probability per season
mvp_predictions <- total_table %>%
  filter(Season != 2025) %>%
  group_by(Season) %>%
  slice_max(mvp_prob_norm, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Season, Player, mvp_prob_norm, mvp) %>%
  mutate(
    Predicted = Player,
    Predicted_mvp = 1
  )

# Get actual DPOY winners for each season
mvp_actual <- total_table %>%
  filter(mvp == 1, Season != 2025) %>%
  select(Season, Actual = Player, Actual_mvp = mvp)

# Join predictions and actuals for comparison
mvp_compare <- mvp_predictions %>%
  left_join(mvp_actual, by = "Season") %>%
  select(
    Season,
    Predicted,
    mvp_prob_norm,
    Actual,
    Predicted_mvp,
    Actual_mvp
  ) %>%
  arrange(Season)

# View the comparison table
print(mvp_compare, n=28)

