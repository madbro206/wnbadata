#wnba dpoy predictor inspired by https://www.linkedin.com/pulse/predicting-nba-defensive-player-year-dpoy-using-data-science-khatkar-0uh6c/?trackingId=SFEQ2rO2f9PhJGv67uirwQ%3D%3D
library(rvest)
library(purrr)
library(dplyr)

#####
#Get advanced stats
#####
years <- 1997:2025

base_url <- "https://www.basketball-reference.com/wnba/years/"
suffix <- "_advanced.html"

#function to read one season's table
read_season_table <- function(year) {
  url <- paste0(base_url, year, suffix)
  Sys.sleep(4)  #slow down
  #read the advanced stats table
  tbl <- tryCatch(
    read_html(url) %>% html_table(fill = TRUE) %>% .[[1]],
    error = function(e) NULL
  )
  if (!is.null(tbl)) {
    tbl$Season <- year  #add season column
  }
  return(tbl)
}

#Scrape all years
all_tables <- map(years, read_season_table)

all_tables <- Filter(Negate(is.null), all_tables)

#shove together
all_tables <- bind_rows(all_tables)

all_tables <- all_tables %>%
  filter(Team != "Team" & Team !="TOT") %>%
  select(Player, Team, Season, G...4, MP...5, 'BLK%', 'STL%', DWS)

all_tables <- all_tables %>%
  rename(
    G = G...4,
    MP = MP...5,
    BLK_pct = `BLK%`,
    STL_pct = `STL%`
  ) %>%
  mutate(
    G = as.numeric(G),
    MP = as.numeric(MP),
    BLK_pct = as.numeric(BLK_pct),
    STL_pct = as.numeric(STL_pct),
    DWS = as.numeric(DWS)
  )


#####
#Get defensive box score stats
#####

suffix2<- "_totals.html"

# Function to read one season's table
read_season_table2 <- function(year) {
  url <- paste0(base_url, year, suffix2)
  Sys.sleep(4)  # Pause before each request

  tbl <- tryCatch(
    read_html(url) %>% html_table(fill = TRUE) %>% .[[1]],
    error = function(e) NULL
  )
  if (!is.null(tbl)) {
    tbl$Season <- year  # Add year as a column for merging
  }
  return(tbl)
}


# Scrape all years and bind together
all_tables2 <- map(years, read_season_table2)
all_tables2 <- Filter(Negate(is.null), all_tables2)
all_tables2 <- bind_rows(all_tables2)

all_tables2 <- all_tables2 %>%
  filter(Team != "Team" & Team !="TOT") %>%
  select(Player, Team, Season, G...4, MP...5, BLK, STL, TRB, ORB)

all_tables2 <- all_tables2 %>%
  rename(
    G = G...4,
    MP = MP...5
  ) %>%
  mutate(
    G = as.numeric(G),
    MP = as.numeric(MP),
    BLK = as.numeric(BLK),
    STL = as.numeric(STL),
    TRB = as.numeric(TRB),
    ORB = as.numeric(ORB),
    DRB=TRB-ORB
  )

#####
#join all player stats
#####

joined_table <- all_tables %>%
  left_join(
    all_tables2 %>% 
      select(Player, Team, Season, BLK, STL, TRB, ORB, DRB),  # exclude G, MP from second table
    by = c("Player", "Team", "Season")
  )

joined_table <- joined_table %>%
  mutate(Player = gsub("\\*", "", Player))
  

#####
#Get team stats
#####

suffix3<- ".html"


# Function to read one season's table
read_season_table3 <- function(year) {
  url <- paste0(base_url, year, suffix3)
  Sys.sleep(4)  # Pause before each request
  if(year == 2025) {
    tbl <- tryCatch(
      read_html(url) %>% html_table(fill = TRUE) %>% .[[710]], #advanced stats is 7th table
      error = function(e) NULL
    )
  }
  # Read the advanced stats table 
  if(year %in% c(1997:2015)) {
    tbl <- tryCatch(
      read_html(url) %>% html_table(fill = TRUE) %>% .[[7]], #advanced stats is 7th table
      error = function(e) NULL
    )
  } else{
    tbl <- tryCatch(
      read_html(url) %>% html_table(fill = TRUE) %>% .[[8]], #8th table
      error = function(e) NULL
    )
  }
  if (!is.null(tbl)) {
    tbl$Season <- year  # Add year as a column for merging
  }
  return(tbl)
}

# Scrape all years and bind together
all_tables3 <- map(years, read_season_table3)

# Set new column names using the first row
all_tables3_clean[] <- lapply(all_tables3_clean, as.character)
colnames(all_tables3_clean) <- as.character(unlist(all_tables3_clean[1, ]))

# Remove the first row (now that it's used for headers)
all_tables3_clean <- all_tables3_clean[-1, ]

all_tables3_clean <- Filter(Negate(is.null), all_tables3)
all_tables3_clean <- bind_rows(all_tables3_clean)

colnames(all_tables3_clean) <- as.character(unlist(all_tables3_clean[1, ]))

# Remove the first row (now that it's used for headers)
all_tables3_clean <- all_tables3_clean[-1, ]

# Replace NA or empty string names with default names
names(all_tables3_clean) <- make.names(names(all_tables3_clean), unique = TRUE)

# Remove trailing asterisks from team_name
all_tables3_clean$Team <- gsub("\\*$", "", all_tables3_clean$Team)


all_tables3_clean <- all_tables3_clean %>% 
  filter(Rk != "" & Rk != "Rk", na.rm=TRUE) %>%
  rename(def_efg = eFG..1, def_tov_pct=TOV..1, opp_drb_pct=DRB., def_FT_ratio = FT.FGA.1, Season=X1997) %>%
  select(Team, Season, W, L, DRtg, def_efg, def_tov_pct, opp_drb_pct, def_FT_ratio)





all_tables3_clean
colnames(all_tables3_clean)

#####
#dpoy data
#####

url <- "https://www.basketball-reference.com/wnba/awards/dpoy.html"
page <- read_html(url)
dpoy_table <- html_table(page, fill = TRUE)[[1]]   # adjust index if needed

# Extract table but treat first row as data, so skip it and then assign proper column names
dpoy_data <- dpoy_table[-1, ]  # Remove first row (header row treated as data)

# Set the proper column names from the first row of original dpoy_table
colnames(dpoy_data) <- as.character(unlist(dpoy_table[1, ]))

# Convert to tibble with correct types if necessary
dpoy_data <- as_tibble(dpoy_data)

# Now you can extract the Year and Player columns
dpoy_key <- dpoy_data %>%
  select(Year, Player) %>%
  mutate(
    Year = as.numeric(Year),
    Player = as.character(Player)
  ) %>%
  rename(Season = Year)


# Add dpoy indicator to joined_table
joined_table <- joined_table %>%
  mutate(dpoy = 0) %>%                                     # default to 0
  left_join(dpoy_key %>% mutate(dpoy = 1),                 # flag dpoy winners with 1
            by = c("Player", "Season")) %>%
  mutate(dpoy = ifelse(is.na(dpoy.y), dpoy.x, dpoy.y)) %>% # replace NA with 0
  select(-dpoy.x, -dpoy.y)                                 # drop duplicates


joined_table_clean <- joined_table %>%
  filter(
    !is.na(G) & 
    !is.na(MP) & 
    !is.na(BLK_pct) & 
    !is.na(STL_pct) & 
    !is.na(BLK) & 
    !is.na(STL) & 
    !is.na(DRB)
  )


#####
#join player and team data
#####

team_key <- tibble::tribble(
  ~Team_Full,               ~Team_Abbr,
  "Houston Comets",         "HOU",
  "Phoenix Mercury",        "PHO",
  "New York Liberty",       "NYL",
  "Los Angeles Sparks",     "LAS",
  "Cleveland Rockers",      "CLE",
  "Charlotte Sting",        "CHA",
  "Sacramento Monarchs",    "SAC",
  "Utah Starzz",            "UTA",
  "Indiana Fever",          "IND",
  "Atlanta Dream",          "ATL",
  "Detroit Shock","DET",
  "Washington Mystics", "WAS",
  "Orlando Miracle", "ORL",
  "Minnesota Lynx", "MIN",
  "Portland Fire", "POR",
  "Miami Sol", "MIA",
  "Seattle Storm", "SEA",
  "Connecticut Sun", "CON",
  "San Antonio Silver Stars", "SAS",
  "Chicago Sky", "CHI",
  "Tulsa Shock", "TUL",
  "Dallas Wings", "DAL",
  "Las Vegas Aces", "LVA",
  "Golden State Valkyries", "GSV"
)

all_tables3_clean <- all_tables3_clean %>%
  left_join(team_key, by = c("Team" = "Team_Full")) %>%
  mutate(Team = Team_Abbr) %>%
  select(-Team_Abbr)

total_table <- joined_table %>%
  inner_join(all_tables3_clean, by = c("Team", "Season"))


# Convert multiple columns from character to numeric at once
cols_to_convert <- c("W", "L", "DRtg", "def_efg", "def_tov_pct", "opp_drb_pct", "def_FT_ratio")

total_table <- total_table %>%
  mutate(across(all_of(cols_to_convert), ~ as.numeric(.)))


#####
#logistic regression with player stats only
#####

model <- glm(dpoy ~ G + MP + BLK_pct + STL_pct + BLK + STL + BLK*STL + DRB + DWS, data = joined_table_clean, family=binomial)

summary(model)


joined_table_clean <- joined_table_clean %>%
  mutate(dpoy_prob = predict(model, newdata = joined_table_clean, type = "response")) %>%
  group_by(Season) %>%
  mutate(dpoy_prob_norm = dpoy_prob / sum(dpoy_prob) * 100) %>%
  arrange(desc(dpoy_prob_norm)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

accuracy_check <- joined_table_clean %>%
  group_by(Season) %>%
  filter(Season != 2025) %>% #2025 season isn't done yet, so can't be checked
  slice_max(dpoy_prob_norm, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  summarise(
    correct = sum(dpoy == 1),
    total = n(),
    accuracy = correct / total * 100
  )

cat("✅ Model correctly predicted DPOY in", accuracy_check$correct, "of", accuracy_check$total, "seasons.\n")
cat("🎯 Accuracy:", round(accuracy_check$accuracy, 1), "%\n")


joined_table_clean %>%
  filter(Season == 2025) %>%
  select(Player, Team, STL, BLK, DRB, DWS, dpoy_prob_norm) %>%
  arrange(desc(dpoy_prob_norm)) %>%
  head(10)


joined_table_clean %>%
  filter(Player=="DiJonai Carrington")

#####
#trying with test set
#####

train_set <- joined_table %>% filter(Season >= 1997 & Season <= 2015)
test_set  <- joined_table %>% filter(Season >= 2016)

model_sub <- glm(dpoy ~ G + MP + BLK_pct + STL_pct + BLK + STL + BLK*STL + DRB + DWS ,data = train_set, family = binomial)

test_set <- test_set %>%
  mutate(dpoy_prob = predict(model_sub, newdata = test_set, type = "response"))

accuracy_test <- test_set %>%
  group_by(Season) %>%
  slice_max(dpoy_prob, n = 1, with_ties = FALSE) %>%
  summarise(
    correct = sum(dpoy == 1),
    total = n(),
    accuracy = correct / total * 100
  )

cat("🎯 Accuracy on test years:", round(accuracy_test$accuracy, 1), "%\n")

##############
#get accurate seasons
###############
# Filter seasons except 2025 and get the top predicted DPOY by normalized probability per season
dpoy_predictions <- joined_table_clean %>%
  filter(Season != 2025) %>%
  group_by(Season) %>%
  slice_max(dpoy_prob_norm, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Season, Player, dpoy_prob_norm, dpoy) %>%
  mutate(
    Predicted = Player,
    Predicted_dpoy = 1
  )

# Get actual DPOY winners for each season
dpoy_actual <- joined_table_clean %>%
  filter(dpoy == 1, Season != 2025) %>%
  select(Season, Actual = Player, Actual_dpoy = dpoy)

# Join predictions and actuals for comparison
dpoy_compare <- dpoy_predictions %>%
  left_join(dpoy_actual, by = "Season") %>%
  select(
    Season,
    Predicted,
    dpoy_prob_norm,
    Actual,
    Predicted_dpoy,
    Actual_dpoy
  ) %>%
  arrange(Season)

# View the comparison table
print(dpoy_compare, n=28)



#####
#logistic regression with TEAM STATS TOO
#####

model2 <- glm(dpoy ~ G + MP + BLK_pct + STL_pct + BLK + STL + DRB + DWS+ DRtg+ def_efg + def_tov_pct+ opp_drb_pct+ def_FT_ratio, data = total_table, family=binomial)

summary(model2)


total_table_probs <- total_table %>%
  mutate(dpoy_prob = predict(model2, newdata = total_table, type = "response")) %>%
  group_by(Season) %>%
  mutate(dpoy_prob_norm = dpoy_prob / sum(dpoy_prob, na.rm=TRUE) * 100) %>%
  arrange(desc(dpoy_prob_norm)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

accuracy_check2 <- total_table_probs %>%
  group_by(Season) %>%
  filter(Season != 2025) %>% #2025 season isn't done yet, so can't be checked
  slice_max(dpoy_prob_norm, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  summarise(
    correct = sum(dpoy == 1),
    total = n(),
    accuracy = correct / total * 100
  )

cat("✅ Model correctly predicted DPOY in", accuracy_check2$correct, "of", accuracy_check2$total, "seasons.\n")
cat("🎯 Accuracy:", round(accuracy_check2$accuracy, 1), "%\n")

#2025 probabilities
total_table_probs %>%
  filter(Season == 2025) %>%
  select(Player, Team, STL, BLK, DRB, DWS, DRtg, dpoy_prob_norm) %>%
  arrange(desc(dpoy_prob_norm)) %>%
  head(10)

##############
#get accurate seasons
###############
# Filter seasons except 2025 and get the top predicted DPOY by normalized probability per season
dpoy_predictions2 <- total_table_probs %>%
  filter(Season != 2025) %>%
  group_by(Season) %>%
  slice_max(dpoy_prob_norm, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Season, Player, dpoy_prob_norm, dpoy) %>%
  mutate(
    Predicted = Player,
    Predicted_dpoy = 1
  )

# Get actual DPOY winners for each season
dpoy_actual2 <- total_table %>%
  filter(dpoy == 1, Season != 2025) %>%
  select(Season, Actual = Player, Actual_dpoy = dpoy)

# Join predictions and actuals for comparison
dpoy_compare2 <- dpoy_predictions2 %>%
  left_join(dpoy_actual2, by = "Season") %>%
  select(
    Season,
    Predicted,
    dpoy_prob_norm,
    Actual) %>%
  arrange(Season)

# View the comparison table
print(dpoy_compare2, n=28)
