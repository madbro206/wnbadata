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
  select(Player, Team, Season, G...4, MP...5, WS, PER, `eFG%`, `USG%`, ORtg, DRtg)

all_tables <- all_tables %>%
  rename(
    G = G...4,
    MP = MP...5,
    usg = `USG%`,
    eFG = `eFG%`
  ) %>%
  mutate(
    G = as.numeric(G),
    MP = as.numeric(MP),
    usg = as.numeric(usg),
    eFG = as.numeric(eFG),
    WS = as.numeric(WS),
    PER = as.numeric(PER),
    ORtg = as.numeric(ORtg),
    DRtg = as.numeric(DRtg)
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
  select(Player, Team, Season, G...4, MP...5, FG, `FG%`, `3P`, `3P%`, `2P%`, FT, `FT%`, AST, TOV, BLK, STL, TRB, PF, PTS)

all_tables22 <- all_tables2 %>%
  rename(
    G = G...4,
    MP = MP...5,
    FGp =`FG%`,
    P3=`3P`,
    P3p=`3P%`,
    P2p =`2P%`,
    FTp=`FT%`
  ) %>%
  mutate(
    G = as.numeric(G),
    MP = as.numeric(MP),
    FGp = as.numeric(FGp),
    P3 = as.numeric(P3),
    P3p = as.numeric(P3p),
    P2p = as.numeric(P2p),
    FTp = as.numeric(FTp),
    AST = as.numeric(AST),
    TOV = as.numeric(TOV),
    PF = as.numeric(PF),
    BLK = as.numeric(BLK),
    STL = as.numeric(STL),
    TRB = as.numeric(TRB),
    PTS= as.numeric(PTS)
  )

#####
#join all player stats
#####

joined_table <- all_tables %>%
  left_join(
    all_tables22, by = c("Player", "Team", "Season")
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
  rename(team_ORtg=ORtg,team_DRtg=DRtg,team_efg=eFG., team_tov=TOV.,team_ORB=ORB.,team_FT_ratio=FT.FGA, def_efg = eFG..1, def_tov_pct=TOV..1, opp_drb_pct=DRB., def_FT_ratio = FT.FGA.1, Season=X1997) %>%
  select(Team, Season, W, L, team_ORtg, team_DRtg, team_efg, team_tov,team_ORB,team_FT_ratio,def_efg, def_tov_pct, opp_drb_pct, def_FT_ratio)





all_tables3_clean
colnames(all_tables3_clean)

#####
#MVP data
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
cols_to_convert <- c("W", "L", "DRtg", "def_efg", "def_tov_pct", "opp_drb_pct", "def_FT_ratio",  "FG","FGp","P3","P3p","P2p","FT","team_ORtg","team_DRtg","team_efg","team_tov","team_ORB","team_FT_ratio")

total_table <- total_table %>%
  mutate(across(all_of(cols_to_convert), ~ as.numeric(.)))


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

