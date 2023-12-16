#opening tip winner vs game winner

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr)

#wnba full play by play
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp(season=c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023))
})
tictoc::toc()


#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season=c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023))
})

tictoc::toc()


#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(season=c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023))
})
tictoc::toc()

#find the opening tips for each wnba game
tip <- subset(wnba_pbp, period_number==1 & type_text=="Jumpball" & clock_display_value=="10:00")

#the key info is found here
#tip$text

library(stringr)

#create a new column "PlayerNames" with the extracted names
#it turns out the earlier games don't have this format in the jumpball row so rip
tip$PlayerNames <- str_extract(tip$text, "\\((.*?)\\sgains possession\\)")

#remove the parentheses and "gains possession" from the extracted names
tip$PlayerNames <- gsub("\\(|\\)|\\sgains possession", "", tip$PlayerNames)

#split the names into first and last names
tip[, c("FirstName", "LastName")] <- str_split_fixed(tip$PlayerNames, " ", 2)

#remove the rows with NA (being lazy for now)
tip <- tip[complete.cases(tip$PlayerNames), ]

library(dplyr)

#merge the data frames based on the athlete_display_name column and game_id column
merged_data <- tip %>%
  left_join(wnba_player_box, by = c("PlayerNames" = "athlete_display_name", "game_id"="game_id"))

#if tip winner was game winner it says true
merged_data$team_winner

#count how many are true
counts_table <- table(merged_data$team_winner)

print(counts_table)


#OVERTIME SECTION
#ot_tip <- subset(wnba_pbp, period_number==5 & type_text=="Jumpball" & clock_display_value=="5:00")

#find all overtimes, take the last overtime tip for each overtime game
library(dplyr)

#Group by game_id and find the maximum period_number
max_period_df <- wnba_pbp %>%
  group_by(game_id) %>%
  summarize(max_period = max(period_number))

#filter rows based on conditions, need it to be overtime game (max_period>4) and take the LAST (max) overtime for each game
ot_tip <- wnba_pbp %>%
  left_join(max_period_df, by = "game_id") %>%
  filter((max_period > 4) & (type_text == "Jumpball") & (clock_display_value == "5:00") & (period_number==max_period))

#extract name of player that gains possession# Assuming your dataframe is named result_df

library(stringr)
#define a regular expression pattern to extract the player's name before "gains possession"
pattern <- "([[:alpha:]]+ [[:alpha:]]+|[[:alpha:]]+)(?= gains possession)"

#apply the pattern to the "text" column
ot_tip$player_gaining_possession <- str_extract(result_df$text, pattern)

#merge with wnba_player_box by name of player gaining possession
merge <- ot_tip %>%
  left_join(wnba_player_box, by = c("player_gaining_possession" = "athlete_display_name", "game_id"="game_id"))

#now the column "team_winner" in "merge" will tell us if the team gaining first possession in the last overtime period had won or not
table(merge$team_winner)
