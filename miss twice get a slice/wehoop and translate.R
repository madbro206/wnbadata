#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, stringr)

##############################################################################################################
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


#equivalent in NCAAW:
tictoc::tic()
progressr::with_progress({
  wbb_pbp <- wehoop::load_wbb_pbp()
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_team_box <- wehoop::load_wbb_team_box()
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box()
})
tictoc::toc()

##############################################################################################################
#translate data

Pbox <- wnba_player_box %>%
  group_by(season, team_location, athlete_display_name) %>%
  filter(season_type==2)%>% #regular season only
  summarise(GP=sum(minutes>0, na.rm=TRUE), MIN=sum(minutes, na.rm = TRUE), PTS=sum(points, na.rm = TRUE),
    P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), P2p=100*P2M/P2A,
    P3M=sum(three_point_field_goals_made, na.rm = TRUE), P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), P3p=100*P3M/P3A,
    FTM=sum(free_throws_made, na.rm = TRUE), FTA=sum(free_throws_attempted, na.rm = TRUE), FTp=100*FTM/FTA,
    OREB=sum(offensive_rebounds, na.rm = TRUE), DREB=sum(defensive_rebounds, na.rm = TRUE), AST=sum(assists, na.rm = TRUE),
    TOV=sum(turnovers, na.rm = TRUE), STL=sum(steals, na.rm = TRUE), BLK=sum(blocks, na.rm = TRUE),
    PF=sum(fouls, na.rm = TRUE)) %>%
  rename(Season=season, Team=team_location,
         Player=athlete_display_name) %>%
         filter(GP>5) %>%
  as.data.frame()


Tbox <- wnba_team_box %>%
  group_by(season, team_location) %>%
  filter(season_type==2)%>% #regular season only
  summarise(GP=n(), 
            PTS=sum(team_score, na.rm = TRUE),
            W=sum(team_winner==TRUE), 
            L=sum(team_winner==FALSE), 
            P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), 
            P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), 
            P2p=100*P2M/P2A, 
            P3M=sum(three_point_field_goals_made, na.rm = TRUE), 
            P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), 
            P3p=100*P3M/P3A, 
            FTM=sum(free_throws_made, na.rm = TRUE), 
            FTA=sum(free_throws_attempted, na.rm = TRUE), 
            FTp=100*FTM/FTA, 
            OREB=sum(offensive_rebounds, na.rm = TRUE), 
            DREB=sum(defensive_rebounds, na.rm = TRUE), 
            AST=sum(assists, na.rm = TRUE), 
            TOV=sum(turnovers, na.rm = TRUE), 
            STL=sum(steals, na.rm = TRUE), 
            BLK=sum(blocks, na.rm = TRUE), 
            PF=sum(fouls, na.rm = TRUE), 
            PM=sum(team_score-opponent_team_score, na.rm=TRUE)) %>%
  rename(Season=season, Team=team_location) %>%
  as.data.frame()
  

Obox <- wnba_team_box %>%
  group_by(season, opponent_team_location) %>%
  filter(season_type==2)%>% #regular season only
  summarise(GP=n(), 
            PTS=sum(team_score, na.rm = TRUE),
            W=sum(team_winner==TRUE), 
            L=sum(team_winner==FALSE), 
            P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), 
            P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), 
            P2p=100*P2M/P2A, 
            P3M=sum(three_point_field_goals_made, na.rm = TRUE), 
            P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), 
            P3p=100*P3M/P3A, 
            FTM=sum(free_throws_made, na.rm = TRUE), 
            FTA=sum(free_throws_attempted, na.rm = TRUE), 
            FTp=100*FTM/FTA, 
            OREB=sum(offensive_rebounds, na.rm = TRUE), 
            DREB=sum(defensive_rebounds, na.rm = TRUE), 
            AST=sum(assists, na.rm = TRUE), 
            TOV=sum(turnovers, na.rm = TRUE), 
            STL=sum(steals, na.rm = TRUE), 
            BLK=sum(blocks, na.rm = TRUE), 
            PF=sum(fouls, na.rm = TRUE), 
            PM=sum(team_score-opponent_team_score, na.rm=TRUE)) %>%
  rename(Season=season, Team=opponent_team_location) %>%
  as.data.frame()
  
Obox_home <- wnba_team_box %>%
  group_by(season, opponent_team_location) %>%
  filter(season_type==2)%>% #regular season only
  filter(team_home_away=="away")%>% #home games only (opponents are away)
  summarise(GP=n(), 
            PTS=sum(team_score, na.rm = TRUE),
            W=sum(team_winner==TRUE), 
            L=sum(team_winner==FALSE), 
            P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), 
            P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), 
            P2p=100*P2M/P2A, 
            P3M=sum(three_point_field_goals_made, na.rm = TRUE), 
            P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), 
            P3p=100*P3M/P3A, 
            FTM=sum(free_throws_made, na.rm = TRUE), 
            FTA=sum(free_throws_attempted, na.rm = TRUE), 
            FTp=100*FTM/FTA, 
            OREB=sum(offensive_rebounds, na.rm = TRUE), 
            DREB=sum(defensive_rebounds, na.rm = TRUE), 
            AST=sum(assists, na.rm = TRUE), 
            TOV=sum(turnovers, na.rm = TRUE), 
            STL=sum(steals, na.rm = TRUE), 
            BLK=sum(blocks, na.rm = TRUE), 
            PF=sum(fouls, na.rm = TRUE), 
            PM=sum(team_score-opponent_team_score, na.rm=TRUE)) %>%
  rename(Season=season, Team=opponent_team_location) %>%
  as.data.frame()

##############################################################################################################
#Opponent FT% by team
opponent_percent <- Obox %>% 
	filter(Team != "Team Stewart" & Team != "Team Wilson")%>%
	arrange(desc(FTp))%>%
	mutate(FTp=FTp/100)%>%
	mutate(FTA_per_game=FTA/GP) %>%
	mutate(prob_miss_both=(1-FTp)*(1-FTp)) %>%	
	mutate(expected_trips_before_slice=1/prob_miss_both)%>%
	mutate(approx_slices_per_game=(FTA_per_game/2)/expected_trips_before_slice)%>%
	mutate(approx_miss_twices=GP*approx_slices_per_game)%>%
	select(Team, FTM, FTA, FTp, FTA_per_game, prob_miss_both, expected_trips_before_slice,approx_slices_per_game, approx_miss_twices)
opponent_percent

#Opponent FT% by team, home games only
opponent_percent_home <- Obox_home %>% 
	filter(Team != "Team Stewart" & Team != "Team Wilson")%>%
	arrange(desc(FTp))%>%
	mutate(FTp=FTp/100)%>%
	mutate(FTA_per_game=FTA/GP) %>%
	mutate(prob_miss_both=(1-FTp)*(1-FTp)) %>%	
	mutate(expected_trips_before_slice=1/prob_miss_both)%>%
	mutate(approx_slices_per_game=(FTA_per_game/2)/expected_trips_before_slice)%>%
	mutate(approx_miss_twices=GP*approx_slices_per_game)%>%
	select(Team, FTM, FTA, FTp, FTA_per_game, prob_miss_both, expected_trips_before_slice,approx_slices_per_game, approx_miss_twices)
opponent_percent_home


#round to two decimal places on each column
# Select numeric columns to be rounded
numeric_columns <- sapply(opponent_percent, is.numeric)

# Round only numeric columns of the opponent_percent dataframe to 2 decimal places
opponent_percent[numeric_columns] <- lapply(opponent_percent[numeric_columns], round, 2)

# Print the rounded dataframe
print(opponent_percent)

# Select numeric columns to be rounded
numeric_columns2 <- sapply(opponent_percent_home, is.numeric)

# Round only numeric columns of the opponent_percent dataframe to 2 decimal places
opponent_percent_home[numeric_columns2] <- lapply(opponent_percent_home[numeric_columns2], round, 2)

# Print the rounded dataframe
print(opponent_percent_home)


##############################################################################################################
#Calculate number of actual miss twice moments for each team

pbp<-subset(wnba_pbp, season_type==2) #regular season only

filtered_pbp <- pbp%>%
	filter(type_text == "Free Throw - 1 of 2" | type_text == "Free Throw - 2 of 2")%>%
	select(game_id, home_team_name, away_team_name, type_text, text, athlete_id_1)
filtered_pbp

#count all miss twice moments
# Summarize by summing new_column for each home_team_name where the text column contains the specified text
summary_df <- filtered_pbp %>%
  group_by(home_team_name) %>%
  summarise(sum_new_column = sum(str_detect(text, "misses free throw 1 of 2") & lead(str_detect(text, "misses free throw 2 of 2"), default = FALSE)))

# Print the summary dataframe
print(summary_df)


#####figure out which miss twice moments are by the away team

# Group by athlete_id and team_location, count occurrences, and arrange by athlete_id
athlete_teams <- wnba_player_box %>%
  group_by(athlete_id, team_location) %>%
  summarise(count = n()) %>%
  arrange(athlete_id, desc(count))

# Keep only the first row for each athlete_id (which corresponds to the team_location they have most often)
unique_athlete_teams <- athlete_teams %>%
  distinct(athlete_id, .keep_all = TRUE)

# Perform a left join to add the team_location column to filtered_pbp
filtered_pbp <- left_join(filtered_pbp, unique_athlete_teams, by = c("athlete_id_1" = "athlete_id"))

filtered_pbp2 <- filtered_pbp%>%
	filter(type_text == "Free Throw - 1 of 2" | type_text == "Free Throw - 2 of 2")%>%
	filter(away_team_name==team_location)%>%
	select(game_id, home_team_name, away_team_name, type_text, text, athlete_id_1)
filtered_pbp2

#count all miss twice moments by AWAY team
# Summarize by summing new_column for each home_team_name where the text column contains the specified text
summary_df2 <- filtered_pbp2 %>%
  group_by(home_team_name) %>%
  summarise(sum_new_column = sum(str_detect(text, "misses free throw 1 of 2") & lead(str_detect(text, "misses free throw 2 of 2"), default = FALSE))) %>%
  arrange(desc(sum_new_column))

# Print the summary dataframe
print(summary_df2)
#THIS IS THE ANSWER (this counts more than one per game, if any)


#print it nicely
joined_df <- left_join(summary_df2, opponent_percent_home, by = c("home_team_name" = "Team"))

joined_df <- joined_df%>%
	select(Team=home_team_name, Expected_slices=approx_miss_twices, Actual_slices=sum_new_column)%>%
	arrange(desc(Actual_slices))

print(joined_df)


##############################################################################################################
#opponent home FT% per NCAAW team (which team should do miss twice get a slice?)

Obox_home2 <- wbb_team_box %>%
  group_by(season, opponent_team_location) %>%
  filter(season_type==2)%>% #regular season only
  filter(team_home_away=="away")%>% #home games only (opponents are away)
  summarise(GP=n(), 
            PTS=sum(team_score, na.rm = TRUE),
            W=sum(team_winner==TRUE), 
            L=sum(team_winner==FALSE), 
            P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), 
            P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), 
            P2p=100*P2M/P2A, 
            P3M=sum(three_point_field_goals_made, na.rm = TRUE), 
            P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), 
            P3p=100*P3M/P3A, 
            FTM=sum(free_throws_made, na.rm = TRUE), 
            FTA=sum(free_throws_attempted, na.rm = TRUE), 
            FTp=100*FTM/FTA, 
            OREB=sum(offensive_rebounds, na.rm = TRUE), 
            DREB=sum(defensive_rebounds, na.rm = TRUE), 
            AST=sum(assists, na.rm = TRUE), 
            TOV=sum(turnovers, na.rm = TRUE), 
            STL=sum(steals, na.rm = TRUE), 
            BLK=sum(blocks, na.rm = TRUE), 
            PF=sum(fouls, na.rm = TRUE), 
            PM=sum(team_score-opponent_team_score, na.rm=TRUE)) %>%
  rename(Season=season, Team=opponent_team_location) %>%
  as.data.frame()

Obox_home2 <- Obox_home2%>%
	arrange(desc(FTp))%>%
	filter(GP>2)%>%
	select(Team, GP, FTM, FTA, FTp)
Obox_home2

##############################################################################################################
#this stuff didn't really work :/

# Count occurrences of "misses free throw 1 of 2" followed by "misses free throw 2 of 2" within three rows
num_occurrences <- pbp %>%
  mutate(
    missed_ft1 = str_detect(text, "misses free throw 1 of 2"),
    missed_ft2 = lead(str_detect(text, "misses free throw 2 of 2"), default = FALSE, n = 3)
  ) %>%
  filter(missed_ft1 & missed_ft2) %>%
  nrow()

# Add a new column indicating 1 next to "misses free throw 2 of 2" if preceded by "misses free throw 1 of 2" within three rows
pbp <- pbp %>%
  mutate(
    new_column = if_else(missed_ft1 & missed_ft2, 1, 0)
  )

# Calculate the sum of new_column values
sum_new_column <- sum(pbp$new_column)

# Print the results
print(num_occurrences)
print(sum_new_column)


hi <- pbp %>%
  group_by(home_team_name) %>%
  summarise(total_new_column = sum(new_column))
hi






# Function to check if the conditions are met for a group
check_conditions <- function(group_df) {
  num_games <- 0
  for (i in 1:(nrow(group_df) - 3)) {
    if (any(str_detect(group_df$text[i:(i+2)], "misses free throw 1 of 2")) &&
        any(str_detect(group_df$text[i:(i+2)], "misses free throw 2 of 2")) &&
        any(group_df$home_team_name[i] != lead(group_df$home_team_name[i:(i + 2)], default = ""))) {
      num_games <- num_games + 1
      break  # Break the loop if condition is met for a game
    }
  }
  return(num_games)
}

# Group by home_team_name, then apply the check_conditions function
num_games_per_team <- pbp %>%
  group_by(home_team_name) %>%
  summarise(num_games = sum(sapply(split(1:nrow(pbp), pbp$game_id), function(idx) check_conditions(pbp[idx,]))))

# Print the results
print(num_games_per_team)
