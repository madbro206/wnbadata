#how to get the WNBA data
#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, tidyverse, gt)


#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season=c(2005:2024)) #last 20 seasons
})
tictoc::toc()

#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp(season=c(2005:2024))
})
tictoc::toc()

##############################################################################################################
#find largest comebacks
#change largest_lead from char to int
wnba_team_box$largest_lead <- as.integer(wnba_team_box$largest_lead)

#filter to losing teams only
new_dataframe <- wnba_team_box[wnba_team_box$team_winner == FALSE, ]

#sort by largest lead (which was lost, equivalent to the other team coming back the same amount)
new_dataframe <- new_dataframe[order(-new_dataframe$largest_lead), ]

#columns game_date, team_name, opponent_team_name, largest_lead, opponent_team_score, team_score
selected_columns <- new_dataframe[, c("game_date", "opponent_team_name", "team_name","largest_lead", "opponent_team_score", "team_score")]

#rename columns
colnames(selected_columns) <- c("Date", "Winner", "Loser", "Lead Lost by Losing Team", "Winning Score", "Losing Score")

#print
selected_columns

#print 20 rows
print(selected_columns,n=20)
##############################################################################################################
#find comeback percentage by score deficit at 5:20 in the fourth quarter

#the columns I need
cols <- wnba_pbp[, c("game_date", "game_id", "period_number", "time", "clock_minutes", "clock_seconds", "home_team_name", "away_team_name", "home_score", "away_score")]

wnba_team_box <- subset(wnba_team_box, team_home_away=="away")

cols <- cols %>%
  left_join(wnba_team_box %>% 
              select(game_id, team_home_away, team_winner),
            by = "game_id")
##############################################################################################################
#score defecit for away team
score_differential_at_5_20 <- cols %>%
  filter(period_number == 4, (clock_minutes == 5 & clock_seconds >= 20)) %>%
  group_by(game_id) %>%
  slice_max(order_by = desc(time), n = 1) %>%
  mutate(score_differential = home_score - away_score) %>%
  select(game_id, game_date, time, away_team_name, away_score , home_team_name, home_score, score_differential, team_winner) #indicates away team defecit and if they won
  
#keep just one row for each game  
score_differential_at_5_20 <- score_differential_at_5_20 %>%
  distinct(game_id, .keep_all = TRUE)
  
#score_differential_at_5_20

#flip negative score differential to positive, and also flip entry of team_winner in those cases
library(dplyr)

score_differential_at_5_20 <- score_differential_at_5_20 %>%
  mutate(
    team_winner = if_else(score_differential < 0, !team_winner, team_winner),
    score_differential = abs(score_differential)
  )

score_differential_at_5_20

subset(score_differential_at_5_20, home_team_name=="New York" & away_team_name=="Minnesota")

##############################################################################################################
#graph

chart_data <- score_differential_at_5_20 %>%
  group_by(score_differential) %>%
  summarize(
    total_count = n(),
    winner_count = sum(team_winner, na.rm = TRUE),
    winner_frequency = mean(team_winner, na.rm = TRUE)
  ) %>%
  ungroup()

#since the original count was from the perspective of the away teams, the count for score_differential=0 is incorrect. I will force this to =0.5, since score diff of 0 will result in one winner and one loser every time
chart_data <- chart_data %>%
  mutate(winner_frequency = if_else(score_differential == 0, 0.5, winner_frequency))

print(chart_data, n=46)

ggplot(chart_data, aes(x = factor(score_differential), y = winner_frequency)) +
  geom_bar(stat = "identity", fill = "#236192") +
  theme_minimal() +
  labs(title = "Win Percentage by Score Deficit w/ 5:20 left",
  		subtitle="2005-2024 WNBA Seasons",
       x = "Score Deficit at Q4 5:20",
       y = "Win Percentage",
       caption="source: wehoop | graphic: @wnbadata") +
  theme(axis.text.x = element_text(angle = 0, hjust = .5)) +
  scale_x_discrete(limits = as.character(0:15))

subset(score_differential_at_5_20, score_differential >=10 & team_winner==TRUE)

subset(score_differential_at_5_20, score_differential >=50)


chart_data <- chart_data %>%
  mutate(
    winner_frequency = round(winner_frequency, 3)
  ) %>%
  rename(
    "Score Deficit" = score_differential,
    "Total Count" = total_count,
    "Winner Count" = winner_count,
    "Winner Frequency" = winner_frequency
  )

gt_table <- chart_data %>%
  gt() %>%
  tab_header(
    title = "Win Percentage by Score Deficit w/ 5:20 left",
    subtitle = "2005-2024 WNBA Seasons"
  )

print(gt_table)



##############################################################################################################
#what i would do if there was an exact 5:20 entry for every game
filtered_cols <- cols %>%
  filter(period_number == 4, clock_minutes == 5, clock_seconds == 20)

score_differentials <- filtered_cols %>%
  mutate(score_differential = home_score - away_score) %>%
  select(game_id, home_team_name, away_team_name, home_score, away_score, score_differential)

print(score_differentials)