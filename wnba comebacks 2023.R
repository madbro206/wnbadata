#how to get the WNBA data
#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr)


#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box()
})

tictoc::toc()

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


#redo for all seasons between 2002-2023
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season=c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023))
})

tictoc::toc()