#how to get the WNBA data

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr)


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


#wehoop has draftboard for season 2016-present
#I want to average players' WS in the WNBA in their first three years, so I just look at drafts 2016-2021 (all those players have had a chance to complete three seasons)
#accounts for 216 players

#add draft pick number to each player's row
d16 <-wnba_draftboard(season=2016)$picks %>%
	  mutate(pick = row_number()) %>%
	  mutate(year=2016)
	  
d17 <-wnba_draftboard(season=2017)$picks %>%
	  mutate(pick = row_number()) %>%
	  mutate(year=2017)
	  
d18 <-wnba_draftboard(season=2018)$picks %>%
	  mutate(pick = row_number()) %>%
	  mutate(year=2018)	  	  

d19 <-wnba_draftboard(season=2019)$picks %>%
	  mutate(pick = row_number()) %>%
	  mutate(year=2019)
	  
d20 <-wnba_draftboard(season=2020)$picks %>%
	  mutate(pick = row_number()) %>%
	  mutate(year=2020)	  

d21 <-wnba_draftboard(season=2021)$picks %>%
	  mutate(pick = row_number()) %>%
	  mutate(year=2021)
	  
drafts <- rbind(d16,d17,d18,d19,d20,d21)

picks <- drafts %>%
	select(year, pick, player_name)
	

stats <- read.csv("/Users/maddy/win_shares_2016to2023.csv",header=TRUE)
stats <- stats %>%
	select(Year, Player, WS)

#players second year
picks2 <- picks %>%
  mutate(year = year + 1)
  
#players third year
picks3 <- picks %>%
  mutate(year = year + 2)

#find win shares for players' first, second, and third years
result_df1 <- inner_join(stats, picks, by = c("Player" = "player_name", "Year" = "year"))
result_df2 <- inner_join(stats, picks2, by = c("Player" = "player_name", "Year" = "year"))
result_df3 <- inner_join(stats, picks3, by = c("Player" = "player_name", "Year" = "year"))

players <- rbind(result_df1,result_df2,result_df3)


#group by Player and sum their WS values
players_sum_WS <- players %>%
  group_by(Player) %>%
  summarize(Total_WS = sum(WS, na.rm = TRUE))

#round the Total_WS column to two decimal places
players_sum_WS$Total_WS <- round(players_sum_WS$Total_WS, 2)

#add the pick numbers back in
picks_and_ws <- left_join(players_sum_WS, picks, by = c("Player" = "player_name"))
picks_and_ws <- picks_and_ws %>%
	arrange(pick)
	
print(picks_and_ws, n=134)

#average WS by draft pick position
total <- picks_and_ws %>%
	group_by(pick) %>%
	summarize(average_ws =mean(Total_WS)) %>%
	select(pick, average_ws)

total$average_ws <- round(total$average_ws,2)
	
print(total, n=36)

#plot data
#create a scatter plot
ggplot(total, aes(x = pick, y = average_ws)) +
  geom_point() +
  labs(x = "Pick Number", y = "Average WS over first three years") +
  ggtitle("Scatter Plot of Pick vs. Average WS Over First Three Years")
