#goal: find the exact moment the picture on ezi magbegor's 2022 prizm card was taken

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
  wnba_pbp <- wehoop::load_wnba_pbp(season=c(2020,2021,2022))
})
tictoc::toc()


#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season=c(2020,2021,2022))	
})

tictoc::toc()


#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(season=c(2020,2021,2022))
})
tictoc::toc()

#ezi's games
ezi <- wnba_player_box %>%
	filter(athlete_display_name=="Ezi Magbegor")
	
#karlie's games, sparks only
karlie <- wnba_player_box %>%
	filter(athlete_display_name=="Karlie Samuelson", team_name=="Sparks")
	
#games where both played
games <- inner_join(ezi, karlie, by ="game_id")

#first game
game1 <- subset(wnba_pbp, game_id==games$game_id[1])

#game1's home team is seattle, it looks like LA in the pic so I'll check game2 first
#game1$home_team_name

#second game
game2 <- subset(wnba_pbp, game_id==games$game_id[2])

#find rows where ezi shot the ball, blocked shots are not included but it doesn't look like a block
ezi2 <- game2[grepl("Ezi Magbegor m", game2$text), ]

#just need a few rows
ezi2<-ezi2[,c("period_number", "clock_display_value", "type_text", "text")]

print(ezi2, n = Inf, width = Inf)