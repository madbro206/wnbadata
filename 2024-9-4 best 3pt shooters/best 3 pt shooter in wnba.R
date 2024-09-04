#Use wehoop package to download WNBA

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, ggplot2)

#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(season=2024)
})
tictoc::toc()


#######################################################################################################
#translate into player, team, and opponent box score
wnba_player_box <- wnba_player_box %>%
	filter(team_name !="Team USA" & team_name !="Team WNBA") %>%
	filter(season_type==2)

Pbox <- wnba_player_box %>%
  group_by(athlete_display_name) %>%
  summarise(GP=sum(minutes>0, na.rm=TRUE), MIN=sum(minutes, na.rm = TRUE), PTS=sum(points, na.rm = TRUE),
    P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), P2p=P2M/P2A,
    P3M=sum(three_point_field_goals_made, na.rm = TRUE), P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), P3p=P3M/P3A,
    FTM=sum(free_throws_made, na.rm = TRUE), FTA=sum(free_throws_attempted, na.rm = TRUE), FTp=FTM/FTA,
    OREB=sum(offensive_rebounds, na.rm = TRUE), DREB=sum(defensive_rebounds, na.rm = TRUE), AST=sum(assists, na.rm = TRUE),
    TOV=sum(turnovers, na.rm = TRUE), STL=sum(steals, na.rm = TRUE), BLK=sum(blocks, na.rm = TRUE),
    PF=sum(fouls, na.rm = TRUE)) %>%
  rename(Player=athlete_display_name) %>%
  as.data.frame()

##############################################################################################################
#look at original distribution
Pbox_filtered <- Pbox %>%
    filter(P3M >= 2, P3p>0)
    
ggplot(Pbox_filtered, aes(x = P3p)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = .005) +
  labs(
    title = "Histogram of WNBA player 3p%",
    subtitle = "2024 season through 9/3, min 2 3PM",
    caption = "source: stats.wnba.com | graphic: @wnbadata",
    x = "3p%",
    y = "Number of players"
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::number_format(accuracy = 1))

#calculate beta params
Pbox_filtered <- na.omit(Pbox_filtered)

m <- MASS::fitdistr(Pbox_filtered$P3p, dbeta, start = list(shape1 = 10, shape2 = 5))

alpha0 <- m$estimate[1]
beta0 <- m$estimate[2]

#expected avg 3p%
alpha0/(alpha0+beta0)

#plot beta dist on original data
ggplot(Pbox_filtered, aes(x = P3p)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = .005) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), color = "red", linewidth = 1)+
	labs(
    title = "WNBA player 3p% with estimated Beta distribution",
    subtitle = "2024 season through 9/3, min 2 3PM",
    caption = "source: stats.wnba.com | graphic: @wnbadata",
    x = "3p%",
    y = "Number of players"
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::number_format(accuracy = 1))

#calculate empirical bayes estimates
Pbox_eb <- Pbox_filtered %>%
    mutate(eb_estimate = (P3M + alpha0) / (P3A + alpha0 + beta0)) %>%
    arrange(desc(eb_estimate)) 

best_shooters <- Pbox_eb %>% dplyr::select(Player, P3M, P3A, P3p, eb_estimate)%>%
	mutate( P3p = round(P3p, 3), eb_estimate = round(eb_estimate, 3) )


#display
best_shooters

best_shooters[c(34,39), ]
