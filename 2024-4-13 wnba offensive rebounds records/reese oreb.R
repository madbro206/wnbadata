#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, ggplot2)

#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(season=c(2010:2023)) #wnba since 2010
})
tictoc::toc()

#wbb box score
tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box()
})
tictoc::toc()

wnba_player_box <- subset(wnba_player_box, season_type==2)

Pbox <- wnba_player_box %>%
  group_by(season, team_short_display_name, athlete_display_name) %>%
  summarise(GP=sum(minutes>0, na.rm=TRUE), MIN=sum(minutes, na.rm = TRUE), PTS=sum(points, na.rm = TRUE),OREB=sum(offensive_rebounds, na.rm = TRUE)) %>%
  as.data.frame()
  


##############################################################################################################
#top three oreb for each season since 2010

# Group by season and filter the top three players with the most oreb
top_oreb <- Pbox %>%
  group_by(season) %>%
  top_n(3, wt = OREB) %>%
  arrange(season, desc(OREB))
  
# Add rank variable based on order of oreb within each season
top_oreb <- top_oreb %>%
  group_by(season) %>%
  mutate(rank = dense_rank(desc(OREB)))

print(top_oreb)

# Reorder the levels of rank variable
top_oreb$rank <- factor(top_oreb $rank, levels = rev(levels(factor(top_oreb $rank))))

# Create scatter plot with lines connecting the dots for the same year
ggplot(top_oreb, aes(x = rank, y = OREB, color = as.factor(season), group = season)) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_discrete(breaks = c(3, 2, 1), labels = c("3rd Place", "2nd Place", "1st Place")) +
  labs(x = "Place in Season", y = "OREB", color = "Season") +
  theme_minimal()


# Create scatter plot with lines connecting the dots for the same year
#with first place labels
ggplot(top_oreb, aes(x = rank, y = OREB, color = as.factor(season), group = season)) +
  geom_point(size = 3) +
  geom_line() +
  geom_text(data = subset(top_oreb, rank == 1), aes(label = athlete_display_name), vjust = -.1, hjust=-0.1,show.legend=FALSE, position = position_jitter(width = 0.01, height = 2)) +
  scale_x_discrete(breaks = c(3, 2, 1), labels = c("3rd Place", "2nd Place", "1st Place")) +
  ylim(20,140)+
  scale_y_continuous(breaks = seq(0, max(top_oreb$OREB), by = 10)) +
  labs(x = "Place in Season", y = "Season OREB", color = "Season", title="WNBA Regular Season OREB Leaders since 2010", subtitle="using wehoop R package") 


  
#display block leader for each season since 2010
oreb<-top_oreb%>%
	filter(rank==1)%>%
	arrange(desc(season))%>%
	select(season, athlete_display_name, OREB)
oreb

