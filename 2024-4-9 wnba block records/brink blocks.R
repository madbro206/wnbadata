#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR)

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
  summarise(GP=sum(minutes>0, na.rm=TRUE), MIN=sum(minutes, na.rm = TRUE), PTS=sum(points, na.rm = TRUE),BLK=sum(blocks, na.rm = TRUE)) %>%
  as.data.frame()
  


##############################################################################################################
#top three blocks for each season since 2010

# Group by season and filter the top three players with the most blocks
top_blocks <- Pbox %>%
  group_by(season) %>%
  top_n(3, wt = BLK) %>%
  arrange(season, desc(BLK))
  
# Add rank variable based on order of blocks within each season
top_blocks <- top_blocks %>%
  group_by(season) %>%
  mutate(rank = dense_rank(desc(BLK)))

print(top_blocks)

# Reorder the levels of rank variable
top_blocks$rank <- factor(top_blocks$rank, levels = rev(levels(factor(top_blocks$rank))))

# Create scatter plot with lines connecting the dots for the same year
ggplot(top_blocks, aes(x = rank, y = BLK, color = as.factor(season), group = season)) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_discrete(breaks = c(3, 2, 1), labels = c("3rd Place", "2nd Place", "1st Place")) +
  labs(x = "Place in Season", y = "Blocks", color = "Season") +
  theme_minimal()


# Create scatter plot with lines connecting the dots for the same year
#with first place labels
ggplot(top_blocks, aes(x = rank, y = BLK, color = as.factor(season), group = season)) +
  geom_point(size = 3) +
  geom_line() +
  geom_text(data = subset(top_blocks, rank == 1), aes(label = athlete_display_name), vjust = -.1, hjust=-0.1,show.legend=FALSE, position = position_jitter(width = 0.01, height = 2)) +
  scale_x_discrete(breaks = c(3, 2, 1), labels = c("3rd Place", "2nd Place", "1st Place")) +
  scale_y_continuous(breaks = seq(0, max(top_blocks$BLK), by = 10)) +
  labs(x = "Place in Season", y = "Blocks", color = "Season", title="WNBA Regular Season Block Leaders since 2010", subtitle="using wehoop R package") +
  theme_minimal()



# Create scatter plot
ggplot(top_blocks, aes(x = rank, y = BLK, color = as.factor(season))) +
  geom_point(size = 3) +
  geom_line()+
  scale_x_discrete(breaks = c(3, 2, 1),labels = c("3rd Place", "2nd Place", "1st Place")) +
  labs(x = "Place in Season", y = "Blocks", color = "Season") +
  theme_minimal()
  
  
  
  
#display block leader for each season since 2010
blk<-top_blocks%>%
	filter(rank==1)%>%
	arrange(desc(season))%>%
	select(season, athlete_display_name, BLK)
blk

