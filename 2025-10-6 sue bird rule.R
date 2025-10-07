#Use wehoop package to download WNBA data
pacman::p_load(wehoop, dplyr, tictoc, progressr, ggplot2)

tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(season=c(2021:2025))
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box(season=c(2021:2025))
})
tictoc::toc()
########################################################################################


wfouls_count <- wnba_player_box %>%
  filter(did_not_play==FALSE & minutes >=1) %>%
  group_by(fouls) %>%
  summarize(count = n())

wfouls_count$fouls <- factor(wfouls_count$fouls, levels = 0:6)

ggplot(wfouls_count, aes(x = fouls, y = count)) + 
  geom_bar(stat = "identity", fill="orange", color="blue") +
  geom_text(aes(label = count), vjust = -0.3) +
  theme_minimal() +
  labs(
    title = "Distribution of WNBA Player Game Fouls",
    subtitle = "2021–Present",
    x = "Fouls",
    y = "Count"
  )


total_games <- sum(wfouls_count$count)
wnba_foul_out_rate <- wfouls_count$count[wfouls_count$fouls == 6] / total_games * 100
hypothetical_foul_out_rate <- 
    (wfouls_count$count[wfouls_count$fouls == 5] + wfouls_count$count[wfouls_count$fouls == 6]) / total_games * 100
  

ggplot(wnba_player_box, aes(x = minutes, y = fouls)) +
  geom_point(alpha = 0.05, color = "red") +
  theme_minimal() +
  labs(
    title = "WNBA Player Minutes vs. Fouls",
    subtitle = "2021–Present",
    x = "Minutes Played",
    y = "Fouls Committed"
  )

cor(wnba_player_box$minutes, wnba_player_box$fouls, use = "complete.obs")



########################################################################################

ncaa_fouls_count <- wbb_player_box %>%
  filter(did_not_play==FALSE & minutes >=1) %>%
  filter(fouls <6)%>%
  group_by(fouls) %>%
  summarize(count = n())


total_games2 <- sum(ncaa_fouls_count$count)
ncaa_fouls_count$count[ncaa_fouls_count$fouls ==5] / total_games2 * 100

ncaa_fouls_count$fouls <- factor(ncaa_fouls_count$fouls, levels = 0:7)

ggplot(ncaa_fouls_count, aes(x = fouls, y = count)) + 
  geom_bar(stat = "identity", fill="red", color="blue") +
  geom_text(aes(label = count), vjust = -0.3) +
  theme_minimal() +
  labs(
    title = "Distribution of NCAAW Player Game Fouls",
    subtitle = "2021–Present",
    x = "Fouls",
    y = "Count"
  ) +
  scale_y_continuous(labels = scales::comma)