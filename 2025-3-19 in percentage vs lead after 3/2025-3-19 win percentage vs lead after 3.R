#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, ggplot2, scales)

#load data
#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season=c(2004:2023))
})
tictoc::toc()

#wnba play by play
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp(season=c(2006:2023))
})
tictoc::toc()

#unrivaled data
unrivaled <- read.csv("~/Desktop/unrivaled.csv", header = TRUE)

############################################    setup    ##################################################################
#identify scores after three quarters, regular season games only
half <- subset(wnba_pbp, period_display_value=="4th Quarter" & clock_display_value=="10:00" & season_type==2 & home_score > 20)
half <- half %>% distinct(game_id, half, .keep_all = TRUE)

half2 <- select(half, game_id, game_date, home_score, away_score, home_team_name, away_team_name)

#get game winner data from wnba_team_box
merged_df <- left_join(half2, wnba_team_box %>% select(game_id, team_location, team_winner), by = c("game_id" = "game_id", "home_team_name"="team_location"))

colnames(merged_df)[colnames(merged_df) == "team_winner"] <- "home_team_winner"

merged_df$home_lead_at_three <- merged_df$home_score -merged_df$away_score
###########################################   wnba & charts   ###################################################################
probability_df <- merged_df %>%
  group_by(home_lead_at_three) %>%
  summarize(wins = sum(home_team_winner == TRUE), total_games = n(), empirical_probability = wins / total_games * 100)

print(probability_df, n=80)

# only data within the range -10 to 10
filtered_df <- probability_df %>%
  filter(home_lead_at_three >= -10 & home_lead_at_three <= 10)

#plotting empirical probability with a smooth curve
ggplot(filtered_df, aes(x = home_lead_at_three, y = empirical_probability)) +
  geom_point() +
  geom_hline(yintercept=50, linetype='dotted', col = 'red')+
  geom_vline(xintercept=0, linetype='dotted', col = 'red')+
  geom_text(aes(label = round(empirical_probability,1)), vjust = -0.5, size=2.7) +
  geom_smooth(method = "loess", se = FALSE, linewidth=.4) +  
  labs(x = "Lead after 3 Quarters (Home Team)", y = "Win Percentage (%)", 
       title = "Historical Win Probability by Lead After 3 Quarters", subtitle="WNBA Regular Season Games Since 2006") +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +  
  scale_y_continuous(breaks = seq(0, 100, by = 5), limits=c(0,100), labels=scales::percent_format(scale = 1)) +
  theme_minimal()


#do without differentiating home and away

merged_df <- merged_df %>%
  mutate(
    lead = abs(home_lead_at_three),
    winner = ifelse(home_lead_at_three <= 0, !home_team_winner, home_team_winner)
  )

prob <- merged_df %>%
  group_by(lead) %>%
  summarize(wins = sum(home_team_winner == TRUE), total_games = n(), empirical_probability = wins / total_games * 100) %>%
  filter(lead <=10 & lead>0)

ggplot(prob, aes(x = lead, y = empirical_probability)) +
  geom_point() +
  geom_hline(yintercept=50, linetype='dotted', col = 'red')+
  geom_vline(xintercept=0, linetype='dotted', col = 'red')+
  geom_text(aes(label = round(empirical_probability,1)), vjust = -0.5, size=2.7) +
  geom_smooth(method = "loess", se = FALSE, linewidth=.4) +  
  labs(x = "Lead after 3 Quarters", y = "Win Percentage (%)", 
title = "Historical Win Rate by Lead After 3 Quarters", subtitle="WNBA Regular Season Games Since 2006") +
  scale_x_continuous(breaks = seq(0,10, by = 1)) +  
  scale_y_continuous(breaks = seq(50, 65, by = 5), limits=c(50,65), labels=scales::percent_format(scale = 1)) +
  theme_minimal()


simplified_df2 <- merged_df %>%
  filter(home_lead_at_three > 0 & home_team_winner == FALSE | home_lead_at_three < 0 & home_team_winner == TRUE) %>% # Filter games where a lead was blown
  mutate(
    team_that_blew_lead = ifelse(home_lead_at_three > 0, home_team_name, away_team_name),
    opponent = ifelse(home_lead_at_three > 0, away_team_name, home_team_name),
    lead_blown = abs(home_lead_at_three)
  ) %>%
  select(game_id, game_date, team_that_blew_lead, opponent, lead_blown)

print(subset(simplified_df2, lead_blown>=10), n=30)

print(simplified_df2)
###################################   unrivaled   ###########################################################################
unrivaled <- unrivaled %>%
  mutate(
    lead = abs(home_lead),
    winner = ifelse(home_lead < 0, 1 - home_winner, home_winner)
  )

#empirical probabilities
unrivaled_prob <- unrivaled %>%
  group_by(lead) %>%
  summarise(empirical_probability = mean(winner), total_games=n())

print(unrivaled_prob, n=23)



ggplot(unrivaled, aes(x = lead, y = 0, color = factor(winner))) +
  geom_point(size = 3, alpha = 0.5) +
  scale_color_manual(values = c("red", "green"), labels = c("Lost", "Won")) +
  labs(
    title = "Score Differential After Three Quarters",
    x = "Score Differential",
    y = NULL,
    color = "Outcome"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  )


  # Filter for games where the team that was down after three quarters won
  down_and_won <- unrivaled %>%
    filter((home_lead < 0 & home_winner == 1) |(home_lead > 0 & home_winner == 0))
  
  # Count the number of such games
  num_down_and_won <- nrow(down_and_won)
  
  # Calculate the total points they were down by in those instances
  total_points_down <- sum(abs(down_and_won$home_lead))
  
print(down_and_won)

  # Print results
  cat("Number of games where the team down after three quarters won:", num_down_and_won, "\n")
  cat("Total points they were down by in those instances:", total_points_down, "\n")
  

simplified_df <- down_and_won %>%
    mutate(
      team_that_lost_lead = ifelse(home_lead > 0, home, visitor),
      team_that_won = ifelse(home_lead > 0, visitor, home),
      lead_overcome = abs(home_lead)
    ) %>%
    select(game, team_that_lost_lead, team_that_won, lead_overcome)
  
  print(simplified_df)
  