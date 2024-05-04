#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR)

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
  wnba_pbp <- wehoop::load_wnba_pbp(season=c(2004:2023))
})
tictoc::toc()

#wbb team full box score
tictoc::tic()
progressr::with_progress({
  wbb_team_box <- wehoop::load_wbb_team_box()
})
tictoc::toc()

##############################################################################################################
#identify halftime scores, regular season games only
half <- subset(wnba_pbp, period_display_value=="3rd Quarter" & clock_display_value=="10:00" & season_type==2)
half <- half %>% distinct(game_id, half, .keep_all = TRUE)

half2 <- select(half, game_id, game_date, home_score, away_score, home_team_name, away_team_name)

#get game winner data from wnba_team_box
# Assuming half2 is your first dataframe and wnba_team_box is your second dataframe
# Assuming you want to join based on the condition half2$home_team_name = wnba_team_box$team_name

merged_df <- left_join(half2, wnba_team_box %>% select(game_id, team_location, team_winner), 
                       by = c("game_id" = "game_id", "home_team_name"="team_location"))

colnames(merged_df)[colnames(merged_df) == "team_winner"] <- "home_team_winner"

merged_df$home_lead_at_half <- merged_df$home_score -merged_df$away_score

merged_df

##############################################################################################################
library(dplyr)
library(ggplot2)
library(scales)

# Assuming merged_df is your dataframe
probability_df <- merged_df %>%
  group_by(home_lead_at_half) %>%
  summarize(wins = sum(home_team_winner == TRUE), 
            total_games = n(),
            empirical_probability = wins / total_games * 100)

print(probability_df, n=66)

# Filter the dataframe to include only data within the range -10 to 10
filtered_df <- probability_df %>%
  filter(home_lead_at_half >= -10 & home_lead_at_half <= 10)

# Plotting the empirical probability with a smooth curve
ggplot(filtered_df, aes(x = home_lead_at_half, y = empirical_probability)) +
  geom_point() +
  geom_hline(yintercept=50, linetype='dotted', col = 'red')+
  geom_vline(xintercept=0, linetype='dotted', col = 'red')+
  geom_text(aes(label = round(empirical_probability,1)), vjust = -0.5, size=2.7) +
  geom_smooth(method = "loess", se = FALSE, linewidth=.4) +  # Add a smooth curve using loess method
  labs(x = "Lead at Half (Home Team)", y = "Win Percentage (%)", 
       title = "Historical Win Probability by Lead at Half", subtitle="WNBA Regular Season Games Since 2004") +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +  # Add more tick marks on the x-axis
  scale_y_continuous(breaks = seq(0, 100, by = 5), limits=c(0,100), labels=scales::percent_format(scale = 1))   # Add more tick marks on the y-axis  
##############################################################################################################
#games down by three at the half
down_three <- subset(merged_df, home_lead_at_half==1)
down_three <- down_three%>%
	arrange(game_date)
print(down_three, n=103)