#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR)

#load data

#wnba full play by play
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp()
})
tictoc::toc()


#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season=2023)
})
tictoc::toc()


#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box()
})
tictoc::toc()


#equivalent in NCAAW:
tictoc::tic()
progressr::with_progress({
  wbb_pbp <- wehoop::load_wbb_pbp()
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_team_box <- wehoop::load_wbb_team_box()
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box()
})
tictoc::toc()

#######################################################################################################
#translate into player, team, and opponent box score

Pbox <- wbb_player_box %>%
  group_by(season, team_location, athlete_display_name) %>%
  summarise(GP=sum(minutes>0, na.rm=TRUE), MIN=sum(minutes, na.rm = TRUE), PTS=sum(points, na.rm = TRUE),
    P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), P2p=100*P2M/P2A,
    P3M=sum(three_point_field_goals_made, na.rm = TRUE), P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), P3p=100*P3M/P3A,
    FTM=sum(free_throws_made, na.rm = TRUE), FTA=sum(free_throws_attempted, na.rm = TRUE), FTp=100*FTM/FTA,
    OREB=sum(offensive_rebounds, na.rm = TRUE), DREB=sum(defensive_rebounds, na.rm = TRUE), AST=sum(assists, na.rm = TRUE),
    TOV=sum(turnovers, na.rm = TRUE), STL=sum(steals, na.rm = TRUE), BLK=sum(blocks, na.rm = TRUE),
    PF=sum(fouls, na.rm = TRUE)) %>%
  rename(Season=season, Team=team_location,
         Player=athlete_display_name) %>%
         filter(GP>5) %>%
  as.data.frame()
  
#calculate game length to calculate the amount of minutes each team played
wbb_pbp <- wbb_pbp %>%
  group_by(game_id) %>%
  mutate(
    game_qtrs = max(qtr),
    game_length = case_when(
      game_qtrs == 4 ~ 40,
      game_qtrs == 5 ~ 45,
      game_qtrs == 6 ~ 50,
      game_qtrs == 7 ~ 55,
      game_qtrs == 8 ~ 60,
      TRUE ~ NA_integer_
    )
  )

#add these minute calculations to the wbb_team_box dataframe
wbb_team_box <- wbb_team_box %>%
  left_join(wbb_pbp %>% select(game_id, game_length), by = "game_id") %>%
  distinct(game_id, team_location, .keep_all = TRUE)

#make sure each game has a length 
wbb_team_box <- wbb_team_box %>%
  mutate(
    game_length = case_when(
      is.na(game_length) ~ 40,  # Set to 40 if game_length is NA
      TRUE ~ game_length  # Keep the original value if not NA
    )
  )

  
Tbox <- wbb_team_box %>%
  group_by(season, team_location) %>%
  summarise(GP=n(), 
  			MIN=sum(game_length, na.rm = TRUE), 
            PTS=sum(team_score, na.rm = TRUE),
            W=sum(team_winner==TRUE), 
            L=sum(team_winner==FALSE), 
            P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), 
            P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), 
            P2p=100*P2M/P2A, 
            P3M=sum(three_point_field_goals_made, na.rm = TRUE), 
            P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), 
            P3p=100*P3M/P3A, 
            FTM=sum(free_throws_made, na.rm = TRUE), 
            FTA=sum(free_throws_attempted, na.rm = TRUE), 
            FTp=100*FTM/FTA, 
            OREB=sum(offensive_rebounds, na.rm = TRUE), 
            DREB=sum(defensive_rebounds, na.rm = TRUE), 
            AST=sum(assists, na.rm = TRUE), 
            TOV=sum(turnovers, na.rm = TRUE), 
            STL=sum(steals, na.rm = TRUE), 
            BLK=sum(blocks, na.rm = TRUE), 
            PF=sum(fouls, na.rm = TRUE), 
            PM=sum(team_score-opponent_team_score, na.rm=TRUE)) %>%
  rename(Season=season, Team=team_location) %>%
  as.data.frame()
  

Obox <- wbb_team_box %>%
  group_by(season, opponent_team_location) %>%
  summarise(GP=n(), 
  			MIN=sum(game_length, na.rm = TRUE),
            PTS=sum(team_score, na.rm = TRUE),
            W=sum(team_winner==TRUE), 
            L=sum(team_winner==FALSE), 
            P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), 
            P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), 
            P2p=100*P2M/P2A, 
            P3M=sum(three_point_field_goals_made, na.rm = TRUE), 
            P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), 
            P3p=100*P3M/P3A, 
            FTM=sum(free_throws_made, na.rm = TRUE), 
            FTA=sum(free_throws_attempted, na.rm = TRUE), 
            FTp=100*FTM/FTA, 
            OREB=sum(offensive_rebounds, na.rm = TRUE), 
            DREB=sum(defensive_rebounds, na.rm = TRUE), 
            AST=sum(assists, na.rm = TRUE), 
            TOV=sum(turnovers, na.rm = TRUE), 
            STL=sum(steals, na.rm = TRUE), 
            BLK=sum(blocks, na.rm = TRUE), 
            PF=sum(fouls, na.rm = TRUE), 
            PM=sum(team_score-opponent_team_score, na.rm=TRUE)) %>%
  rename(Season=season, Team=opponent_team_location) %>%
  as.data.frame()

##############################################################################################################
library(dplyr)

# Filter the dataframe for games where the Las Vegas Aces won
aces_wins <- wnba_team_box %>%
  filter(team_name == "Aces" & team_winner == TRUE & season_type==2)

aces_losses <- aces_wins %>%
  select(game_id, game_date, team_name = opponent_team_name, 
         team_score = opponent_team_score, aces_score = team_score)
         
aces_losses$loss_margin <- aces_losses$aces_score-aces_losses$team_score

print(aces_losses)


# Group by team_name and find the largest loss margin for each team
largest_losses <- aces_losses %>%
  group_by(team_name) %>%
  summarise(largest_loss_margin = max(loss_margin))

# Join back with aces_losses to get the corresponding rows
largest_losses_info <- aces_losses %>%
  inner_join(largest_losses, by = c("team_name", "loss_margin" = "largest_loss_margin"))

# Print the resulting dataframe
print(largest_losses_info)


# Group by team_name and find the largest and average loss margin for each team
loss_summary <- aces_losses %>%
  group_by(team_name) %>%
  summarise(largest_loss_margin = max(loss_margin),
            average_loss_margin = mean(loss_margin))

# Join back with aces_losses to get the corresponding rows
loss_summary_info <- aces_losses %>%
  inner_join(loss_summary, by = "team_name")

# Print the resulting dataframe
print(loss_summary_info)
##############################################################################################################
library(ggplot2)

# Convert team_name to character
loss_summary_info$team_name <- as.character(loss_summary_info$team_name)

# Arrange loss_summary_info by average loss margin in descending order
loss_summary_info <- loss_summary_info[order(-loss_summary_info$average_loss_margin), ]

# Create a side-by-side bar plot for loss_summary_info
plot <- ggplot(loss_summary_info, aes(x = reorder(team_name, -average_loss_margin))) +
  geom_bar(aes(y = largest_loss_margin, fill = "Largest Loss Margin"), 
           position = position_dodge(width = 0.9), stat = "identity", width = 0.4) +
  #geom_bar(aes(x = as.numeric(reorder(team_name, -average_loss_margin)) + 0.4, y = average_loss_margin, fill = "Average Loss Margin"), position = position_dodge(width = 0.9), stat = "identity", width = 0.4) +
  labs(title="Aces vs Everybody", subtitle = "Largest and Average Loss Margins vs Aces for Each Team in WNBA 2023", x="Team",
       y = "Loss Margin", fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") +
  scale_fill_manual(values = c("Largest Loss Margin" = "purple", "Average Loss Margin" = "69B5A3")) 
  
  # Create a side-by-side bar plot for loss_summary_info
both <- ggplot(loss_summary_info, aes(x = reorder(team_name, -average_loss_margin))) +
  geom_bar(aes(y = largest_loss_margin, fill = "Largest Loss Margin"), 
           position = position_dodge(width = 0.9), stat = "identity", width = 0.4) +
  geom_bar(aes(x = as.numeric(reorder(team_name, -average_loss_margin)) + 0.4, y = average_loss_margin, fill = "Average Loss Margin"), position = position_dodge(width = 0.9), stat = "identity", width = 0.4) +
  labs(title="Aces vs Everybody", subtitle = "Largest and Average Loss Margins vs Aces for Each Team in WNBA 2023", x="Team",
       y = "Loss Margin", fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") +
  scale_fill_manual(values = c("Largest Loss Margin" = "purple", "Average Loss Margin" = "69B5A3")) 


print(plot)

print(both)
