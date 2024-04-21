#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, ggplot2)

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
  
#team box scores  
Tbox <- wbb_team_box %>%
  group_by(season, team_location) %>%
  summarise(GP=n(), 
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

#opponent box scores  
Obox <- wbb_team_box %>%
  group_by(season, opponent_team_location) %>%
  summarise(GP=n(), 
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

#league box scores
Lbox <- wbb_team_box %>%
  group_by(season) %>%
  summarise(GP=n(), 
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
  rename(Season=season) %>%
  as.data.frame()
  
 ###########################################################################################################  
osu_opponents <-subset(Obox, Team=="Ohio State")

#osu home opponents
osu_home <- subset(wbb_team_box, opponent_team_location=="Ohio State" & team_home_away=="away" & team_location !="Oklahoma State")

home_opponents<- osu_home %>%
  group_by(season) %>%
  summarise(GP=n(), 
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
  rename(Season=season) %>%
  as.data.frame()
print(home_opponents)

#osu away opponents
osu_away <- subset(wbb_team_box, opponent_team_location=="Ohio State" & (team_home_away!="away" | team_location=="Oklahoma State"))
  
away_opponents<- osu_away %>%
  group_by(season) %>%
  summarise(GP=n(), 
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
  rename(Season=season) %>%
  as.data.frame()
print(away_opponents)

#ohio state's opponents actually make more free throws against them on osu's home floor than on their own

#what did ohio state's 32 (non-unique) opponents average in general?
osu_opp <- c(osu_home$team_location, osu_away$team_location)
osu_opp_box <- subset(wbb_team_box, team_location %in% osu_opp)

osu_opp_total <- osu_opp_box %>%
  group_by(season) %>%
  summarise(GP=n(), 
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
  rename(Season=season) %>%
  as.data.frame()
print(osu_opp_total)

###########################################################################################################  
#stacked bar chart
library(tidyr)
  
# Example data frame
df <- data.frame(
  category = c("A", "B", "C", "D"),
  variable1 = c(10, 15, 20, 25),
  variable2 = c(5, 10, 15, 20)
)

# Reshape the data frame from wide to long format using tidyr
df_long <- pivot_longer(df, cols = starts_with("variable"), names_to = "variable", values_to = "value")

# Create the stacked bar chart using ggplot2
ggplot(df_long, aes(x = category, y = value, fill = variable)) +
  geom_bar(stat = "identity")
  

#real version now
df <- data.frame(
  category = c("Home opponents (19 games)", "Away opponents (13 games)"),
  variable1 = c(home_opponents$FTA-home_opponents$FTM, away_opponents$FTA-away_opponents$FTM),
  variable2 = c(home_opponents$FTM, away_opponents$FTM)
)

df_long <- pivot_longer(df, cols = starts_with("variable"), names_to = "variable", values_to = "value")

ggplot(df_long, aes(x = category, y = value, fill = variable)) +
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("variable1" = "brown3", "variable2" = "cadetblue"),                    name = "FTA Result", labels = c("FT Missed", "FT Made"))+
  labs(title="Ohio State Opponent Free Throw Statistics", subtitle="2023-24 season, via wehoop R package", x="Ohio State Home vs Away", y="Opponent Free Throw Count")

###########################################################################################################  
#old stuff

# Create vectors for each column
column1 <- c("NCAA D1","ACC","Big 10", "Big 12", "Pac-12", "SEC")
column2 <- c(Lbox$FTp,acc_box$FTp,big_box$FTp,big12_box$FTp,pac_box$FTp,sec_box$FTp)
column3 <- c(Lbox$FTM,acc_box$FTM,big_box$FTM,big12_box$FTM,pac_box$FTM,sec_box$FTM)
column4 <- c(Lbox$FTA,acc_box$FTA,big_box$FTA,big12_box$FTA,pac_box$FTA,sec_box$FTA)

# Combine the vectors into a dataframe
my_dataframe <- data.frame(League = column1, FTM= column3, FTA = column4, FT_percent = column2)

# View the dataframe
print(my_dataframe)

my_dataframe <- my_dataframe[-1,]

my_dataframe

my_dataframe <- my_dataframe %>%
	arrange(FTM, decreasing=TRUE)
	
# Reorder levels of League based on FTM values
my_dataframe_long$League <- factor(my_dataframe_long$League, levels = my_dataframe_long$League[order(my_dataframe_long$Count, decreasing = TRUE)])


library(tidyr)

# Stacked Bar Chart
ggplot(my_dataframe_long, aes(x = League, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Free Throws Attempted and Made by League",
       x = "League", y = "Count", fill = "Type") +
    scale_fill_manual(values = c("skyblue", "blue"),
                     labels = c("Free Throws Missed", "Free Throws Made")) +
  theme_minimal()


#this works
# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Create the dataframe
my_dataframe <- data.frame(
  League = c("NCAA D1", "ACC", "Big 10", "Big 12", "Pac-12", "SEC"),
  FTM = c(128420, 5479, 5004, 5312, 4248, 5748),
  FTA = c(180752, 7481, 6739, 7435, 5808, 8110),
  FT_percent = c(71.04762, 73.23887, 74.25434, 71.44586, 73.14050, 70.87546)
)

my_dataframe <- my_dataframe %>%
	mutate(FTA=FTA-FTM)

# Remove "NCAA D1" row
my_dataframe <- my_dataframe[-1, ]

# Order the dataframe by FTM in descending order
my_dataframe <- my_dataframe %>%
  arrange(desc(FT_percent))

# Reshape the data to long format
my_dataframe_long <- pivot_longer(my_dataframe, cols = c(FTA, FTM),
                                  names_to = "Type", values_to = "Count")

# Reorder levels of League based on FTM values
my_dataframe_long$League <- factor(my_dataframe_long$League, levels = my_dataframe$League)

# Stacked Bar Chart with text labels for FT_percent
ggplot(my_dataframe_long, aes(x = League, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = sprintf("%.2f", FT_percent), y = Count), vjust = -0.5, size = 6, color = "black") +
  labs(title = "Free Throw Percentages in Power Five Conferences",
       x = "Conference", y = "Free Throws", fill = "Type") +
  scale_fill_manual(values = c("skyblue", "blue"),
                     labels = c("Free Throws Missed", "Free Throws Made")) +
  theme_minimal()


