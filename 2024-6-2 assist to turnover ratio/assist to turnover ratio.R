#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, ggplot2)

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
  wnba_team_box <- wehoop::load_wnba_team_box()
})

tictoc::toc()


#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box()
})
tictoc::toc()

#######################################################################################################
#translate into player box score

Pbox <- wnba_player_box %>%
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
  as.data.frame()

##############################################################################################################
#calculate assist to turnover ratio, just basic version
Pbox$A_T <- Pbox$AST/Pbox$TOV

#calculate minutes per game
Pbox$MP <- Pbox$MIN/Pbox$GP

#sort Pbox by ratio, only players with at least 10 minutes per game
Pbox <- Pbox %>%
	filter(MP>=10)%>%
	arrange(desc(A_T))

#plot TOV vs AST
library(ggrepel)

ggplot(Pbox, aes(x = TOV, y = AST)) +
  geom_point() +
  geom_text_repel(aes(label = Player), size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Add x=y line
  annotate("text", x = max(Pbox$TOV), y = max(Pbox$TOV), label = "AST=TOV", vjust = -0.5, hjust = 1, color = "red") +
  ggtitle("Scatter Plot of WNBA Player Turnovers vs Assists") +
  xlab("Turnovers") +
  ylab("Assists") +
  theme_minimal() +
  coord_equal()  # Ensures 1:1 scale for x and y axes
##############################################################################################################
# count bad passes for each player
# Load the stringr package for string manipulation functions
library(stringr)

# Extract "[player name] bad pass" or "[player name] out of bounds bad pass" from text column
bad_pass_events <- str_extract(wnba_pbp$text, "[^\\(]+(?: bad pass| out of bounds bad pass)")

# Clean up the extracted events to ensure consistency
bad_pass_events <- gsub(" out of bounds", "", bad_pass_events)

# Extract player names from the events
players <- str_extract(bad_pass_events, "[A-Za-z' ]+")

# Count occurrences for each player
bad_pass_counts <- table(players)

# Convert to data frame
bad_pass_data <- data.frame(Player = names(bad_pass_counts), Bad_Pass_Count = as.numeric(bad_pass_counts))

# Extract player names
bad_pass_data$Player <- sub(" bad pass.*", "", bad_pass_data$Player)

# Fix some of the entries
bad_pass_data$Player[bad_pass_data$Player == "Skylar Diggins"] <- "Skylar Diggins-Smith"
bad_pass_data$Player[bad_pass_data$Player == "Myisha Hines"] <- "Myisha Hines-Allen"
bad_pass_data$Player[bad_pass_data$Player == "Olivia Nelson"] <- "Olivia Nelson-Ododa"
bad_pass_data$Player[bad_pass_data$Player == "Shatori Walker"] <- "Shatori Walker-Kimbrough"
bad_pass_data$Player[bad_pass_data$Player == "Betnijah Laney"] <- "Betnijah Laney-Hamilton"
bad_pass_data$Player[bad_pass_data$Player == "Cheyenne Parker"] <- "Cheyenne Parker-Tyus"

# Print the updated data frame
bad_pass_data <- bad_pass_data %>%
	arrange(desc(Bad_Pass_Count))
print(bad_pass_data)


#join datasets together
# Merge Pbox and bad_pass_data on "Player" column
merged_data <- merge(Pbox, bad_pass_data, by = "Player")

# Print the merged data
print(merged_data)


#ast to bad pass ratio
#calculate assist to turnover ratio, just basic version
merged_data$a_bp <- merged_data$AST/merged_data$Bad_Pass_Count

#sort data by ast to bad pass ratio, only players with at least 10 minutes per game
merged_data <- merged_data %>%
	filter(MP>=10)%>%
	arrange(desc(a_bp))

print(merged_data)



#scatter plot of assists to bad passes
ggplot(merged_data, aes(x = Bad_Pass_Count, y = AST)) +
  geom_point() +
  geom_text_repel(aes(label = Player), size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Add x=y line
  annotate("text", x = max(merged_data$Bad_Pass_Count), y = max(merged_data$TOV), label = "AST=Bad Passes", vjust = -0.5, hjust = 1, color = "red") +
  ggtitle("Scatter Plot of WNBA Player Bad Passes vs Assists") +
  xlab("Bad Passes") +
  ylab("Assists") +  
  theme_minimal() 
  
  
# Example dataframe
data <- data.frame(
  Name = c("John", "Alice", "Bob"),
  Age = c(25, 30, 22),
  Gender = c("Male", "Female", "Male")
)


##############################################################################################################
#print nice data
# Selecting columns to print
selected_data <- Pbox[, c("Player", "Team", "MP", "AST", "TOV", "A_T")]
selected_data$A_T <- round(selected_data$A_T, 2)
selected_data$MP <- round(selected_data$MP, 2)

# Print the selected columns
print(selected_data)
  
  
# Selecting columns to print
selected_data2 <- merged_data[, c("Player", "Team", "MP", "AST", "TOV", "Bad_Pass_Count", "a_bp")]
selected_data2$a_bp <- round(selected_data2$a_bp, 2)
selected_data2$MP <- round(selected_data2$MP, 2)

# Print the selected columns
print(selected_data2)  
