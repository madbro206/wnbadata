#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, tidyr, glue, tictoc, progressr, BasketballAnalyzeR, ggplot2, gt)

#load data
#wnba player full box score since 2002
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(season=c(2002:2024))
})
tictoc::toc()

#bday data from
# https://acrossthetimeline.com/wnba/data.html#query=2&set=wnba-demo
data <- read.csv("~/Desktop/wehoop/2024-8-24 age vs pts/current_player_ages.csv", header = TRUE)

##############################################################################################################
#calculate player total career points
Pbox <- wnba_player_box %>%
  #filter(team != "Team Stewart" & team != "Team Wilson" & team != "Team WNBA" & team != "Team USA") %>%
  filter(season_type==2) %>% #regular season only
  group_by(athlete_display_name) %>%
  summarise(GP=sum(minutes>0, na.rm=TRUE), PTS=sum(points, na.rm = TRUE)) %>%
  rename(Player=athlete_display_name) %>%
  as.data.frame()

#calculate player ages in days
# Convert Bday to Date format 
data$Bday <- as.Date(data$Bday, format = "%B %d, %Y") 
# Calculate age in days 
data <- data %>% mutate(Age_in_days = as.numeric(Sys.Date() - Bday)) 


# join age and pts dataframes
data <- data %>%
  left_join(Pbox %>% select(Player, PTS), by = "Player")
  
#have to fix DT's pts since the data source isn't complete for some seasons
data <- data %>%
  mutate(PTS = if_else(Player == "Diana Taurasi", 10518 + 10+9+13+18+13+3+4+12, PTS))
  

##############################################################################################################
#chart
# Remove rows with missing values in PTS or Age_in_days
data_clean <- data %>%
  filter(!is.na(PTS) & !is.na(Age_in_days))

# Calculate the points-to-age ratio
data_clean <- data_clean %>%
  mutate(PTS_to_Age_Ratio = PTS / Age_in_days)
  
#sort by pts to age ratio
data_clean <- data_clean %>%
	arrange(desc(PTS_to_Age_Ratio))

# Create the scatterplot
library(scales)  #for comma function

ggplot(data_clean, aes(x = Age_in_days, y = PTS)) +
  geom_point(color = "#5bb2c9") +             # Scatter plot points
  labs(x = "Age in Days", y = "Non-playoff career points", 
       title = "Active WNBA Player Career PTS vs Age in Days", 
       subtitle = "players below DB line will be eliminated from the league, sad", 
       caption = "Data: basketball-reference, acrossthetimeline | Graphic: @wnbadata") +
  scale_x_continuous(breaks = seq(0, max(data_clean$Age_in_days, na.rm = TRUE), by = 1000), 
                     labels = scales::comma) +  # X-axis breaks every 1000
  scale_y_continuous(breaks = seq(0, max(data_clean$PTS, na.rm = TRUE), by = 1000), 
                     labels = scales::comma) +  # Y-axis breaks every 1000
  geom_abline(intercept = 0, slope = 0.5451250185, color = "#74618f", linetype = "dashed") +  
  annotate("text", x = 14000, y = 15500 * 0.546, label = "DB Line", color = "#74618f", hjust = -0.1) + 
  geom_text(data = subset(data_clean, Player %in% c("Diana Taurasi", "DeWanna Bonner", "Tina Charles")),
            aes(label = Player), vjust = 0, hjust = 1, color = "black", size=3) +  # Label the specific points
  theme_minimal()


##############################################################################################################
