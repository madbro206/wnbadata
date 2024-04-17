#create timeline of number 1 picks career and championships


# Load required libraries
library(ggplot2)

# Your dataframe
df <- data.frame(
  Draft_Year = c(1997, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 
                 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 
                 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023),
  Team = c("Starzz", "Comets", "Starzz", "Mystics", "Rockers", "Storm", "Storm", 
           "Rockers", "Mercury", "Sting", "Lynx", "Mercury", "Sparks", "Dream", 
           "Sun", "Lynx", "Sparks", "Mercury", "Sun", "Storm", "Storm", "Stars", 
           "Aces", "Aces", "Aces", "Liberty", "Wings", "Dream"),
  Player = c("Dena Head", "Tina Thompson", "Margo Dydek", "Chamique Holdsclaw", 
             "Ann Wauters", "Lauren Jackson", "Sue Bird", "LaToya Thomas", 
             "Diana Taurasi", "Janel McCarville", "Seimone Augustus", 
             "Lindsey Harding", "Candace Parker", "Angel McCoughtry", 
             "Tina Charles", "Maya Moore", "Nneka Ogwumike", "Brittney Griner", 
             "Chiney Ogwumike", "Jewell Loyd", "Breanna Stewart", "Kelsey Plum", 
             "A'ja Wilson", "Jackie Young", "Sabrina Ionescu", "Charli Collier", 
             "Rhyne Howard", "Aliyah Boston"),
  Champ_team = c(NA, "Comets", NA, NA, "Sparks", "Storm", "Storm", NA, "Mercury", 
                 "Lynx", "Lynx", NA, "Sparks, Sky, Aces", NA, NA, "Lynx", "Sparks", 
                 "Mercury", NA, "Storm", "Storm", "Aces", "Aces", "Aces", NA, NA, NA, NA),
  Champ_year = c(NA, "1997, 1998, 1999, 2000", NA, NA, "2016", "2004, 2010", 
                 "2004, 2010, 2018, 2020", NA, "2007, 2009, 2014", "2013", 
                 "2011, 2013, 2015, 2017", NA, "2016, 2021, 2023", NA, NA, "2011, 2013, 2015, 2017", 
                 "2016", "2014", NA, "2018, 2020", "2018, 2020", "2022, 2023", "2022, 2023", 
                 "2022, 2023", NA, NA, NA, NA)
)

# Separate Champ_year into multiple rows
champ_df <- tidyr::separate_rows(df, Champ_year, sep = ", ")

# Convert Champ_year to numeric
champ_df$Champ_year <- as.numeric(champ_df$Champ_year)

# Convert Player to factor with levels arranged by draft year in reverse order
champ_df$Player <- factor(champ_df$Player, levels = unique(champ_df$Player[order(champ_df$Draft_Year, decreasing = TRUE)]))

# Plot the timelines with rotated x-axis labels
ggplot(champ_df, aes(x = Champ_year, y = Player, color = Champ_team)) +
  geom_segment(aes(xend = Champ_year), size = 3) +
  geom_point(aes(x = Champ_year), size = 3) +
  #geom_segment(data = tina_thompson_df, aes(x = 1997, xend = 2002, y = Player, yend = Player), size = 0.5, color = "black") + # Add horizontal line for Tina Thompson
  scale_x_continuous(expand = c(0, 0), limits = c(1996.5, 2023.5), breaks = seq(1997, 2023, by = 1)) +
  labs(x = "Year", y = "No.1 Pick", title = "WNBA No.1 Picks and Championships") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))


# Filter the data for Tina Thompson
tina_thompson_df <- played_df %>%
  filter(Player == "Tina Thompson")

# Plot the timelines
ggplot(played_df, aes(x = Champ_year, y = Player, color = Champ_team)) +
  geom_segment(aes(xend = Champ_year), size = 3) +
  geom_point(aes(x = Champ_year), size = 3) +
  geom_segment(aes(x = years_played, xend = Champ_year, yend = Player), size = 1, color = "black") +
  geom_segment(aes(x = 1997, xend = 2013, y = "Tina Thompson", yend = "Tina Thompson"), size = 1, color = "blue") + # Add horizontal line for Tina Thompson
  scale_x_continuous(expand = c(0, 0), limits = c(1995, 2025), breaks = seq(1997, 2023, by = 1)) +
  labs(x = "Year", y = "Player", title = "WNBA Players' Championship Years and Years Played") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = seq(1997, 2023, by = 1), linetype = "dotted", color = "grey", alpha = 0.5)
