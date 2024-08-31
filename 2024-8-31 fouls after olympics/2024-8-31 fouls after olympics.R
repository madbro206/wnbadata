#Use wehoop package to download WNBA data

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, ggplot2, forcats)

#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box()
})
tictoc::toc()

#######################################################################################################
#translate into player cumulative box score
Pbox <- wnba_player_box %>%
  group_by(season, athlete_display_name) %>%
  summarise(GP=sum(minutes>0, na.rm=TRUE), MIN=sum(minutes, na.rm = TRUE), PTS=sum(points, na.rm = TRUE),
    PF=sum(fouls, na.rm = TRUE)) %>%
  rename(Season=season,
         Player=athlete_display_name) %>%
  as.data.frame()
  
#list of Olympic players
olympic_players <- c(
  "Amy Atwell", "Cayla George", "Lauren Jackson", "Tess Magden", "Marianna Tolo",
  "Emma Meesseman", "Natalie Achonwa", "Kayla Alexander", "Nirra Fields", "Li Meng",
  "Han Xu", "Valeriane Ayayi", "Marine Johannès", "Iliana Rupert", "Gabby Williams",
  "Marie Gülich", "Alexis Peterson", "Rui Machida", "Arella Guirantes", "Yvonne Anderson",
  "Tina Krajišnik", "Maite Cazorla", "Leticia Romero", "Tiffany Hayes", "Dearica Hamby",
  "Rhyne Howard", "Anneli Maley", "Marcedes Walker", "Cierra Burdick", "Ezi Magbegor",
  "Jade Melbourne", "Alanna Smith", "Stephanie Talbot", "Kristy Wallace", "Sami Whitcomb",
  "Julie Vanloo", "Laeticia Amihere", "Bridget Carleton", "Aaliyah Edwards", "Kia Nurse",
  "Li Yueru", "Leonie Fiebich", "Nyara Sabally", "Satou Sabally", "Megan Gustafson",
  "Napheesa Collier", "Kahleah Copper", "Chelsea Gray", "Brittney Griner", "Sabrina Ionescu",
  "Jewell Loyd", "Kelsey Plum", "Breanna Stewart", "Diana Taurasi", "Alyssa Thomas", 
  "A'ja Wilson", "Jackie Young"
)

# Filter the dataframe based on Olympic players and pre/post olympics
olympic_pre <- wnba_player_box %>%
  filter(athlete_display_name %in% olympic_players) %>%
  filter(game_date < as.Date("2024-08-01")) %>%
   group_by(season, athlete_display_name) %>%
  summarise(GP=sum(minutes>0, na.rm=TRUE), MIN=sum(minutes, na.rm = TRUE), PTS=sum(points, na.rm = TRUE), PF=sum(fouls, na.rm = TRUE)) %>%
  rename(Season=season, Player=athlete_display_name) %>%
  as.data.frame()

olympic_post <- wnba_player_box %>%
  filter(athlete_display_name %in% olympic_players) %>%
  filter(game_date > as.Date("2024-08-01")) %>%
   group_by(season, athlete_display_name) %>%
  summarise(GP=sum(minutes>0, na.rm=TRUE), MIN=sum(minutes, na.rm = TRUE), PTS=sum(points, na.rm = TRUE), PF=sum(fouls, na.rm = TRUE)) %>%
  rename(Season=season, Player=athlete_display_name) %>%
  as.data.frame()

nonolympic_pre <- wnba_player_box %>%
  filter(!athlete_display_name %in% olympic_players) %>%
  filter(game_date < as.Date("2024-08-01")) %>%
   group_by(season, athlete_display_name) %>%
  summarise(GP=sum(minutes>0, na.rm=TRUE), MIN=sum(minutes, na.rm = TRUE), PTS=sum(points, na.rm = TRUE), PF=sum(fouls, na.rm = TRUE)) %>%
  rename(Season=season, Player=athlete_display_name) %>%
  as.data.frame()

nonolympic_post <- wnba_player_box %>%
  filter(!athlete_display_name %in% olympic_players) %>%
  filter(game_date > as.Date("2024-08-01")) %>%
   group_by(season, athlete_display_name) %>%
  summarise(GP=sum(minutes>0, na.rm=TRUE), MIN=sum(minutes, na.rm = TRUE), PTS=sum(points, na.rm = TRUE), PF=sum(fouls, na.rm = TRUE)) %>%
  rename(Season=season, Player=athlete_display_name) %>%
  as.data.frame()


############################################################################################################
# Function to calculate summary stats
calculate_summary <- function(df) {
  df %>%
    group_by(season, athlete_display_name) %>%
    summarise(
      GP = sum(minutes > 0, na.rm = TRUE),
      MIN = sum(minutes, na.rm = TRUE),
      PF = sum(fouls, na.rm = TRUE)
    ) %>%
    mutate(
      PF_per_GP = PF / GP,
      PF_per_MIN = PF / MIN
    ) %>%
    rename(Season = season, Player = athlete_display_name) %>%
    as.data.frame()
}

# Filter and calculate for Olympic players before and after the Olympics
olympic_pre <- wnba_player_box %>%
  filter(athlete_display_name %in% olympic_players) %>%
  filter(game_date < as.Date("2024-08-01")) %>%
  calculate_summary()

olympic_post <- wnba_player_box %>%
  filter(athlete_display_name %in% olympic_players) %>%
  filter(game_date >= as.Date("2024-08-01")) %>%
  calculate_summary()

# Filter and calculate for non-Olympic players before and after the Olympics
nonolympic_pre <- wnba_player_box %>%
  filter(!athlete_display_name %in% olympic_players) %>%
  filter(game_date < as.Date("2024-08-01")) %>%
  calculate_summary()

nonolympic_post <- wnba_player_box %>%
  filter(!athlete_display_name %in% olympic_players) %>%
  filter(game_date >= as.Date("2024-08-01")) %>%
  calculate_summary()


# Calculate total PF divided by total MIN for Olympic players before the Olympics
olympic_pre_summary <- olympic_pre %>%
  summarise(
    Total_PF = sum(PF, na.rm = TRUE),
    Total_MIN = sum(MIN, na.rm = TRUE),
    PF_per_MIN = Total_PF / Total_MIN
  )

# Calculate total PF divided by total MIN for Olympic players after the Olympics
olympic_post_summary <- olympic_post %>%
  summarise(
    Total_PF = sum(PF, na.rm = TRUE),
    Total_MIN = sum(MIN, na.rm = TRUE),
    PF_per_MIN = Total_PF / Total_MIN
  )

# Calculate total PF divided by total MIN for non-Olympic players before the Olympics
nonolympic_pre_summary <- nonolympic_pre %>%
  summarise(
    Total_PF = sum(PF, na.rm = TRUE),
    Total_MIN = sum(MIN, na.rm = TRUE),
    PF_per_MIN = Total_PF / Total_MIN
  )

# Calculate total PF divided by total MIN for non-Olympic players after the Olympics
nonolympic_post_summary <- nonolympic_post %>%
  summarise(
    Total_PF = sum(PF, na.rm = TRUE),
    Total_MIN = sum(MIN, na.rm = TRUE),
    PF_per_MIN = Total_PF / Total_MIN
  )

# View the summaries
olympic_pre_summary
olympic_post_summary
nonolympic_pre_summary
nonolympic_post_summary

############################################################################################################
#compare individual olympic players before/after the games
# Combine the pre- and post-Olympics data using "Player" as the key
comparison <- olympic_pre %>%
  inner_join(olympic_post, by = "Player", suffix = c("_pre", "_post"))

comparison <- comparison %>%
  mutate(
    Difference_PF_per_MIN = PF_per_MIN_post - PF_per_MIN_pre,
    Difference_PF_per_GP = PF_per_GP_post - PF_per_GP_pre
  ) 

# Convert Player to a factor if it is not already
comparison$Player <- as.factor(comparison$Player)

# Handle NAs in Difference_PF_per_MIN by removing rows with NAs (if any)
comparison <- comparison[!is.na(comparison$Difference_PF_per_MIN), ]

# Plot
ggplot(comparison, aes(x = fct_reorder(Player, Difference_PF_per_MIN), y = Difference_PF_per_MIN)) +
  geom_bar(stat = "identity", fill="#74618f") +
  coord_flip() +
  labs(title = "Are Olympic WNBA players fouling more after the games?", subtitle="Difference in personal fouls per minute before/after the Olympics", caption="includes 3x3 and 5x5 players | stats.wnba.com | @wnbadata",
       x = "Player", y = "Increase in personal fouls per minute") +
       theme_minimal()

sum(comparison$Difference_PF_per_MIN)
sum(comparison$Difference_PF_per_GP)
############################################################################################################
#compare individual nonolympic players before/after the games
# Combine the pre- and post-Olympics data using "Player" as the key
comparison2 <- nonolympic_pre %>%
  inner_join(nonolympic_post, by = "Player", suffix = c("_pre", "_post"))

comparison2 <- comparison2 %>%
  mutate(
    Difference_PF_per_MIN = PF_per_MIN_post - PF_per_MIN_pre,
    Difference_PF_per_GP = PF_per_GP_post - PF_per_GP_pre
  )

# Convert Player to a factor if it is not already
comparison2$Player <- as.factor(comparison2$Player)

# Handle NAs in Difference_PF_per_MIN by removing rows with NAs (if any)
comparison2 <- comparison2[!is.na(comparison2$Difference_PF_per_MIN), ]

# Plot
ggplot(comparison2, aes(x = fct_reorder(Player, Difference_PF_per_MIN), y = Difference_PF_per_MIN)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Difference in Personal Fouls per Minute After the Olympics",
       x = "Player", y = "Difference in PF per Minute")

############################################################################################################
#make some nice tables
library(dplyr)

# Select the desired columns
comparison_selected <- comparison %>%
  select(Player, MIN_pre, PF_pre, PF_per_MIN_pre, MIN_post, PF_post, PF_per_MIN_post)

# Display the table
comparison_selected

library(knitr)
# For a basic table
kable(comparison_selected)

# Alternatively, using the gt package for a more styled table
library(gt)
gt(comparison_selected)
