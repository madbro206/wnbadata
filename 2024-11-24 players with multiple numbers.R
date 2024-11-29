#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, progressr, BasketballAnalyzeR)

wbb_player_box <- wehoop::load_wbb_player_box()

unique_players_2025 <- wbb_player_box %>%
  distinct(athlete_id, team_location, .keep_all = TRUE) %>%	
  select(athlete_display_name, athlete_jersey, team_location)%>%
  mutate(athlete_jersey = as.integer(athlete_jersey))

players_multiple_jerseys <- wbb_player_box %>%
  group_by(athlete_id, athlete_display_name, team_location) %>%
  summarize(unique_jerseys = n_distinct(athlete_jersey, na.rm = TRUE),
            jersey_numbers = paste(unique(athlete_jersey), collapse = ", "),
            .groups = "drop") %>%
  filter(unique_jerseys > 1) %>%
  arrange(desc(unique_jerseys), athlete_display_name) %>%
  select(athlete_display_name, team_location, unique_jerseys, jersey_numbers)


##############players with multiple numbers############################################
subset <- subset(wbb_player_box, wbb_player_box $athlete_display_name %in% players_multiple_jerseys$athlete_display_name) %>%
	select(game_date, athlete_display_name, team_location, athlete_jersey) %>%
	arrange(athlete_display_name)
	
subset