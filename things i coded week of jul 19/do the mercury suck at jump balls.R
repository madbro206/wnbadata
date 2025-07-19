#Use wehoop package to download WNBA data
#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

#this loads all the packages I need at once, i could instead just do "library(wehoop)" for each package
pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, stringr, tidyr)

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

#mercury game ids
game_ids <- unique(subset(wnba_team_box, team_name=="Mercury")$game_id)

#jump balls in pbp
jump_balls <- subset(wnba_pbp, type_text=="Jump Ball" |type_text=="Jumpball")

#mercury jump balls
merc_jb <- subset(jump_balls, game_id %in% game_ids)


#find player who gets the ball
mercury_jump_balls_with_winner <- merc_jb %>%
  mutate(
    possession_player = str_extract(text, "\\((.*?) gains possession\\)"),
    possession_player = str_replace_all(possession_player, "\\(| gains possession\\)", ""),
    possession_player = str_trim(possession_player)
  )

#join with player box score to get the team of the possession player
full_jump_ball_info <- full_jump_ball_info %>%
  mutate(
    mercury_win = case_when(
      is.na(possession_player) | is.na(team_name) ~ NA_character_,
      team_name == "Mercury" ~ "Mercury",
      TRUE ~ "Other Team"
    )
  )

#count how many jump balls had each outcome
jump_ball_counts <- full_jump_ball_info %>%
  count(mercury_win, name = "count")

print(jump_ball_counts)

merc_jb$text


###################################### all teams ######################################################
#this part is AI generated i was lazy
# Step 1: Filter jump balls
jump_balls <- wnba_pbp %>%
  filter(grepl("Jump Ball", type_text, ignore.case = TRUE) | grepl("Jumpball", type_text, ignore.case = TRUE)) %>%
  mutate(
    winner = str_extract(text, "\\((.*?) gains possession\\)"),
    winner = str_replace_all(winner, "\\(| gains possession\\)", ""),
    winner = str_trim(winner)
  )

# Step 2: Map winner to team via player box scores
jump_balls_full <- jump_balls %>%
  left_join(
    wnba_player_box %>% select(game_id, athlete_display_name, team_name),
    by = c("game_id", "winner" = "athlete_display_name")
  )

# Step 3: Assign win/loss/NA for both teams in game
jump_balls_results <- jump_balls_full %>%
  rowwise() %>%
  mutate(
    game = game_id,  # prevent scope issues
    teams_in_game = list(
      wnba_player_box %>%
        filter(game_id == game) %>%
        pull(team_name) %>%
        unique()
    ),
    outcomes = list(
      sapply(teams_in_game, function(team) {
        if (is.na(team_name)) return(NA_character_)
        if (team == team_name) return("Win")
        return("Loss")
      })
    )
  ) %>%
  ungroup() %>%
  unnest(cols = c(teams_in_game, outcomes)) %>%
  rename(team = teams_in_game, result = outcomes)

# Step 4: Aggregate jump ball results by team
jump_ball_summary <- jump_balls_results %>%
  group_by(team, result) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = result, values_from = n)

# Step 5: Add missing columns if they don't exist
if (!"Win" %in% names(jump_ball_summary)) jump_ball_summary$Win <- 0
if (!"Loss" %in% names(jump_ball_summary)) jump_ball_summary$Loss <- 0
if (!"NA" %in% names(jump_ball_summary)) jump_ball_summary$`idk` <- 0

# Step 6: Compute final stats
jump_ball_summary <- jump_ball_summary %>%
  mutate(
    win_rate = ifelse((Win + Loss) > 0, Win / (Win + Loss), NA_real_)
  ) %>%
  arrange(desc(win_rate))

jump_ball_summary
