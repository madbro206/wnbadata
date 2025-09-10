pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, ggplot2)

#load data

#wnba full play by play
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp()
})
tictoc::toc()
#######################################################################################################
#coaches challenges data
c <- subset(wnba_pbp, type_text=="Coach's Challenge (Stands)"| type_text=="Coach's Challenge (Supported)"| type_text=="Coach's Challenge (Overturned)" | type_text=="Coach's Challenge (replaycenter)")

#double check there aren't more rows
#wnba_pbp %>% filter(str_detect(type_text, "Coach's Challenge"))



#fake (was a problem in 2024 stats, doesn't seem to be in 2025)
c <- c %>% filter(type_text != "Coach's Challenge (replaycenter)")

#combine "Supported" and "Stands" (both mean the coach lost the challenge)
c$type_text <- gsub("Coach's Challenge \\(Stands\\)", "Stands", c$type_text)
c$type_text <- gsub("Coach's Challenge \\(Supported\\)", "Stands", c$type_text)
c$type_text <- gsub("Coach's Challenge \\(Overturned\\)", "Overturned", c$type_text)

#extract team names
c$team <- sub(".*\\[(.*?)\\].*", "\\1", c$text)
c$team[c$team == "Golden State Valkyries"] <- "Valkyries"


#see what's up with the 2025 all star teams
#subset(c, team=="Team Collier" | team=="Team Clark")

##############################################################################################################
#summarize the data by team and challenge result
challenge_summary <- c %>%
  filter(team != "Team Clark" & team != "Team Collier") %>% #not all star game
  filter(season_type==2 & season==2025) %>% #not preseason or playoffs
  group_by(team, type_text) %>%
  summarize(count = n(), .groups = 'drop')

#pivot data to have separate columns for "Overturned" and "Stands"
challenge_summary <- challenge_summary %>%
  pivot_wider(names_from = type_text, values_from = count, values_fill = 0)

colnames(challenge_summary) <- c("team", "Overturned", "Stands")

challenge_summary <- challenge_summary[, c("team", "Overturned", "Stands")]

challenge_summary$total <- challenge_summary$Overturned+challenge_summary$Stands

#calculate success rate
challenge_summary$Success_Rate <- challenge_summary$Overturned/(challenge_summary$Overturned+challenge_summary$Stands)*100

#arrange by success rate
challenge_summary <- challenge_summary%>%
	arrange(desc(Success_Rate))

# Print the modified summary
print(challenge_summary)

#overall success rate
tot <- sum(challenge_summary$Overturned)/(sum(challenge_summary$Overturned)+sum(challenge_summary$Stands))*100
tot

##############################################################################################################
#vis
# Pivot longer and set the order of levels for Result
challenge_summary_long <- challenge_summary %>%
  pivot_longer(cols = c(Overturned, Stands), names_to = "Result", values_to = "Count") %>%
  mutate(Result = factor(Result, levels = c("Stands", "Overturned")))

# Plot with reordered Result and vertical bars
ggplot(challenge_summary_long, aes(x = reorder(team, -total), y = Count, fill = Result)) +
  geom_bar(stat = "identity") +
  labs(title = "WNBA Coach's Challenges by Team", subtitle='through 9/7/2025, arranged by total challenges',x = "Team", y = "Total Challenges") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##############################################################################################################
#how many successful challenges per team in a single game?
#add col to categorize challenges as first vs second per game
per_game <- c %>%
  transmute(game_id, game_date, team, event_id = game_play_number %||% row_number(),  
            period,
            type_text) %>%
  group_by(game_id, team) %>%
  mutate(challenge_index = row_number()) %>%
  ungroup()

#add col to categoize success (1) vs not (0)
summary_pg <- per_game %>%
  group_by(game_id, game_date, team) %>%
  summarise(
    challenges_used = n(),
    successes = sum(type_text == "Overturned", na.rm = TRUE),
    events = paste(event_id, collapse = ","),
    .groups = "drop"
  )

#teams that used one challenge in a single game 
games_one_used <- summary_pg %>%
  filter(challenges_used == 1)

games_one_used

#teams that used both (2) challenges in a single game 
games_both_used <- summary_pg %>%
  filter(challenges_used >= 2)  # should be exactly 2 per rules; keep >= 2 as guard

print(games_both_used, n=45)


#teams that had both challenges successful in one game
games_both_success <- summary_pg %>%
  filter(challenges_used >= 2, successes >= 2)

print(games_both_success, n=27)

teams_both_success_counts <- games_both_success %>%
  count(team, name = "count") %>%
  arrange(desc(count))

print(teams_both_success_counts)

#just spot checking
#subset(c, team=="Fever" & game_id==401736380)


##############################################################################################################
details_both_used <- per_game %>%
  semi_join(games_both_used, by = c("game_id","team")) %>%
  arrange(game_id, team, challenge_index)

details_both_used

details_both_success <- per_game %>%
  semi_join(games_both_success, by = c("game_id","team")) %>%
  arrange(game_id, team, challenge_index)

details_both_success
