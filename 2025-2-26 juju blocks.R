#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR)

#load data
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


#USC vs UCLA game no.1 (juju had 8 blocks)
usc_ucla <- subset(wbb_pbp, home_team_name == "USC" & away_team_name== "UCLA")




#blocks and results
result_df <- wbb_pbp %>%
  # Create a logical flag that is TRUE if the current row is "Block Shot"
  # or if the previous row was "Block Shot"
  mutate(include = type_text == "Block Shot" | lag(type_text) == "Block Shot") %>%
  # Filter to keep only those rows
  filter(include) %>%
  # Remove the helper column
  select(-include) %>%
  select(game_play_number, type_text, text, period_number, clock_display_value)

print(result_df, n=20)


#count what happens after each block
aftermath <- subset(result_df, type_text != "Block Shot")

aftermath_count <- aftermath %>%
  group_by(type_text) %>%
  summarise(count = n())

print(aftermath_count)


#sort aftermath into offensive vs defensive team gaining possession
aftermath_count_categorized <- aftermath_count %>%
  mutate(category = case_when(
    type_text %in% c("Defensive Rebound", "Lost Ball Turnover", "Steal") ~ "Defensive Possession",
    type_text %in% c("Offensive Rebound", "DunkShot", "JumpShot", "LayUpShot", "MadeFreeThrow", "TipShot") ~ "Offensive Possession",
    TRUE ~ "Not Sure"
  ))

aftermath_category_summary <- aftermath_count_categorized %>%
  group_by(category) %>%
  summarise(total_count = sum(count))

print(aftermath_count_categorized)
print(aftermath_category_summary)


#general aftermath count
ggplot(aftermath_category_summary, aes(x="", y=total_count, fill=category)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() 


15349/(15349+604+13180)




#the dunk? feb 8 SD vs oregon state
subset(wbb_pbp, text=="Kylie Horstmeyer missed Dunk." & period_number==4)$game_date


#juju aftermath counts
juju <- wbb_pbp %>%
  mutate(include = text == "JuJu Watkins Block." | lag(text) == "JuJu Watkins Block.") %>%
  filter(include) %>%
  select(-include) %>%
  select(game_date, game_play_number, type_text, text, period_number, clock_display_value)

print(juju, n=106)

juj_aftermath <- subset(juju, type_text != "Block Shot")

juj_aftermath_count <- juj_aftermath %>%
  group_by(type_text) %>%
  summarise(count = n())



#sort aftermath into offensive vs defensive team gaining possession
juj_aftermath_count_categorized <- juj_aftermath_count %>%
  mutate(category = case_when(
    type_text %in% c("Defensive Rebound", "Lost Ball Turnover", "Steal") ~ "Defensive Possession",
    type_text %in% c("Offensive Rebound", "DunkShot", "JumpShot", "LayUpShot", "MadeFreeThrow", "TipShot") ~ "Offensive Possession",
    TRUE ~ "Not Sure"
  ))

juj_aftermath_category_summary <- juj_aftermath_count_categorized %>%
  group_by(category) %>%
  summarise(total_count = sum(count))

print(juj_aftermath_category_summary)



juju2 <- juju %>%
  filter(text=="JuJu Watkins Block.") %>%
  group_by(game_date) %>%
  summarise(count = n())

print(juju2, n=53)


#################### player blocks vs fouls in 2025 season ###########################
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
  #filter(MIN>300) %>%
  as.data.frame()

ggplot(Pbox, aes(x=BLK, y=PF, size = MIN)) +
  geom_point(alpha=0.4, color="navyblue") +
  scale_size(range = c(.2, 3)) +
  xlab("Player Blocks") + 
  ylab("Player Fouls") +
  labs(title = "NCAAW Player Blocks vs Fouls", subtitle="2024-25 season through 2/26", caption = "data: wehoop | graphic: @wnbadata") + 
    scale_x_continuous(breaks = seq(0, 100, by = 10), minor_breaks = NULL) +
    scale_y_continuous(breaks = seq(0, 100, by = 10), minor_breaks = NULL) +
  theme_minimal()

cor(Pbox$BLK, Pbox$PF)
