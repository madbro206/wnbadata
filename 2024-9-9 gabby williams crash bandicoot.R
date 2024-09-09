#Use wehoop package to download WNBA data
#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, knitr, kableExtra)

#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(season=c(2018:2024))
})
tictoc::toc()

#############################################################################################################
#find season totals for gabrielle

wnba_player_box_gw <- wnba_player_box %>%
	filter(athlete_display_name=="Gabby Williams" & season_type==2)

gabby <- wnba_player_box_gw %>%
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
#calculate per 39:39 stats
time <- 39+(39/60)

gabby_per_3939 <- gabby %>%
  mutate(
    PTS = time*PTS / MIN,
    P2M = time*P2M / MIN,
    P2A = time*P2A / MIN,
    P3M = time*P3M / MIN,
    P3A = time*P3A / MIN,
    FTM = time*FTM / MIN,
    FTA = time*FTA / MIN,
    OREB = time*OREB / MIN,
    DREB = time*DREB / MIN,
    AST = time*AST / MIN,
    TOV = time*TOV / MIN,
    STL = time*STL / MIN,
    BLK = time*BLK / MIN,
    PF = time*PF / MIN
  ) %>% 
  mutate(across(where(is.numeric), round, 2))

# Create a nicely formatted table
gabby_per_3939 %>%
  kable(digits = 2,  # Round to 2 decimal places in the table
        col.names = c("Season", "Team", "Player", "GP", "MIN", "PTS", "P2M", "P2A", "P2p", "P3M", "P3A", "P3p", "FTM", "FTA", "FTp", "OREB", "DREB", "AST", "TOV", "STL", "BLK", "PF")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = F, 
                font_size = 14)
