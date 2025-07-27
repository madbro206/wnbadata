#Use wehoop package to download WNBA data
pacman::p_load(wehoop, dplyr, tidyr, glue, tictoc, progressr, BasketballAnalyzeR)

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
#translate into player, team, and opponent box score

wnba_team_box <- wnba_team_box%>%
  filter(team_name != "TEAM COLLIER" & team_name != "TEAM CLARK")

#team free throws home/awy
Tbox <- wnba_team_box %>%
  group_by(team_name, team_home_away) %>%
  summarise(GP=n(), FTM=sum(free_throws_made, na.rm = TRUE), FTA=sum(free_throws_attempted, na.rm = TRUE), FTp=100*FTM/FTA) %>%
  rename(Team=team_name, Location=team_home_away) %>%
  arrange(desc(FTp)) %>%
  as.data.frame()

Tbox

#opponent team free throws home/away ("Storm away" means that the other team was away, so the Storm were home)
Obox <- wnba_team_box %>%
  group_by(opponent_team_name, team_home_away) %>%
  summarise(GP=n(), FTM=sum(free_throws_made, na.rm = TRUE), 
            FTA=sum(free_throws_attempted, na.rm = TRUE), 
            FTp=100*FTM/FTA) %>%
  rename(Team=opponent_team_name, Opp_Loc=team_home_away) %>%
  arrange(desc(FTp)) %>%
  as.data.frame()

Obox


Obox %>% filter(Opp_Loc=="away") %>% rename(Home_team=Team) %>% select(Home_team, GP, FTM, FTA, FTp)



#general away team FT%
wnba_team_box %>%
  group_by(team_home_away) %>%
  summarise(GP=n(), FTM=sum(free_throws_made, na.rm = TRUE), 
            FTA=sum(free_throws_attempted, na.rm = TRUE), 
            FTp=100*FTM/FTA) %>%
  rename(Location=team_home_away) %>%
  arrange(desc(FTp)) %>%
  as.data.frame()