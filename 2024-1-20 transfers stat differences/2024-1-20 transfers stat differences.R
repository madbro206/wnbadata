#how to get the data

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR)

tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box(c(2023,2024))#just last season vs this season
})
tictoc::toc()

#data frame with cumulative box score stats for every player
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
         filter(GP>5) %>%
  as.data.frame()
  
result <- Pbox %>%
  group_by(Player) %>%
  filter(n_distinct(Team) == 2) %>%
  arrange(Player)
  
#find average box score stats for each player, taking only the ones I care about for this video :)
Pbox_avg <- result %>%
  mutate(ppg = PTS/GP, apg = AST/GP, rpg=(OREB+DREB)/GP, spg=STL/GP, bpg= BLK/GP, tpg = TOV/GP, mpg=MIN/GP) %>%
  select(Season, Player, Team, GP, mpg, ppg, apg, rpg, spg, bpg, tpg, P2p, P3p, FTp) 

#summarize the difference in stats between 2023 and 2024 for each player
diff <- Pbox_avg %>%
  summarize(
    Team_2023 = first(Team[Season == 2023]),
    Team_2024 = first(Team[Season == 2024]),
    MIN_diff=  first(mpg[Season == 2024]) - first(mpg[Season == 2023]),
    PPG_diff = first(ppg[Season == 2024]) - first(ppg[Season == 2023]),
    APG_diff = first(apg[Season == 2024]) - first(apg[Season == 2023]),
    RPG_diff = first(rpg[Season == 2024]) - first(rpg[Season == 2023]),
    SPG_diff = first(spg[Season == 2024]) - first(spg[Season == 2023]),
    BPG_diff = first(bpg[Season == 2024]) - first(bpg[Season == 2023]),
    TPG_diff = first(tpg[Season == 2024]) - first(tpg[Season == 2023]),
    P2P_diff = first(P2p[Season == 2024]) - first(P2p[Season == 2023]),
    P3P_diff = first(P3p[Season == 2024]) - first(P3p[Season == 2023]),
    FTP_diff = first(FTp[Season == 2024]) - first(FTp[Season == 2023])
  )
  
#calculate the average of each _diff column in result, ignoring NA and NaN
averages <- diff %>%
  summarize(
  	avg_MIN_diff= mean(MIN_diff, na.rm= TRUE),
    avg_PPG_diff = mean(PPG_diff, na.rm = TRUE),
    avg_APG_diff = mean(APG_diff, na.rm = TRUE),
    avg_RPG_diff = mean(RPG_diff, na.rm = TRUE),
    avg_SPG_diff = mean(SPG_diff, na.rm = TRUE),
    avg_BPG_diff = mean(BPG_diff, na.rm = TRUE),
    avg_TPG_diff = mean(TPG_diff, na.rm = TRUE),
    avg_P2P_diff = mean(P2P_diff, na.rm = TRUE),
    avg_P3P_diff = mean(P3P_diff, na.rm = TRUE),
    avg_FTP_diff = mean(FTP_diff, na.rm = TRUE)
  )

# Display the averages
print(averages)


#do the same as above for per-minutes stats

per_min <- result %>%
  mutate(ppm = PTS/MIN, apm = AST/MIN, rpm=(OREB+DREB)/MIN, spm=STL/MIN, bpm= BLK/MIN, tpm = TOV/MIN) %>%
  select(Season, Player, Team, GP, MIN, ppm, apm, rpm, spm, bpm, tpm, P2p, P3p, FTp) 

diff_min <- per_min %>%
  summarize(
    Team_2023 = first(Team[Season == 2023]),
    Team_2024 = first(Team[Season == 2024]),
    PPM_diff = first(ppm[Season == 2024]) - first(ppm[Season == 2023]),
    APM_diff = first(apm[Season == 2024]) - first(apm[Season == 2023]),
    RPM_diff = first(rpm[Season == 2024]) - first(rpm[Season == 2023]),
    SPM_diff = first(spm[Season == 2024]) - first(spm[Season == 2023]),
    BPM_diff = first(bpm[Season == 2024]) - first(bpm[Season == 2023]),
    TPM_diff = first(tpm[Season == 2024]) - first(tpm[Season == 2023]),
    P2P_diff = first(P2p[Season == 2024]) - first(P2p[Season == 2023]),
    P3P_diff = first(P3p[Season == 2024]) - first(P3p[Season == 2023]),
    FTP_diff = first(FTp[Season == 2024]) - first(FTp[Season == 2023])
  )
  
averages_min <- diff_min %>%
  summarize(
    avg_PPM_diff = mean(PPM_diff, na.rm = TRUE),
    avg_APM_diff = mean(APM_diff, na.rm = TRUE),
    avg_RPM_diff = mean(RPM_diff, na.rm = TRUE),
    avg_SPM_diff = mean(SPM_diff, na.rm = TRUE),
    avg_BPM_diff = mean(BPM_diff, na.rm = TRUE),
    avg_TPM_diff = mean(TPM_diff, na.rm = TRUE),
    avg_P2P_diff = mean(P2P_diff, na.rm = TRUE),
    avg_P3P_diff = mean(P3P_diff, na.rm = TRUE),
    avg_FTP_diff = mean(FTP_diff, na.rm = TRUE)
  )
  
library(knitr)
averages_r <- round(averages, 2)
averages_t <- t(averages_r)
kable(averages_t, format = "markdown", caption="Average Per-Game Stat Differences for Transfers Between 2023 and 2024")

hvl <- subset(diff, Player=="Hailey Van Lith")
hvl_t <- t(hvl)
kable(hvl_t, format = "markdown", caption="HVL Per-Game Difference 2023 to 2024")
