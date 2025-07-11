#Use wehoop package to download WNBA or NCAAW data
#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

#if you need to install the packages:
install.packages("wehoop")
install.packages("dplyr")

#load the packages
library(wehoop)
library(dplyr)


#wnba player full box score
wnba_player_box <- wehoop::load_wnba_player_box(season=c(2014:2025))


at_near_td <- wnba_player_box %>%
  filter(athlete_display_name=="Alyssa Thomas") %>%
  filter(points>=8 & rebounds >=8 & assists >=8) %>%
  filter(points<10 | rebounds <10 | assists <10) %>%
  arrange(desc(game_date)) %>%
  select("game_date", "opponent_team_name", "points", "rebounds", "assists")

print(at_near_td,n=55)



wnba_player_box %>%
  filter(points>=8 & rebounds >=8 & assists >=8) %>%
  arrange(desc(game_date)) %>%
  select("game_date", "opponent_team_name", "athlete_display_name", "points", "rebounds", "assists")
