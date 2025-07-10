#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, stringr, ggplot2)

#load data
wnba_team_box <- wehoop::load_wnba_team_box()

#list of unique team names
teams <- unique(c(wnba_team_box$team_name, wnba_team_box$opponent_team_name))

#generate all possible pairs
all_pairs <- t(combn(teams, 2))
all_pairs_df <- as.data.frame(all_pairs)
colnames(all_pairs_df) <- c("team1", "team2")

played_pairs <- unique(
  t(apply(wnba_team_box[, c("team_name", "opponent_team_name")], 1, sort))
)
played_pairs <- as.data.frame(played_pairs)
colnames(played_pairs) <- c("team1", "team2")

# Original played_pairs dataframe
played_pairs_reversed <- played_pairs %>%
  rename(team1 = team2, team2 = team1)

# Combine both
played_pairs_doubled <- bind_rows(played_pairs, played_pairs_reversed)

played_pairs_df <- as.data.frame(played_pairs_doubled)
colnames(played_pairs_df) <- c("team1", "team2")

not_played_df <- anti_join(all_pairs_df, played_pairs_df, by = c("team1", "team2"))
not_played_df 
