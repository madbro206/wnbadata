#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package
#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

pacman::p_load(wehoop, dplyr, tidyr, glue, tictoc, progressr, ggalt)

tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box(season=c(2020:2025))
})
tictoc::toc()

#player games
dd <- subset(wbb_player_box, athlete_display_name=="Dalayah Daniels" & did_not_play == FALSE)

ap <- subset(wbb_player_box, athlete_display_name=="Alissa Pili" & did_not_play == FALSE)

#find overlapping games
games <- intersect(dd$game_id, ap$game_id)

# Filter subsets to only shared games
dd_shared <- dd[dd$game_id %in% games,]
ap_shared <- ap[ap$game_id %in% games,]

# Ensure game_id is character in both datasets
dd_shared$game_id <- as.character(dd_shared$game_id)
ap_shared$game_id <- as.character(ap_shared$game_id)

dd_shared <- as.data.frame(dd_shared)
ap_shared <- as.data.frame(ap_shared)


merged_stats <- merge(dd_shared, ap_shared, 
  by = "game_id",
  suffixes = c("_dd", "_ap"))


final_df <- merged_stats[,c("game_id", "game_date_dd",
  "points_dd", "rebounds_dd", 
  "points_ap", "rebounds_ap")]
final_df


ggplot(final_df, aes(y = as.factor(game_date_dd))) +
  geom_dumbbell(
    aes(x = points_dd, xend = points_ap),
    colour = "#b9e0fb",
    size = 2,
    colour_x = "#4b2e83",      # DD color
    colour_xend = "#BE0000"    # AP color
  ) +
  # Label for DD (left point)
  geom_text(
    aes(x = points_dd, label = "DD"),
    color = "#4b2e83", fontface = "bold", hjust = 1.2, size = 3
  ) +
  # Label for AP (right point)
  geom_text(
    aes(x = points_ap, label = "AP"),
    color = "#BE0000", fontface = "bold", hjust = -0.2, size = 3
  ) +
  labs(
    title = "Pili vs Daniels Head-to-Head Scoring",
    subtitle = "Points per game in matchups 2022-2024",
    x = "Points", y = "Game Date"
  ) +
  theme_minimal()
