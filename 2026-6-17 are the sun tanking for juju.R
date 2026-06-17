# =========================================
# WNBA team-season k-means clustering
# =========================================

# ============================
# Libraries
# ============================
library(wehoop)
library(progressr)
library(tictoc)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(cluster)
library(factoextra)
library(gt)
library(ggrepel)
library(grid)

# ============================
# 0. Load data
# ============================
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season = 2006:2026)
})
tictoc::toc()

# keep regular season only
wnba_team_box <- wnba_team_box %>%
  filter(season_type == 2)

wnba_team_box_hist <- wnba_team_box %>%
  filter(season <= 2025)

wnba_team_box_2026 <- wnba_team_box %>%
  filter(season == 2026)

# ============================
# 1. Team lookup
# ============================
team_lookup_season <- 
  wnba_team_box %>%
  arrange(season) %>%
  distinct(season, team_id, .keep_all = TRUE) %>%
  select(season, team_id, team_name, team_abbreviation)

# ============================
# 2. Helper: build team-season stats
# ============================
build_team_season_stats <- function(team_box_df) {
  
  team_game_raw <- 
    team_box_df %>%
    select(
      game_id, season, season_type, game_date,
      team_id, team_home_away, team_score, team_winner,
      field_goals_made, field_goals_attempted, field_goal_pct,
      three_point_field_goals_made, three_point_field_goals_attempted,
      three_point_field_goal_pct,
      free_throws_made, free_throws_attempted, free_throw_pct,
      offensive_rebounds, defensive_rebounds,
      assists, steals, blocks,
      team_turnovers, fouls
    ) %>%
    inner_join(
      .,
      .,
      by = c("game_id", "season", "season_type", "game_date"),
      suffix = c("_team", "_opp"),
      relationship = "many-to-many"
    ) %>%
    filter(team_id_team != team_id_opp)
  
  team_game <- 
    team_game_raw %>%
    transmute(
      game_id,
      season,
      season_type,
      game_date,
      team_id = team_id_team,
      opp_id = team_id_opp,
      team_home_away = team_home_away_team,
      team_score = team_score_team,
      opp_score = team_score_opp,
      team_winner = team_winner_team,
      # team stats
      team_fgm = field_goals_made_team,
      team_fga = field_goals_attempted_team,
      team_fg_pct = field_goal_pct_team,
      team_3pm = three_point_field_goals_made_team,
      team_3pa = three_point_field_goals_attempted_team,
      team_3p_pct = three_point_field_goal_pct_team,
      team_ftm = free_throws_made_team,
      team_fta = free_throws_attempted_team,
      team_ft_pct = free_throw_pct_team,
      team_orb = offensive_rebounds_team,
      team_drb = defensive_rebounds_team,
      team_ast = assists_team,
      team_stl = steals_team,
      team_blk = blocks_team,
      team_tov = team_turnovers_team,
      team_fouls = fouls_team,
      # opp stats
      opp_fgm = field_goals_made_opp,
      opp_fga = field_goals_attempted_opp,
      opp_fg_pct = field_goal_pct_opp,
      opp_3pm = three_point_field_goals_made_opp,
      opp_3pa = three_point_field_goals_attempted_opp,
      opp_3p_pct = three_point_field_goal_pct_opp,
      opp_ftm = free_throws_made_opp,
      opp_fta = free_throws_attempted_opp,
      opp_ft_pct = free_throw_pct_opp,
      opp_orb = offensive_rebounds_opp,
      opp_drb = defensive_rebounds_opp,
      opp_ast = assists_opp,
      opp_stl = steals_opp,
      opp_blk = blocks_opp,
      opp_tov = team_turnovers_opp,
      opp_fouls = fouls_opp
    )
  
  team_game_metrics <- 
    team_game %>%
    mutate(
      team_possessions = team_fga + 0.44 * team_fta - team_orb + team_tov,
      opp_possessions  = opp_fga  + 0.44 * opp_fta  - opp_orb  + opp_tov,
      possessions = (team_possessions + opp_possessions) / 2,
      offensive_rating = 100 * team_score / team_possessions,
      defensive_rating = 100 * opp_score / opp_possessions,
      net_rating = offensive_rating - defensive_rating,
      efg_pct = (team_fgm + 0.5 * team_3pm) / team_fga,
      three_point_rate = team_3pa / team_fga,
      ft_rate = team_fta / team_fga,
      orb_pct = team_orb / (team_orb + opp_drb),
      drb_pct = team_drb / (team_drb + opp_orb),
      tov_pct = team_tov / team_possessions,
      fouls_per_100 = 100 * team_fouls / team_possessions
    )
  
  team_season_stats <- 
    team_game_metrics %>%
    group_by(season, team_id) %>%
    summarise(
      games_played = n(),
      wins = sum(team_winner, na.rm = TRUE),
      losses = games_played - wins,
      win_pct = wins / games_played,
      off_rtg = mean(offensive_rating, na.rm = TRUE),
      def_rtg = mean(defensive_rating, na.rm = TRUE),
      net_rtg = mean(net_rating, na.rm = TRUE),
      efg_pct = mean(efg_pct, na.rm = TRUE),
      three_pt_rate = mean(three_point_rate, na.rm = TRUE),
      ft_rate = mean(ft_rate, na.rm = TRUE),
      orb_pct = mean(orb_pct, na.rm = TRUE),
      drb_pct = mean(drb_pct, na.rm = TRUE),
      tov_pct = mean(tov_pct, na.rm = TRUE),
      pace = mean(possessions, na.rm = TRUE),
      pts_per_game = mean(team_score, na.rm = TRUE),
      opp_pts_per_game = mean(opp_score, na.rm = TRUE),
      steals_per_game = mean(team_stl, na.rm = TRUE),
      blocks_per_game = mean(team_blk, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(games_played > 4)
  
  team_season_stats
}

# ============================
# 3. Build historical team-season stats
# ============================
team_season_stats_hist <- build_team_season_stats(wnba_team_box_hist)

feature_cols <- c(
  "off_rtg", "def_rtg", "net_rtg",
  "efg_pct", "three_pt_rate", "ft_rate",
  "orb_pct", "drb_pct",
  "tov_pct", "pace"
)

team_season_features_hist <- 
  team_season_stats_hist %>%
  select(
    season, team_id,
    games_played, wins, losses, win_pct,
    all_of(feature_cols)
  )

X <- team_season_features_hist %>%
  select(all_of(feature_cols)) %>%
  scale()

# ============================
# 4. Fit k-means
# ============================
set.seed(123)
k_opt <- 6
km_final <- kmeans(X, centers = k_opt, nstart = 50)

team_season_clustered_hist <- 
  team_season_features_hist %>%
  mutate(cluster = km_final$cluster)

# ============================
# 5. Relabel clusters by average win%
#    1 = best, 6 = worst
# ============================
cluster_summary_orig <- 
  team_season_clustered_hist %>%
  group_by(cluster) %>%
  summarise(
    mean_win_pct = mean(win_pct, na.rm = TRUE),
    .groups = "drop"
  )

cluster_relabel <- 
  cluster_summary_orig %>%
  arrange(desc(mean_win_pct)) %>%   # best to worst
  mutate(new_cluster = row_number()) %>%
  select(old_cluster = cluster, new_cluster)

team_season_named_hist <- 
  team_season_clustered_hist %>%
  left_join(team_lookup_season, by = c("season", "team_id")) %>%
  mutate(final_record = paste0(wins, "-", losses)) %>%
  left_join(cluster_relabel, by = c("cluster" = "old_cluster")) %>%
  mutate(cluster = new_cluster) %>%
  select(-new_cluster)

print(team_season_named_hist,n=30)

# ============================
# 6. Historical cluster summary
# ============================
cluster_summary_hist <- 
  team_season_named_hist %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    mean_win_pct = mean(win_pct, na.rm = TRUE),
    sd_win_pct   = sd(win_pct, na.rm = TRUE),
    mean_net_rtg = mean(net_rtg, na.rm = TRUE),
    sd_net_rtg   = sd(net_rtg, na.rm = TRUE),
    mean_off_rtg = mean(off_rtg, na.rm = TRUE),
    mean_def_rtg = mean(def_rtg, na.rm = TRUE),
    mean_efg     = mean(efg_pct, na.rm = TRUE),
    mean_orb_pct = mean(orb_pct, na.rm = TRUE),
    mean_tov_pct = mean(tov_pct, na.rm = TRUE),
    mean_pace    = mean(pace, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(cluster)

cluster_summary_hist_gt <- 
  cluster_summary_hist %>%
  gt() %>%
  fmt_number(columns = where(is.numeric), decimals = 2) %>%
  tab_header(title = "Cluster-level archetypes (WNBA team seasons, 2006–2025)")

cluster_summary_hist
cluster_summary_hist_gt

# ============================
# 7. PCA on historical data
# ============================
pca_res <- prcomp(
  team_season_named_hist %>% select(all_of(feature_cols)),
  center = TRUE, scale. = TRUE
)

team_season_pca_hist <- 
  team_season_named_hist %>%
  mutate(
    PC1 = pca_res$x[, 1],
    PC2 = pca_res$x[, 2]
  )

# ============================
# 8. Build 2026 team-season stats
# ============================
team_season_stats_2026 <- build_team_season_stats(wnba_team_box_2026)

team_season_features_2026 <- 
  team_season_stats_2026 %>%
  select(
    season, team_id,
    games_played, wins, losses, win_pct,
    all_of(feature_cols)
  )

# ============================
# 9. Assign 2026 teams to historical clusters
# ============================
center_vals <- attr(X, "scaled:center")
scale_vals  <- attr(X, "scaled:scale")

X_2026 <- 
  team_season_features_2026 %>%
  select(all_of(feature_cols)) %>%
  as.matrix()

X_2026_scaled <- sweep(X_2026, 2, center_vals, FUN = "-")
X_2026_scaled <- sweep(X_2026_scaled, 2, scale_vals, FUN = "/")

assign_cluster <- function(row, centers) {
  dists <- apply(centers, 1, function(cntr) sum((row - cntr)^2))
  which.min(dists)
}

clusters_2026 <- apply(X_2026_scaled, 1, assign_cluster, centers = km_final$centers)

team_season_clustered_2026 <- 
  team_season_features_2026 %>%
  mutate(cluster = clusters_2026)

team_season_named_2026 <- 
  team_season_clustered_2026 %>%
  left_join(team_lookup_season, by = c("season", "team_id")) %>%
  mutate(final_record = paste0(wins, "-", losses)) %>%
  left_join(cluster_relabel, by = c("cluster" = "old_cluster")) %>%
  mutate(cluster = new_cluster) %>%
  select(-new_cluster)

# ============================
# 10. Find 2026 Connecticut Sun
# ============================
sun_2026 <- 
  team_season_named_2026 %>%
  filter(team_name == "Sun")

sun_2026
sun_2026_cluster_id <- sun_2026$cluster[1]

sun_cluster_summary <- 
  cluster_summary_hist %>%
  filter(cluster == sun_2026_cluster_id)

cluster_summary_hist%>% select(cluster,n,mean_win_pct, mean_off_rtg, mean_def_rtg, mean_efg)

sun_cluster_summary

# ============================
# 11. PCA coordinates for 2026
# ============================
pca_2026_scores <- predict(
  pca_res,
  newdata = team_season_named_2026 %>% select(all_of(feature_cols))
)

team_season_pca_2026 <- 
  team_season_named_2026 %>%
  mutate(
    PC1 = pca_2026_scores[, 1],
    PC2 = pca_2026_scores[, 2]
  )

# ============================
# 12. Plot data + labels
# ============================
plot_df <- bind_rows(
  team_season_pca_hist %>% mutate(period = "Historical"),
  team_season_pca_2026 %>% mutate(period = "2026")
)

plot_df <- plot_df %>%
  mutate(label_name = paste0(season, " ", team_name))

high_thresh <- 0.80
low_thresh  <- 0.20

label_df <- bind_rows(
  team_season_pca_hist %>%
    filter(win_pct >= high_thresh) %>%
    mutate(label_name = paste0(season, " ", team_name, " ", round(win_pct, 2))),
  
  team_season_pca_hist %>%
    filter(win_pct <= low_thresh) %>%
    mutate(label_name = paste0(season, " ", team_name, " ", round(win_pct, 2))),
  
  team_season_pca_2026 %>%
    filter(team_name=="Sun") %>%
    mutate(label_name = paste0(season, " ", team_name, " ", round(win_pct, 2)))
) %>%
  distinct(season, team_id, .keep_all = TRUE)


sun_label_df <- label_df %>%
  filter(season == 2026, grepl("Sun", team_name))

other_label_df <- label_df %>%
  filter(!(season == 2026 & grepl("Sun", team_name)))
# ============================
# 13. PCA scatterplot
# ============================
ggplot(plot_df, aes(x = PC1, y = PC2, color = factor(cluster))) +
  geom_point(alpha = 0.55, size = 2.5) +
  geom_point(
    data = label_df,
    size = 3.5,
    show.legend = FALSE
  ) +
  geom_point(
    data = plot_df %>% filter(season == 2026, team_name == "Connecticut Sun"),
    size = 5,
    color = "black",
    fill = "gold",
    shape = 21,
    stroke = 1.2,
    show.legend = FALSE
  ) +
  # other teams' labels
  ggrepel::geom_label_repel(
    data = other_label_df,
    aes(label = label_name),
    color = "black",
    fill = scales::alpha("white", 0.9),
    label.size = 0.25,
    label.r = unit(0.15, "lines"),
    size = 3.8,
    box.padding = 0.45,
    point.padding = 0.3,
    segment.color = "grey35",
    segment.size = 0.5,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  # Connecticut Sun label: bolder box + bold text
  ggrepel::geom_label_repel(
    data = sun_label_df,
    aes(label = label_name),
    color = "black",
    fill = "gold",
    label.size = 0.7,                 # thicker border
    label.r = unit(0.15, "lines"),
    size = 4.0,
    fontface = "bold",
    box.padding = 0.6,
    point.padding = 0.4,
    segment.color = "black",
    segment.size = 0.7,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  labs(
    title = "How WNBA team seasons group by four factors",
    subtitle = "WNBA Regular Season 2006–2026",
    caption = "data: wehoop | chart: @wnbadata",
    x = "PC1",
    y = "PC2",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )


# ensure both historical and 2026 frames have team_name
hist_with_names <- 
  team_season_named_hist %>%   # already has team_name in the cleaned script
  select(
    season, team_id, team_name, final_record, cluster,
    win_pct, off_rtg, def_rtg, net_rtg,
    efg_pct, orb_pct, tov_pct, pace
  )

curr_with_names <- 
  team_season_named_2026 %>%   # already has team_name in the cleaned script
  select(
    season, team_id, team_name, final_record, cluster,
    win_pct, off_rtg, def_rtg, net_rtg,
    efg_pct, orb_pct, tov_pct, pace
  )

# combine and filter cluster 6
cluster6_teams <- bind_rows(
  hist_with_names %>% mutate(period = "2006-2025"),
  curr_with_names %>% mutate(period = "2026")
) %>%
  filter(cluster == 6 | (season == 2026 & team_name == "Connecticut Sun")) %>%
  arrange(season, team_name)

cluster6_teams %>% arrange(desc(season)) %>% select(season, team_name, win_pct, off_rtg, def_rtg, efg_pct)




wnba_2026_by_cluster <- 
  team_season_named_2026 %>%
  arrange(cluster, desc(win_pct)) %>%
  select(
    cluster,
    season,
    team_name,
    final_record,
    win_pct,
    off_rtg,
    def_rtg,
    efg_pct
  )

wnba_2026_by_cluster