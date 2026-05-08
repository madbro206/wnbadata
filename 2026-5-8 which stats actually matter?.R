library(wehoop)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(gt)
library(stringr)
library(purrr)
library(scales)

#load data and filter out all star games
wnba_team_box <- load_wnba_team_box(seasons = c(2016:2025))
wnba_team_box <- wnba_team_box %>% filter(team_name !="TEAM CLARK" & team_name !="TEAM COLLIER" & season_type==2)


leader_win_pct <- function(data, stats, ties = "include") {

  long_df <- data %>%
    select(game_id, team_winner, all_of(stats)) %>%
    pivot_longer(
      cols = all_of(stats),
      names_to = "stat",
      values_to = "value"
    ) %>%
    group_by(game_id, stat) %>%
    mutate(
      leader = case_when(
        ties == "include" ~ value == max(value, na.rm = TRUE),
        ties == "drop"    ~ value == max(value, na.rm = TRUE) & !duplicated(max(value)) &
          sum(value == max(value, na.rm = TRUE), na.rm = TRUE) == 1
      )
    ) %>%
    ungroup()

  if (ties == "drop") {
    long_df <- long_df %>%
      group_by(game_id, stat) %>%
      filter(sum(leader, na.rm = TRUE) == 1) %>%
      ungroup()
  }

  long_df %>%
    filter(leader) %>%
    group_by(stat) %>%
    summarise(
      games = n(),
      wins = sum(team_winner, na.rm = TRUE),
      win_pct = wins / games,
      .groups = "drop"
    ) %>%
    arrange(desc(win_pct))
}

wnba_team_box_four_factors <- wnba_team_box %>%
  # get opponent defensive rebounds for ORB%
  group_by(game_id) %>%
  mutate(
    opp_def_reb = if_else(
      team_id == first(team_id),
      last(defensive_rebounds),
      first(defensive_rebounds)
    )
  ) %>%
  ungroup() %>%
  mutate(
    # Effective field goal percentage
    off_eFG_pct = (field_goals_made + 0.5 * three_point_field_goals_made) /
                  field_goals_attempted,

    # Turnover percentage (use turnovers or total_turnovers depending on your preference)
    off_TOV_pct = turnovers /
      (field_goals_attempted + 0.44 * free_throws_attempted + turnovers),

    # Offensive rebound percentage
    off_ORB_pct = offensive_rebounds /
      (offensive_rebounds + opp_def_reb),

    # Free throw rate
    off_FTr = free_throws_attempted / field_goals_attempted,

    #twos made
    two_point_field_goals_made= field_goals_made-three_point_field_goals_made
  )

stats_to_check <- c(
  "assists",
  "blocks",
  "steals",
  "total_rebounds",
  "field_goal_pct",
  "team_score",
  "three_point_field_goals_made",
  "off_eFG_pct",
  "off_TOV_pct",
  "off_ORB_pct",
  "off_FTr",
  "team_turnovers",
  "defensive_rebounds",
  "offensive_rebounds",
  "fouls"
)


leader_win_pct(wnba_team_box_four_factors, stats_to_check)


box_stats <- c(
  "assists",
  "blocks",
  "steals",
  "off_eFG_pct",
  "total_rebounds",
  "field_goal_pct",
  "team_score",
  "three_point_field_goals_made",
  "three_point_field_goals_made",
  "two_point_field_goals_made",
  "team_turnovers",
  "defensive_rebounds",
  "offensive_rebounds",
  "fouls"
)

leader_win_pct(wnba_team_box_four_factors, box_stats)







games_better_reb_stl_to_but_lost <- wnba_team_box %>%
  group_by(game_id) %>%
  mutate(
    # opponent values in the other row of the same game
    opp_total_rebounds = if_else(
      row_number() == 1,
      dplyr::lead(total_rebounds),
      dplyr::lag(total_rebounds)
    ),
    opp_steals = if_else(
      row_number() == 1,
      dplyr::lead(steals),
      dplyr::lag(steals)
    ),
    opp_turnovers = if_else(
      row_number() == 1,
      dplyr::lead(turnovers),
      dplyr::lag(turnovers)
    ),
    opp_blocks = if_else(
      row_number() == 1,
      dplyr::lead(blocks),
      dplyr::lag(blocks)
    ),

    out_rebounded   = total_rebounds > opp_total_rebounds,
    out_stealed     = steals > opp_steals,
    out_blocked   = blocks > opp_blocks,
    out_turnovered  = turnovers < opp_turnovers  # fewer turnovers
  ) %>%
  ungroup() %>%
  filter(
    out_rebounded,
    out_stealed,
    out_turnovered,
    out_blocked,
    !team_winner
  )

games_better_reb_stl_to_but_lost %>%
  filter(season==2025)%>%
  select(game_date, season, team_name, team_score, opponent_team_name,
          opponent_team_score, total_rebounds, opp_total_rebounds,
          steals, opp_steals, turnovers, opp_turnovers)






efg_upset_games <- wnba_team_box_four_factors %>%
  group_by(game_id) %>%
  mutate(
    # identify game-high and game-low eFG%
    max_efg = max(off_eFG_pct, na.rm = TRUE),
    min_efg = min(off_eFG_pct, na.rm = TRUE),
    is_efg_leader = off_eFG_pct == max_efg,
    is_efg_trailer = off_eFG_pct == min_efg
  ) %>%
  ungroup()

# sanity: overall win% when leading eFG% (you already did this)
efg_leader_winrate <- efg_upset_games %>%
  filter(is_efg_leader) %>%
  summarise(
    games = n(),
    wins  = sum(team_winner, na.rm = TRUE),
    win_pct = wins / games
  )

# focus subset: team had *lower* eFG% but *won* the game
efg_trailer_wins <- efg_upset_games %>%
  filter(is_efg_trailer, team_winner)

# add per-game margin (leader eFG – trailer eFG)
efg_margins <- efg_upset_games %>%
  group_by(game_id) %>%
  summarise(
    efg_leader = max(off_eFG_pct, na.rm = TRUE),
    efg_trailer = min(off_eFG_pct, na.rm = TRUE),
    efg_diff = efg_leader - efg_trailer
  )

# eFG% margin only for games where the trailer actually won
trailer_win_margins <- efg_trailer_wins %>%
  select(game_id) %>%
  left_join(efg_margins, by = "game_id")

# basic summary of those differences
trailer_win_margins %>%
  summarise(
    n_games = n(),
    mean_diff = mean(efg_diff, na.rm = TRUE),
    median_diff = median(efg_diff, na.rm = TRUE),
    p90_diff = quantile(efg_diff, 0.9, na.rm = TRUE),
    max_diff = max(efg_diff, na.rm = TRUE)
  )



  efg_margins <- efg_upset_games %>%
    group_by(game_id) %>%
    summarise(
      efg_leader  = max(off_eFG_pct, na.rm = TRUE),
      efg_trailer = min(off_eFG_pct, na.rm = TRUE),
      efg_diff    = efg_leader - efg_trailer,
      .groups = "drop"
    )

all_games_efg_gap <- efg_margins %>%
  summarise(
    n_games     = n(),
    mean_diff   = mean(efg_diff, na.rm = TRUE),
    median_diff = median(efg_diff, na.rm = TRUE),
    p90_diff    = quantile(efg_diff, 0.9, na.rm = TRUE),
    max_diff    = max(efg_diff, na.rm = TRUE)
  )
all_games_efg_gap

upset_efg_gap <- trailer_win_margins %>%
  summarise(
    n_games     = n(),
    mean_diff   = mean(efg_diff, na.rm = TRUE),
    median_diff = median(efg_diff, na.rm = TRUE),
    p90_diff    = quantile(efg_diff, 0.9, na.rm = TRUE),
    max_diff    = max(efg_diff, na.rm = TRUE)
  ) %>%
  mutate(sample = "eFG% trailer wins")

overall_efg_gap <- efg_margins %>%
  summarise(
    n_games     = n(),
    mean_diff   = mean(efg_diff, na.rm = TRUE),
    median_diff = median(efg_diff, na.rm = TRUE),
    p90_diff    = quantile(efg_diff, 0.9, na.rm = TRUE),
    max_diff    = max(efg_diff, na.rm = TRUE)
  ) %>%
  mutate(sample = "all games")

comparison <- bind_rows(overall_efg_gap, upset_efg_gap) %>%
  select(sample, everything())

comparison



win_stats <- leader_win_pct(wnba_team_box_four_factors, box_stats)
win_stats <- leader_win_pct(wnba_team_box_four_factors, box_stats) %>%
  mutate(
    stat_label = stat %>%
      str_replace_all("_", " ") %>%    # underscores → spaces
      str_to_title()                   # title case, e.g. "Off Efg Pct"
  )

win_stats <- win_stats %>%
  mutate(
    stat_label = case_when(
      stat == "off_eFG_pct" ~ "eFG%",
      stat == "team_turnovers" ~ "Turnovers",
      TRUE ~ stat_label
    )
  )


win_stats %>%
  ggplot(aes(x = reorder(stat_label, desc(win_pct)), y = win_pct)) +
  geom_col(fill = "darkorange") +
  geom_text(
    aes(label = percent(win_pct, accuracy = 1)),
    vjust = -0.3,
    size = 3
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.05)
  ) +
  labs(
    x = NULL,
    y = "Win%",
    title = "Team Win% when leading various box score stats",
    subtitle="WNBA 2016-2025"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



win_stats %>% filter(stat_label=="Team Score" | stat_label=="eFG%") %>%
  ggplot(aes(x = reorder(stat_label, desc(win_pct)), y = win_pct)) +
  geom_col(fill = "darkorange") +
  geom_text(
    aes(label = percent(win_pct, accuracy = 1)),
    vjust = -0.3,
    size = 6
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.05)
  ) +
  labs(
    x = NULL,
    y = "Win%",
    title = "Team Win% when leading game in [stat]",
    subtitle="WNBA 2016-2025",
    caption = "data: wehoop | chart: @wnbadata"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=15)
  )



win_stats %>% filter(stat_label=="Team Score" | stat_label=="eFG%" |stat_label=="Field Goal Pct" | stat_label=="Defensive Rebounds" | stat_label=="Assists"  ) %>%
  ggplot(aes(x = reorder(stat_label, desc(win_pct)), y = win_pct)) +
  geom_col(fill = "darkorange") +
  geom_text(
    aes(label = percent(win_pct, accuracy = 1)),
    vjust = -0.3,
    size = 6
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.05)
  ) +
  labs(
    x = NULL,
    y = "Win%",
    title = "Team Win% when leading game in [stat]",
    subtitle="WNBA 2016-2025",
    caption = "data: wehoop | chart: @wnbadata"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=15)
  )

win_stats %>%
  ggplot(aes(x = reorder(stat_label, desc(win_pct)), y = win_pct)) +
  geom_col(fill = "darkorange") +
  geom_text(
    aes(label = percent(win_pct, accuracy = 1)),
    vjust = -0.3,
    size = 6
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.05)
  ) +
  labs(
    x = NULL,
    y = "Win%",
    title = "Team Win% when leading game in [stat]",
    subtitle="WNBA 2016-2025",
    caption = "data: wehoop | chart: @wnbadata"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=15)
  )