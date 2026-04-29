library(wehoop)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(gt)

#load data and filter out all star games
team_box_2025 <- load_wnba_team_box(seasons = 2025)
team_box_2025 <- team_box_2025 %>% filter(team_name !="TEAM CLARK" & team_name !="TEAM COLLIER")

# Function to calculate Four Factors for a specific team
calculate_team_factors <- function(data, team_name_val) {
  team_games <- data %>%
    filter(team_name == team_name_val | opponent_team_name == team_name_val)
  
  team_stats <- team_games %>% filter(team_name == team_name_val)
  opp_stats <- team_games %>% filter(team_name != team_name_val)
  
  tibble(
    team_name = team_name_val,
    off_efg_pct = sum((team_stats$field_goals_made + 0.5 * team_stats$three_point_field_goals_made), na.rm=TRUE) / 
                  sum(team_stats$field_goals_attempted, na.rm=TRUE),
    off_tov_pct = sum(team_stats$team_turnovers, na.rm=TRUE) / 
                  sum(team_stats$field_goals_attempted + 0.44 * team_stats$free_throws_attempted + team_stats$team_turnovers, na.rm=TRUE),
    off_orb_pct = sum(team_stats$offensive_rebounds, na.rm=TRUE) / 
                  sum(team_stats$offensive_rebounds + opp_stats$defensive_rebounds, na.rm=TRUE),
    off_ft_rate = sum(team_stats$free_throws_made, na.rm=TRUE) / 
                  sum(team_stats$field_goals_attempted, na.rm=TRUE),
    def_efg_pct = sum((opp_stats$field_goals_made + 0.5 * opp_stats$three_point_field_goals_made), na.rm=TRUE) / 
                  sum(opp_stats$field_goals_attempted, na.rm=TRUE),
    def_tov_pct = sum(opp_stats$team_turnovers, na.rm=TRUE) / 
                  sum(opp_stats$field_goals_attempted + 0.44 * opp_stats$free_throws_attempted + opp_stats$team_turnovers, na.rm=TRUE),
    def_drb_pct = sum(team_stats$defensive_rebounds, na.rm=TRUE) / 
                  sum(opp_stats$offensive_rebounds + team_stats$defensive_rebounds, na.rm=TRUE),
    def_ft_rate = sum(opp_stats$free_throws_made, na.rm=TRUE) / 
                  sum(opp_stats$field_goals_attempted, na.rm=TRUE)
  )
}

# Get unique team names
teams <- unique(team_box_2025$team_name)

# Before Aug 2, 2025
before_data <- team_box_2025 %>%
  filter(game_date <= as.Date("2025-08-02"), season_type == 2) #regular season only

all_factors_before <- bind_rows(lapply(teams, function(tm) {
  calculate_team_factors(before_data, tm)
})) %>%
  mutate(
    off_efg_rank = rank(desc(off_efg_pct)),
    off_tov_rank = rank(off_tov_pct),  # Lower is better
    off_orb_rank = rank(desc(off_orb_pct)),
    off_ft_rank = rank(desc(off_ft_rate)),
    def_efg_rank = rank(def_efg_pct),  # Lower is better
    def_tov_rank = rank(desc(def_tov_pct)),  # Higher is better (forcing TOs)
    def_drb_rank = rank(desc(def_drb_pct)),
    def_ft_rank = rank(def_ft_rate)  # Lower is better
  )

# After Aug 2, 2025
after_data <- team_box_2025 %>%
  filter(game_date > as.Date("2025-08-02"), season_type == 2)

all_factors_after <- bind_rows(lapply(teams, function(tm) {
  calculate_team_factors(after_data, tm)
})) %>%
  mutate(
    off_efg_rank = rank(desc(off_efg_pct)),
    off_tov_rank = rank(off_tov_pct),
    off_orb_rank = rank(desc(off_orb_pct)),
    off_ft_rank = rank(desc(off_ft_rate)),
    def_efg_rank = rank(def_efg_pct),
    def_tov_rank = rank(desc(def_tov_pct)),
    def_drb_rank = rank(desc(def_drb_pct)),
    def_ft_rank = rank(def_ft_rate)
  )

# Get Aces comparison
aces_comparison <- bind_rows(
  all_factors_before %>% filter(team_name == "Aces") %>% mutate(period = "Before/Including Aug 2"),
  all_factors_after %>% filter(team_name == "Aces") %>% mutate(period = "After Aug 2")
) %>%
  select(period, everything())

print(aces_comparison)

# Create ranking comparison table
ranking_changes <- aces_comparison %>%
  select(period, ends_with("_rank")) %>%
  pivot_longer(cols = ends_with("_rank"), names_to = "factor", values_to = "rank") %>%
  pivot_wider(names_from = period, values_from = rank) %>%
  mutate(
    change = `After Aug 2` - `Before/Including Aug 2`,
    improvement = ifelse(change < 0, "Improved", ifelse(change > 0, "Declined", "Same"))
  )

print(ranking_changes)







###########################

# Prepare data for slope chart
slope_data <- ranking_changes %>%
  select(factor, `Before/Including Aug 2`, `After Aug 2`) %>%
  pivot_longer(cols = c(`Before/Including Aug 2`, `After Aug 2`), 
               names_to = "period", 
               values_to = "rank") %>%
  mutate(
    factor_label = case_when(
      factor == "off_efg_rank" ~ "Off eFG%",
      factor == "off_tov_rank" ~ "Off TOV%",
      factor == "off_orb_rank" ~ "Off ORB%",
      factor == "off_ft_rank" ~ "Off FT Rate",
      factor == "def_efg_rank" ~ "Def eFG%",
      factor == "def_tov_rank" ~ "Def TOV%",
      factor == "def_drb_rank" ~ "Def DRB%",
      factor == "def_ft_rank" ~ "Def FT Rate"
    ),
    period = factor(period, levels = c("Before/Including Aug 2", "After Aug 2"))
  )

# Create slope chart
ggplot(slope_data, aes(x = period, y = rank, group = factor_label)) +
  geom_line(aes(color = factor_label), size = 1.2) +
  geom_point(size = 3) +
  scale_y_reverse(breaks = 1:12) +  # Reverse so rank 1 is at top
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                                 "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")) +
  labs(
    title = "Las Vegas Aces Four Factors Rankings: Before vs After Aug 2, 2025",
    subtitle = "Lower rank = Better performance",
    x = NULL,
    y = "League Rank",
    color = "Factor"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )






improvement_arrows <- ranking_changes %>%
  transmute(
    factor,
    factor_clean = case_when(
      factor == "off_efg_rank" ~ "Off eFG%",
      factor == "off_tov_rank" ~ "Off TOV%",
      factor == "off_orb_rank" ~ "Off ORB%",
      factor == "off_ft_rank"  ~ "Off FT Rate",
      factor == "def_efg_rank" ~ "Def eFG%",
      factor == "def_tov_rank" ~ "Def TOV%",
      factor == "def_drb_rank" ~ "Def DRB%",
      factor == "def_ft_rank"  ~ "Def FT Rate"
    ),
    before_rank = `Before/Including Aug 2`,
    after_rank  = `After Aug 2`
  )

ggplot(improvement_arrows, aes(y = reorder(factor_clean, after_rank-before_rank))) +
  geom_point(aes(x = before_rank), color = "#000000", size = 1) +
  geom_point(aes(x = after_rank), color = "#000000", size = 1) +
  geom_segment(
    aes(x = before_rank, xend = after_rank,
        yend = reorder(factor_clean, before_rank),
        color = after_rank < before_rank),
    arrow = arrow(length = unit(0.2, "cm")),
    linewidth = 1.5
  ) +
  scale_x_reverse(breaks = 1:12) +
  scale_color_manual(values = c(`TRUE` = "#2b44eb", `FALSE` = "#e66464"), guide = "none") +
  labs(
    title = "Aces 2025 Four Factors League Rank Change",
    subtitle = "Before Aug. 2 (28 games) vs. after Aug. 2 (16 games)",
    x = "League rank (1 = best)",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )





ranking_changes %>%
  mutate(
    factor_label = case_when(
      factor == "off_efg_rank" ~ "Offensive eFG%",
      factor == "off_tov_rank" ~ "Offensive TOV%",
      factor == "off_orb_rank" ~ "Offensive ORB%",
      factor == "off_ft_rank"  ~ "Offensive FT Rate",
      factor == "def_efg_rank" ~ "Defensive eFG%",
      factor == "def_tov_rank" ~ "Defensive TOV%",
      factor == "def_drb_rank" ~ "Defensive DRB%",
      factor == "def_ft_rank"  ~ "Defensive FT Rate"
    )
  ) %>%
  select(factor_label, `Before/Including Aug 2`, `After Aug 2`, change) %>%
  gt() %>%
  tab_header(
    title = "Las Vegas Aces Four Factors Rankings",
    subtitle = "League rank before and after August 2, 2025"
  ) %>%
  cols_label(
    factor_label = "Factor",
    `Before/Including Aug 2` = "Before Aug 2",
    `After Aug 2` = "After Aug 2",
    change = "Change"
  ) %>%
  data_color(
    columns = change,
    colors = scales::col_numeric(
      palette = c("#b22222", "white", "#1b7f3b"),
      domain = c(-7, 7),
      reverse = TRUE
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = change,
      rows = change < 0
    )
  ) %>%
  tab_footnote(
    footnote = "Negative change = improvement in ranking (lower rank number is better)",
    locations = cells_column_labels(columns = change)
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(18),
    heading.subtitle.font.size = px(14)
  )




aces_comparison %>%
  filter(period == "Before/Including Aug 2") %>%
  select(off_efg_pct:def_ft_rank) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("side", "stat", "type"),
    names_pattern = "(.+)_(.+)_(pct|rank|rate)",
    values_to = "value"
  ) %>%
  mutate(
    factor = paste0(
      ifelse(side == "off", "Offensive ", "Defensive "),
      case_when(
        stat == "efg" ~ "eFG%",
        stat == "tov" ~ "TOV%",
        stat == "orb" ~ "ORB%",
        stat == "drb" ~ "DRB%",
        stat == "ft" ~ "FT Rate"
      )
    ),
    metric = ifelse(type == "rank", "League Rank", "Value")
  ) %>%
  select(factor, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  gt() %>%
  tab_header(
    title = "Las Vegas Aces: Early Season Four Factors",
    subtitle = "Performance through August 2, 2025 (14-14 record)"
  ) %>%
  cols_label(
    factor = "Factor",
    Value = "Stat",
    `League Rank` = "Rank"
  ) %>%
  fmt_percent(columns = Value, decimals = 1) %>%
  data_color(
    columns = `League Rank`,
    colors = scales::col_numeric(
      palette = c("#1b7f3b", "#f0f0f0", "#b22222"),
      domain = c(1, 12)
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = `League Rank`)
  ) %>%
  tab_footnote(
    footnote = "Lower rank = better performance (1st is best, 13th is worst)",
    locations = cells_column_labels(columns = `League Rank`)
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(18)
  )




aces_comparison %>%
  filter(period == "After Aug 2") %>%
  select(off_efg_pct:def_ft_rank) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("side", "stat", "type"),
    names_pattern = "(.+)_(.+)_(pct|rank|rate)",
    values_to = "value"
  ) %>%
  mutate(
    factor = paste0(
      ifelse(side == "off", "Offensive ", "Defensive "),
      case_when(
        stat == "efg" ~ "eFG%",
        stat == "tov" ~ "TOV%",
        stat == "orb" ~ "ORB%",
        stat == "drb" ~ "DRB%",
        stat == "ft" ~ "FT Rate"
      )
    ),
    metric = ifelse(type == "rank", "League Rank", "Value")
  ) %>%
  select(factor, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  gt() %>%
  tab_header(
    title = "Las Vegas Aces: Late Season Four Factors",
    subtitle = "Performance After August 2, 2025 (16-0 record)"
  ) %>%
  cols_label(
    factor = "Factor",
    Value = "Stat",
    `League Rank` = "Rank"
  ) %>%
  fmt_percent(columns = Value, decimals = 1) %>%
  data_color(
    columns = `League Rank`,
    colors = scales::col_numeric(
      palette = c("#1b7f3b", "#f0f0f0", "#b22222"),
      domain = c(1, 12)
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = `League Rank`)
  ) %>%
  tab_footnote(
    footnote = "Lower rank = better performance (1st is best, 13th is worst)",
    locations = cells_column_labels(columns = `League Rank`)
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(18)
  )



aces_comparison %>%
  select(period, off_efg_pct, off_efg_rank, def_efg_pct, def_efg_rank) %>%
  gt() %>%
  tab_header(
    title = "Aces Shooting Efficiency",
    subtitle = "Effective Field Goal % before and after August 2, 2025"
  ) %>%
  cols_label(
    period = "Period",
    off_efg_pct = "Offensive eFG%",
    off_efg_rank = "Rank",
    def_efg_pct = "Defensive eFG%",
    def_efg_rank = "Rank"
  ) %>%
  tab_spanner(
    label = "Offense",
    columns = c(off_efg_pct, off_efg_rank)
  ) %>%
  tab_spanner(
    label = "Defense",
    columns = c(def_efg_pct, def_efg_rank)
  ) %>%
  fmt_percent(
    columns = c(off_efg_pct, def_efg_pct),
    decimals = 1
  ) %>%
  data_color(
    columns = c(off_efg_rank, def_efg_rank),
    colors = scales::col_numeric(
      palette = c("#1b7f3b", "#f0f0f0", "#b22222"),
      domain = c(1, 12)
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", size = px(16)),
    locations = cells_body(columns = ends_with("_rank"))
  ) %>%
  tab_footnote(
    footnote = "8th to 1st: +7.7 percentage points",
    locations = cells_body(columns = off_efg_rank, rows = period == "After Aug 2")
  ) %>%
  tab_footnote(
    footnote = "9th to 3rd: opponents shot 3.2pp worse",
    locations = cells_body(columns = def_efg_rank, rows = period == "After Aug 2")
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(18),
    heading.subtitle.font.size = px(14),
    column_labels.font.weight = "bold"
  )