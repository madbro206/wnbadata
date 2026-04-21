library(readr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(gt)
library(scales)

#2025 wnba player basic and advanced stats from
#https://www.basketball-reference.com/wnba/years/2025_advanced.html
data <- read.csv("~/Desktop/2025_wnba.csv")


#efg vs fg plot
plot_data <- data %>%
  mutate(
    gap = eFG. - FG.,
    group = case_when(
      Player == "Kelsey Mitchell" ~ "Kelsey Mitchell",
      Player == "Laeticia Amihere" ~ "Laeticia Amihere",
      TRUE ~ "Other"
    ),
    label = case_when(
      group %in% c("Kelsey Mitchell", "Laeticia Amihere", "Outlier") ~ Player,
      TRUE ~ NA_character_
    )
  )

kelsey <- data %>%
  filter(Player == "Kelsey Mitchell")

amihiere <- data %>%
  filter(Player == "Laeticia Amihere")

ggplot(plot_data, aes(x = FG., y = eFG.)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_point(aes(color = group), size = 2.8, alpha = 0.8, color="gray70") +
  ggrepel::geom_text_repel(
    aes(label = label),
    size = 6,
    box.padding = 0.35,
    point.padding = 0.2,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
    geom_point(
      data = kelsey,
      aes(x = FG., y = eFG.),
      shape=21, 
      fill = "#E76F51",
      stroke = 1.2,
      size = 3.8
    ) +
    geom_point(
      data = amihiere,
      aes(x = FG., y = eFG.),
      shape = 21,
      fill = "#2A9D8F",
      stroke = 1.2,
      size = 3.8
    ) +
  labs(
    title = "Effective field goal% gives extra credit for made 3s",
    subtitle = "Field Goal% vs Effective Field Goal%, WNBA 2025",
    caption = "data: Basketball Reference | chart: @wnbadata",
    x = "FG% (raw shooting percentage)",
    y = "eFG% (adjusted for 3-pointers)",
    color = NULL
  ) + scale_x_continuous(
    breaks = seq(0, 1, by = 0.1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    labels = scales::label_percent(accuracy = 1)
  )+
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )



#TS vs efg%
plot_data_ts <- data %>%
  mutate(
    gap_ts = TS. - eFG.
  ) %>%
  # pick, say, top 6 players whose TS% is most above eFG%
  arrange(desc(gap_ts)) %>%
  mutate(
    highlight = row_number() <= 10,
    group = if_else(highlight, "High TS boost", "Other"),
    label = if_else(highlight, Player, NA_character_)
  )

ggplot(plot_data_ts, aes(x = eFG., y = TS.)) +
  # 45-degree reference line: TS% = eFG%
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", color = "black"
  ) +
  # background cloud
  geom_point(
    data = filter(plot_data_ts, !highlight),
    aes(x = eFG., y = TS.),
    color = "gray80",
    size = 2.8,
    alpha = 0.8
  ) +
  # highlighted outliers
  geom_point(
    data = filter(plot_data_ts, highlight),
    aes(x = eFG., y = TS.),
    shape = 21,
    fill  = "#2A9D8F",   # or whatever accent you like
    color = "white",
    stroke = 1.2,
    size  = 3.8
  ) +
  # labels for highlighted outliers
  ggrepel::geom_text_repel(
    data = filter(plot_data_ts, highlight),
    aes(x = eFG., y = TS., label = label),
    size = 4.5,
    box.padding = 0.35,
    point.padding = 0.2,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  labs(
    title    = "True shooting% rewards free throws",
    subtitle = "eFG% vs TS%, WNBA 2025",
    caption  = "data: Basketball Reference | chart: @wnbadata",
    x = "eFG% (adjusted for 3-pointers)",
    y = "TS% (eFG% plus free throws)"
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "none",
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


top_efg <- player_filter %>%
  arrange(desc(efg_pct)) %>%
  slice_head(n = 10) %>%
  mutate(rank = row_number()) %>%
  select(rank, Player, Team, FGA, efg_pct, ts_pct)

top_efg_table <- top_efg %>%
  gt() %>%
  tab_header(
    title    = "Top eFG% Scorers, WNBA 2025",
    subtitle = "Minimum 50 FGA · eFG% adjusts FG% for 3-pointers"
  ) %>%
  cols_label(
    rank    = "Rk",
    Player  = "Player",
    Team      = "Team",
    FGA     = "FGA",
    efg_pct = "eFG%",
    ts_pct  = "TS%"
  ) %>%
  fmt_number(
    columns = c(FGA),
    decimals = 0
  ) %>%
  fmt_percent(
    columns = c(efg_pct, ts_pct),
    decimals = 1
  ) %>%
  tab_source_note(
    source_note = md("data: Basketball Reference · table: @wnbadata")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = 12,
    data_row.padding = px(4),
    table.width = pct(100),
    column_labels.background.color = "#F3F3F3"
  )


top_ts <- player_filter %>%
  arrange(desc(ts_pct)) %>%
  slice_head(n = 10) %>%
  mutate(rank = row_number()) %>%
  select(rank, Player, Team, FGA, efg_pct, ts_pct)

top_ts_table <- top_ts %>%
  gt() %>%
  tab_header(
    title    = "Top TS% Scorers, WNBA 2025",
    subtitle = "Minimum 50 FGA · TS% combines eFG% with free throws"
  ) %>%
  cols_label(
    rank    = "Rk",
    Player  = "Player",
    Team      = "Team",
    FGA     = "FGA",
    efg_pct = "eFG%",
    ts_pct  = "TS%"
  ) %>%
  fmt_number(
    columns = c(FGA),
    decimals = 0
  ) %>%
  fmt_percent(
    columns = c(efg_pct, ts_pct),
    decimals = 1
  ) %>%
  tab_source_note(
    source_note = md("data: Basketball Reference · table: @wnbadata")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = 12,
    data_row.padding = px(4),
    table.width = pct(100),
    column_labels.background.color = "#F3F3F3"
  )

top_boost <- player_filter %>%
  arrange(desc(ts_boost)) %>%
  slice_head(n = 10) %>%
  mutate(rank = row_number()) %>%
  select(rank, Player, Team, FGA, efg_pct, ts_pct, ts_boost)

top_boost_table <- top_boost %>%
  gt() %>%
  tab_header(
    title    = "Who Does TS% Love the Most?",
    subtitle = "Top TS% – eFG% gaps, WNBA 2025 · Minimum 100 FGA"
  ) %>%
  cols_label(
    rank     = "Rk",
    Player   = "Player",
    Team       = "Team",
    FGA      = "FGA",
    efg_pct  = "eFG%",
    ts_pct   = "TS%",
    ts_boost = "TS% - eFG%"
  ) %>%
  fmt_number(
    columns = c(FGA),
    decimals = 0
  ) %>%
  fmt_percent(
    columns = c(efg_pct, ts_pct),
    decimals = 1
  ) %>%
  fmt_percent(
    columns = ts_boost,
    decimals = 1,
    force_sign = TRUE
  ) %>%
  data_color(
    columns = ts_boost,
    colors = scales::col_bin(
      palette = c("#F4A261", "#E76F51"), # light to stronger orange/red
      domain  = NULL,
      bins    = 3
    )
  ) %>%
  tab_source_note(
    source_note = md("data: Basketball Reference · table: @wnbadata")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = 12,
    data_row.padding = px(4),
    table.width = pct(100),
    column_labels.background.color = "#F3F3F3"
  )
  




top_efg_table
top_ts_table
top_boost_table