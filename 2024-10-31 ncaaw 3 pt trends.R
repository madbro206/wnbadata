#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, ggplot2)

#load data
wbb_player_box <- wehoop::load_wbb_player_box(season=c(2017:2024))

#######################################################################################################
#translate into player, team, and opponent box score

Pbox <- wbb_player_box %>%
  group_by(season, team_location, athlete_display_name) %>%
  summarise(GP=sum(minutes>0, na.rm=TRUE), MIN=sum(minutes, na.rm = TRUE), PTS=sum(points, na.rm = TRUE),
    P2M=sum(field_goals_made, na.rm = TRUE)-sum(three_point_field_goals_made, na.rm = TRUE), P2A=sum(field_goals_attempted, na.rm = TRUE)-sum(three_point_field_goals_attempted, na.rm = TRUE), P2p=100*P2M/P2A,
    P3M=sum(three_point_field_goals_made, na.rm = TRUE), P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), P3p=100*P3M/P3A,
    FTM=sum(free_throws_made, na.rm = TRUE), FTA=sum(free_throws_attempted, na.rm = TRUE), FTp=100*FTM/FTA) %>%
  rename(Season=season, Team=team_location,
         Player=athlete_display_name) %>%
         filter(MIN>=150) %>% #MIN 150 mins played
         filter(GP>=3) %>% #trying to filter out non d1 players
           as.data.frame()


#############################################################################################################
#overall 3pt percentage, includes games by non-d1 players against d1 teams
total <- wbb_player_box %>%
  group_by(season) %>%
  summarise(GP=sum(minutes>0, na.rm=TRUE),P3M=sum(three_point_field_goals_made, na.rm = TRUE), P3A=sum(three_point_field_goals_attempted, na.rm = TRUE), P3p=100*P3M/P3A) %>%
  rename(Season=season) %>%
  as.data.frame()

total


#players who use at least 50% of their scoring attempts on threes
#i.e. 3PA > 2PA +0.475*FTA
three_shooters <- Pbox %>%
  filter(P3A > P2A + 0.475*FTA) %>%
  group_by(Season) %>%
  summarise(P3M=sum(P3M, na.rm = TRUE), P3A=sum(P3A, na.rm = TRUE), P3p=100*P3M/P3A) %>%
  as.data.frame()

three_shooters



#non shooters
#no more than 15% of their scoring attempts on threes
non_shooters <- Pbox %>%
  filter(P3A < .15 * (P3A + P2A + 0.475*FTA)) %>%
  group_by(Season) %>%
  summarise(P3M=sum(P3M, na.rm = TRUE), P3A=sum(P3A, na.rm = TRUE), P3p=100*P3M/P3A) %>%
  as.data.frame()
  
non_shooters

#############################################################################################################
ggplot(total, aes(x = Season, y = P3p)) +
  geom_path(size=1.5, color="#2a2529") + 
  geom_point(shape=21, color="black", fill="#f88158", size=3) + 
  ylim(30,33)+
  labs(title="NCAAW 3 point percentage since 2016-17", x="Season", y="Overall 3 point percentage", caption="data: wehoop | graphic: @wnbadata")+ 
  scale_x_continuous(breaks = c(2017:2024))+
  theme_minimal()



ggplot(three_shooters, aes(x = Season, y = P3p)) +
  geom_path(size=1.5, color="#2a2529") + 
  geom_point(shape=21, color="black", fill="#f88158", size=3) + 
  #ylim(30,34)+
  labs(title="High-volume shooter 3 point percentage", subtitle="NCAAW since 2016-17", x="Season", y="3 point percentage", caption="Players with at least 150 minutes on the season\ndata: wehoop | graphic: @wnbadata")+ 
  scale_x_continuous(breaks = c(2017:2024))+
  theme_minimal()
  
ggplot(non_shooters, aes(x = Season, y = P3p)) +
  geom_path(size=1.5, color="#2a2529") + 
  geom_point(shape=21, color="black", fill="#f88158", size=3) + 
  #ylim(30,34)+
  labs(title="Low-volume shooter 3 point percentage", subtitle="NCAAW since 2016-17", x="Season", y="3 point percentage", caption="Players with at least 150 minutes on the season\ndata: wehoop | graphic: @wnbadata")+ 
  scale_x_continuous(breaks = c(2017:2024))+
  theme_minimal()
  
  
library(gt)
library(dplyr)

total %>%
  mutate(P3p = round(P3p, 2)) %>%
  gt() %>%
  tab_header(
    title = "Three-Point Performance in Women's College Basketball",
    subtitle = "Line moved back 16 inches in 2022"
  ) %>%
  fmt_number(
    columns = c(GP, P3M, P3A),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  fmt_percent(
    columns = P3p,
    decimals = 2,
    scale_values = FALSE
  ) %>%
  cols_label(
    Season = "Season",
    GP = "Player Games",
    P3M = "3PT Made",
    P3A = "3PT Attempted",
    P3p = "3PT %"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "gray85",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body()
  ) %>%
  tab_options(
    table.border.top.color = "white",
    table.border.bottom.color = "white",
    data_row.padding = px(3),
    column_labels.background.color = "#F0F0F0",
    column_labels.font.weight = "bold",
    table.width = pct(100)
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_source_note(
    source_note = "data: wehoop | graphic: @wnbadata"
  ) %>%
  opt_row_striping() %>%
  opt_table_font(
    font = google_font("Roboto")
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#F9F9F9"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  )
  
  
three_shooters %>%
  mutate(P3p = round(P3p, 2)) %>%
  gt() %>%
  tab_header(
    title = "Three-Point Performance in Women's College Basketball",
    subtitle = "By players with at least half of their scoring attempts as 3 pointers"
  ) %>%
  fmt_number(
    columns = c(P3M, P3A),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  fmt_percent(
    columns = P3p,
    decimals = 2,
    scale_values = FALSE
  ) %>%
  cols_label(
    Season = "Season",
    P3M = "3PT Made",
    P3A = "3PT Attempted",
    P3p = "3PT %"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "gray85",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body()
  ) %>%
  tab_options(
    table.border.top.color = "white",
    table.border.bottom.color = "white",
    data_row.padding = px(3),
    column_labels.background.color = "#F0F0F0",
    column_labels.font.weight = "bold",
    table.width = pct(100)
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_source_note(
    source_note = "data: wehoop | graphic: @wnbadata | idea: Calvin Wetzel"
  ) %>%
  opt_row_striping() %>%
  opt_table_font(
    font = google_font("Roboto")
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#F9F9F9"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  )
  
non_shooters %>%
  mutate(P3p = round(P3p, 2)) %>%
  gt() %>%
  tab_header(
    title = "Three-Point Performance in Women's College Basketball",
    subtitle = "By players with at most 15% of their scoring attempts as 3 pointers"
  ) %>%
  fmt_number(
    columns = c(P3M, P3A),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  fmt_percent(
    columns = P3p,
    decimals = 2,
    scale_values = FALSE
  ) %>%
  cols_label(
    Season = "Season",
    P3M = "3PT Made",
    P3A = "3PT Attempted",
    P3p = "3PT %"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "gray85",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body()
  ) %>%
  tab_options(
    table.border.top.color = "white",
    table.border.bottom.color = "white",
    data_row.padding = px(3),
    column_labels.background.color = "#F0F0F0",
    column_labels.font.weight = "bold",
    table.width = pct(100)
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_source_note(
    source_note = "data: wehoop | graphic: @wnbadata | idea: Calvin Wetzel"
  ) %>%
  opt_row_striping() %>%
  opt_table_font(
    font = google_font("Roboto")
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#F9F9F9"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  )