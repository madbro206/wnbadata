library(tidyr)
library(dplyr)
library(ggplot2)
library(wehoop)
library(tictoc)
library(progressr)
library(stringr)

#load data
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp()
})
tictoc::toc()


q3_diff <- wnba_pbp %>%
  filter(period_number == 3) %>%
  group_by(game_id) %>%
  arrange(sequence_number) %>%  # ensure play order within game

  summarise(
    game_date      = first(game_date),
    home_team_id   = first(home_team_id),
    home_team_name = first(home_team_name),
    away_team_id   = first(away_team_id),
    away_team_name = first(away_team_name),

    # scores at start of Q3 (just before any Q3 scoring)
    home_start = first(home_score),
    away_start = first(away_score),

    # scores at end of Q3 (after last Q3 scoring play)
    home_end   = last(home_score),
    away_end   = last(away_score),

    # points scored in Q3 by each team
    home_q3_pts = home_end - home_start,
    away_q3_pts = away_end - away_start,

    # differential: home minus away in Q3
    q3_home_diff = home_q3_pts - away_q3_pts
  ) %>%
  ungroup()

all <- q3_diff %>% select(game_date, home_team_name, away_team_name, q3_home_diff)
print(all, n=312)

fever_all<- q3_diff %>% filter(home_team_name=="Indiana") %>% select(game_date, home_team_name, away_team_name, q3_home_diff)
fever_curds <- fever_all %>% filter(q3_home_diff >0)


q3_home_wins_by_team <- q3_diff %>%
  filter(q3_home_diff > 0) %>%              # home outscored opponent in Q3
  group_by(home_team_id, home_team_name) %>%
  summarise(
    n_q3_home_outscore = n(),               # number of such games
    .groups = "drop"
  ) %>%
  arrange(desc(n_q3_home_outscore))



q_diff <- wnba_pbp %>%
  filter(period_number %in% 1:4) %>%
  group_by(game_id, period_number) %>%
  arrange(sequence_number) %>%
  summarise(
    game_date      = first(game_date),
    home_team_id   = first(home_team_id),
    home_team_name = first(home_team_name),
    away_team_id   = first(away_team_id),
    away_team_name = first(away_team_name),
    home_start     = first(home_score),
    away_start     = first(away_score),
    home_end       = last(home_score),
    away_end       = last(away_score),
    home_pts_q     = home_end - home_start,
    away_pts_q     = away_end - away_start,
    home_q_diff    = home_pts_q - away_pts_q,
    .groups = "drop"
  )



home_q_win_counts <- q_diff %>%
  mutate(home_outscored = home_q_diff > 0) %>%
  group_by(home_team_id, home_team_name, period_number, home_outscored) %>%
  summarise(n = n(), .groups = "drop")

home_q_win_table <- home_q_win_counts %>%
  filter(home_outscored) %>%                 # only cases where home won the quarter
  select(-home_outscored) %>%
  pivot_wider(
    names_from  = period_number,
    values_from = n,
    names_prefix = "q",
    values_fill = 0
  ) %>%
  arrange(home_team_name)




library(dplyr)
library(tidyr)
library(ggplot2)

fever_q_wins_long <- home_q_win_table %>%
  filter(home_team_name == "Indiana") %>%
  pivot_longer(
    cols = q1:q4,
    names_to = "quarter",
    values_to = "games_outscored"
  )

  ggplot(fever_q_wins_long,
    aes(x = quarter, y = games_outscored,
        fill = quarter == "q3")) +
geom_col(width = 0.6) +
geom_text(aes(label = games_outscored),
         vjust = -0.3, size = 6) +
scale_fill_manual(values = c("FALSE" = "grey70", "TRUE" = "#ffb000")) +
labs(
 title = "Fever home quarter wins",
 x = "Quarter",
 y = "Games outscoring opponent"
) +
coord_cartesian(ylim = c(0, 18)) +
theme_minimal(base_size = 18) +
theme(
 legend.position = "none",
 plot.title = element_text(hjust = 0.5)
)


library(dplyr)
library(ggplot2)

q3_by_team <- home_q_win_table %>%
  select(home_team_name, q3)

ggplot(q3_by_team,
       aes(x = reorder(home_team_name, q3), y = q3)) +
  geom_col(fill = "grey70") +
  geom_col(data = subset(q3_by_team, home_team_name %in% c("Indiana", "Minnesota")),
           aes(x = home_team_name, y = q3),
           fill = c("Indiana" = "#ffb000", "Minnesota" = "#0072ce")[home_team_name]) +
  geom_text(aes(label = q3), hjust = -0.2, size = 4) +
  coord_flip() +
  labs(
    title = "Home third-quarter wins by team",
    x = NULL,
    y = "Games outscoring opponent in Q3"
  ) +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5))


library(dplyr)
library(ggplot2)

q3_by_team <- home_q_win_table %>%
  filter(home_team_name !="Team Clark") %>%
  select(home_team_name, q3) %>%
  mutate(
    highlight = case_when(
      home_team_name == "Indiana"   ~ "Indiana",
      home_team_name == "Minnesota" ~ "Minnesota",
      TRUE                          ~ "Other"
    )
  )

ggplot(q3_by_team,
       aes(x = reorder(home_team_name, q3), y = q3,
           fill = highlight)) +
  geom_col() +
  geom_text(aes(label = q3), hjust = -0.2, size = 4) +
  scale_fill_manual(
    values = c(
      "Indiana"   = "#ffb000",
      "Minnesota" = "#0072ce",
      "Other"     = "grey70"
    )
  ) +
  coord_flip(ylim = c(0, max(q3_by_team$q3) + 2)) +
  labs(
    title = "Home third-quarter wins by team",
    x = NULL,
    y = "Games outscoring opponent in Q3"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )
