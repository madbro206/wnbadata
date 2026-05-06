#assist to turnover ratio
library(wehoop)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(gt)
library(stringr)
library(ggrepel)

#load data and filter out all star games
data <- load_wnba_player_box(seasons = c(2025))
player_ast_tov <- data %>% 
  filter(
    team_name != "TEAM CLARK",
    team_name != "TEAM COLLIER",
    season_type == 2
  ) %>%
  group_by(athlete_id, athlete_display_name) %>%
  summarise(
    AST = sum(assists, na.rm = TRUE),
    TOV = sum(turnovers, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(AST_TOV = AST / if_else(TOV == 0, NA_real_, TOV)) %>%
  filter(AST>=50) %>%
  arrange(desc(AST_TOV))

print(player_ast_tov, n=15)

data2 <- read.csv("~/Desktop/wehoop/2025_bpTOV.csv")


#scatterplot
labeled_players <- player_ast_tov %>% 
  filter(
    AST >= 250 | TOV >=100 | AST_TOV > 2.5
  )


ggplot(player_ast_tov, aes(x = TOV, y = AST)) +
  # all players
  geom_point(alpha = 0.6, color = "gray70") +
  # highlighted labeled players
  geom_point(
    data = labeled_players,
    color = "dodgerblue3",
    size  = 3
  ) +
  ggrepel::geom_text_repel(
    data = labeled_players,
    aes(
      label = paste0(
        athlete_display_name, "\n",
        round(AST_TOV, 2)
      )
    ),
    size = 3,
    color = "black",
    box.padding = 0.3,
    max.overlaps = 20
  ) +
  scale_x_continuous(
    limits = c(0, max(player_ast_tov$TOV + 10, na.rm = TRUE)),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, max(player_ast_tov$AST + 10, na.rm = TRUE)),
    expand = c(0, 0)
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dotted",
    color = "red",
    linewidth = 0.5
  ) +
  labs(
    x = "Turnovers",
    y = "Assists",
    title = "WNBA 2025: Assists vs Turnovers, players with at least 50 assists",
    subtitle = "Labeled outliers by assist-to-turnover ratio",
    caption = "data: Basketball Reference, Wehoop | chart: @wnbadata"
  ) +
  theme_minimal()


#bad pass tov assist ratio
badpass <- data2 %>% 
  rename(athlete_display_name = Player) %>% 
  group_by(athlete_display_name) %>% 
  slice_max(BadPass, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(
    athlete_display_name = as.character(athlete_display_name)
  )

player_ast_tov_bp <- player_ast_tov %>% 
  left_join(badpass, by = "athlete_display_name") %>% 
  mutate(
    AST_BadPass = AST / if_else(BadPass == 0 | is.na(BadPass),
                                NA_real_, BadPass)
  ) %>%
  filter(AST>=50) %>%
  arrange(desc(AST_BadPass))

print(player_ast_tov_bp, n=15)

player_ast_tov_bp %>%select(athlete_display_name, AST, TOV, BadPass, AST_BadPass)

labeled_players_bp <- player_ast_tov_bp %>% 
  filter(
    AST >= 250 | TOV >=100 | AST_BadPass>4.5
  )

ggplot(player_ast_tov_bp, aes(x = BadPass, y = AST)) +
  # all players
  geom_point(alpha = 0.6, color = "gray70") +
  # highlighted labeled players
  geom_point(
    data = labeled_players_bp,
    color = "dodgerblue3",
    size = 3
  ) +
  # labels
  ggrepel::geom_text_repel(
    data = labeled_players_bp,
    aes(
      label = paste0(
        athlete_display_name, "\n",
        round(AST_BadPass, 2)
      )
    ),
    size = 3,
    color = "black",
    box.padding = 0.3,
    max.overlaps = 20
  ) +
  # axes from zero with a little padding
  scale_x_continuous(
    limits = c(0, max(player_ast_tov_bp$BadPass + 10, na.rm = TRUE)),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, max(player_ast_tov_bp$AST + 10, na.rm = TRUE)),
    expand = c(0, 0)
  ) +
  labs(
    x = "Bad Pass Turnovers",
    y = "Assists",
    title = "WNBA 2025: Assists vs Bad Pass Turnovers, players with >= 50 assists.",
    subtitle = "Labeled outliers by assist-to-bad-pass ratio",
    caption = "data: Basketball Reference, Wehoop | chart: @wnbadata"
  ) +
  theme_minimal()



#improvement from ast_tov vs just bad pass tov
player_ast_tov_bp <- player_ast_tov_bp %>% 
  mutate(
    ratio_diff  = AST_BadPass - AST_TOV,
    ratio_ratio = AST_BadPass / AST_TOV
  )

player_ast_tov_bp <- player_ast_tov_bp %>% 
  mutate(
    ratio_diff  = AST_BadPass - AST_TOV,
    ratio_ratio = AST_BadPass / AST_TOV
  ) %>%
  filter(
    AST >= 50,          # example assist cutoff
    TOV >= 20           # example turnover cutoff
  )

top_by_ratio <- player_ast_tov_bp %>% 
  arrange(desc(ratio_ratio)) %>% 
  slice_head(n = 15) %>% 
  select(athlete_display_name, AST, TOV, BadPass,
          AST_TOV, AST_BadPass, ratio_diff, ratio_ratio)

top_by_ratio
