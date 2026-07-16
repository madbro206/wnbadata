library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)

#gathered from basketball reference :/ wehoop was down idk
reese <- tribble(
  ~game,        ~mebounds, ~orb, ~drb, ~total,
  "5/9/2026",   2,  9,  5, 14,
  "5/12/2026",  2,  8,  8, 16,
  "5/17/2026",  0,  4,  4,  8,
  "5/22/2026",  0,  3,  6,  9,
  "5/24/2026",  1,  5,  5, 10,
  "5/27/2026",  1,  4,  4,  8,
  "5/29/2026",  2,  5,  7, 12,
  "6/2/2026",   1,  7,  6, 13,
  "6/4/2026",   0,  4,  6, 10,
  "6/6/2026",   1,  7, 10, 17,
  "6/9/2026",   1,  5, 12, 17,
  "6/11/2026",  1,  6,  3,  9,
  "6/14/2026",  5, 11,  6, 17,
  "6/18/2026",  1,  4,  7, 11,
  "6/20/2026",  0,  4,  4,  8,
  "6/22/2026",  0,  3,  6,  9,
  "6/24/2026",  3,  7,  5, 12,
  "6/26/2026",  1,  5,  7, 12,
  "6/27/2026",  0,  1,  8,  9,
  "7/2/2026",   2,  6,  7, 13,
  "7/4/2026",   2,  5,  8, 13,
  "7/9/2026",   2,  3,  8, 11,
  "7/11/2026",  NA, NA, NA, NA,
  "7/13/2026",  1,  3, 10, 13
) %>%
  mutate(game = as.Date(game, format = "%m/%d/%Y")) %>%
  filter(!is.na(total))  # drops the missing 7/11 game


reese_long <- reese %>%
  pivot_longer(cols = c(mebounds, orb, drb, total),
               names_to = "stat", values_to = "value") %>%
  mutate(stat = factor(stat,
                        levels = c("mebounds", "orb", "drb", "total"),
                        labels = c("Mebounds", "OREB", "DREB", "Total REB")))


ggplot(reese_long, aes(x = stat, y = value, fill = stat)) +
  geom_boxplot(width = 0.5, outlier.shape = 21, outlier.size = 2) +
  geom_jitter(width = 0.001, alpha = 0.2, height=0, size = 3) +
  scale_fill_manual(values = c("Mebounds" = "#C8102E",
                                "OREB"     = "#9EA2A2",
                                "DREB"     = "#CED9E5",
                                "TREB"     = "#373A36")) +
  scale_y_continuous(breaks = seq(0, max(reese_long$value, na.rm = TRUE), by = 1)) +
  labs(
    title = "Angel Reese rebounding, 2026 season",
    subtitle = "through 7/13/2026",
    x = NULL,
    y = "reb",
    caption= "chart @wnbadata"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )