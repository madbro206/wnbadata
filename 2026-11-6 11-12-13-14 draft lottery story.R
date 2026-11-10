# charts for the "11-12-13-14 story" video (draft lottery, publishes 11/6)
#
# everything here is plain math with no outside data, so no data credit, just @wnbadata.
#
# n = balls in the machine, k = balls pulled. choose(n, k) = number of combinations.

library(dplyr)
library(tidyr)
library(ggplot2)

BLUE   <- "#2a78d6"   # fewer than 1000
ORANGE <- "#eb6834"   # more than 1000, and the highlight color
MID    <- "#ECECE8"   # right around 1000
GRAY   <- "#8A8A86"
INK    <- "#15171D"
CREDIT <- "chart: @wnbadata"

base_theme <- function() {
  theme_minimal(base_size = 15) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_text(face = "bold", color = INK, size = 19),
      plot.subtitle = element_text(color = GRAY, size = 13, margin = margin(b = 14)),
      plot.caption  = element_text(color = GRAY, size = 11, hjust = 0, margin = margin(t = 14)),
      axis.text     = element_text(color = INK, size = 13),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#E3E3E0", linewidth = 0.3),
      plot.margin   = margin(18, 18, 14, 10)
    )
}

stopifnot(choose(14, 4) == 1001, choose(13, 4) == 715, choose(15, 4) == 1365)


# 13 BALLS VS 14 VS 15, ALWAYS PULLING 4
# the "why 14 balls?" beat. bars start at zero.
ball_options <- tibble(balls = 13:15, combos = choose(balls, 4))

p_ball_options <- ggplot(ball_options, aes(factor(balls), combos, fill = balls == 14)) +
  geom_col(width = 0.62) +
  geom_hline(yintercept = 1000, color = INK, linewidth = 0.6, linetype = "dashed") +
  annotate("text", x = 0.55, y = 1025, label = "1000", hjust = 0, vjust = 0,
           color = INK, size = 4.6) +
  geom_text(aes(label = combos), vjust = -0.5,
            color = INK, size = 6, fontface = "bold") +
  scale_fill_manual(values = c(`TRUE` = ORANGE, `FALSE` = "#C9C9C4"), guide = "none") +
  scale_y_continuous(limits = c(0, 1520), breaks = NULL) +
  labs(title = "Possible combos when you pull 4 balls",
       x = "balls in the machine", y = NULL, caption = CREDIT) +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        axis.title.x = element_text(color = GRAY, size = 13, margin = margin(t = 8)))


# PASCAL'S TRIANGLE
# every n choose k through 15 balls. color = how far from 1000, on a log scale so
# 10x too small and 10x too big look equally far away. the 2 outlined cells are the 1001s.
tri_rows <- 15

triangle <- expand_grid(n = 1:tri_rows, k = 0:tri_rows) %>%
  filter(k <= n) %>%
  mutate(combos = choose(n, k),
         x = k - n / 2,
         y = -n,
         off = pmax(pmin(log10(combos / 1000), 1), -1),
         is_1001 = combos == 1001,
         text_col = ifelse(abs(off) > 0.7, "white", INK))

stopifnot(sum(triangle$is_1001) == 2)

p_triangle <- ggplot(triangle, aes(x, y)) +
  geom_tile(aes(fill = off), width = 0.95, height = 0.9) +
  geom_tile(data = filter(triangle, is_1001), width = 0.95, height = 0.9,
            fill = NA, color = INK, linewidth = 1.1) +
  geom_text(aes(label = combos, color = text_col), size = 3.1) +
  annotate("text", x = -tri_rows / 2 - 1.1, y = -(1:tri_rows), label = 1:tri_rows,
           color = GRAY, size = 3.4, hjust = 1) +
  annotate("text", x = -tri_rows / 2 - 1.1, y = 0, label = "balls",
           color = GRAY, size = 3.4, hjust = 1) +
  scale_fill_gradient2(low = BLUE, mid = MID, high = ORANGE, midpoint = 0,
                       limits = c(-1, 1), breaks = c(-1, 0, 1),
                       labels = c("100 or fewer", "1000", "10000 or more"),
                       name = "combinations") +
  scale_color_identity() +
  coord_fixed(ratio = 1, clip = "off") +
  labs(title = "Combinations by number of balls",
       subtitle = "move right to pull more balls",
       x = NULL, y = NULL, caption = CREDIT) +
  base_theme() +
  theme(axis.text = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(3.2, "lines"),
        legend.title = element_text(color = GRAY, size = 12),
        legend.text  = element_text(color = INK, size = 11))


# CHART INDEX
#   "so why 14 balls? with 13 you get 715, with 15 you get 1365"   p_ball_options
#   the same beat, the "every x choose y" version                  p_triangle
p_ball_options
p_triangle
