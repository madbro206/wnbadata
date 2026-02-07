#data from sports illustrated
#https://www.si.com/ranking-top-unrivaled-lineups-2026-season-bueckers-brink-martin

library(tibble)
library(dplyr)
library(ggplot2)
library(ggrepel)


lineups <- tribble(
  ~team,      ~lineup,                                        ~possessions, ~net_rating,
  "Breeze",   "Brink, Martin, Bueckers",                      127,          44.2,
  "Mist",     "Gray, Stewart, Burton",                        272,          42.9,
  "Breeze",   "Brink, Bueckers, Jackson",                     108,          30.7,
  "Phantom",  "Boston, Plum, Hayes",                          208,          27.5,
  "Rose",     "Stevens, Gray, Hull",                          129,          24.7,
  "Laces",    "Sykes, Canada, Hillmon",                       152,          14.6,
  "Phantom",  "Plum, Iriafen, Cloud",                         114,          10.2,
  "Laces",    "Canada, Siegrist, Hillmon",                    141,          10.1,
  "Vinyl",    "Hamby, Wheeler, Howard",                       125,           4.8,
  "Rose",     "Gray, Austin, Sutton",                         114,           3.5,
  "Phantom",  "Boston, Plum, Cloud",                          236,          -2.1,
  "Breeze",   "Malonga, Martin, Bueckers",                    110,          -3.6,
  "Rose",     "Gray, Hull, Austin",                           132,          -4.5,
  "Breeze",   "Malonga, Bueckers, Jackson",                   369,          -5.0,
  "Lunar Owls",    "Edwards, Mabrey, Allen",                       382,          -6.2,
  "Laces",    "Thomas, Sykes, Young",                         297,          -6.4,
  "Rose",     "Gray, Copper, Austin",                         126,          -7.2,
  "Lunar Owls",    "Edwards, Mabrey, Diggins",                     171,         -12.7,
  "Hive",     "Magbegor, Mitchell, Citron",                   336,         -14.4,
  "Phantom",  "Iriafen, Cloud, Hayes",                        122,         -16.5,
  "Hive",     "Billings, Hiedeman, Rivers",                   137,         -19.4,
  "Vinyl",    "Williams, Hamby, Howard",                      348,         -21.7,
  "Rose",     "Stevens, Gray, Copper",                        163,         -23.0,
  "Hive",     "Mitchell, Billings, Citron",                   128,         -30.0
)

lineups_lab <- lineups %>%
  mutate(
    is_outlier = net_rating >= 25 | net_rating <= -15 | possessions >250,
    label_text = ifelse(is_outlier, lineup, NA_character_)
  )
  team_colors <- c(
    "Breeze"     = "#A64DFF",
    "Hive"       = "#F2C94C",  
    "Laces"      = "#00C2CB",  
    "Lunar Owls" = "#002df8",  
    "Mist"       = "#808ea1",  
    "Phantom"    = "#13b608", 
    "Rose"       = "#2F4F2F", 
    "Vinyl"      = "#B71C1C"  
  )

ggplot(lineups_lab, aes(x = possessions, y = net_rating, color = team)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(size = 3, alpha = 0.9) +
  ggrepel::geom_text_repel(
    aes(label = label_text),
    size = 3,
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  scale_color_manual(values = team_colors) +
  scale_x_continuous("Possessions (min 100)") +
  scale_y_continuous("Net rating (points per 100 possessions)") +
  labs(
    title = "2026 Unrivaled Lineups by Net Rating",
    subtitle = "Lineups with at least 100 possessions, through 1/28",
    caption="data: Dan Falkenheim | chart: @wnbadata",
    color = "Team"
  ) +
  theme_minimal()
  