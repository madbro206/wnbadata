library(ggplot2)

#data from https://www.thenexthoops.com/ncaaw/2025-transfer-portal-tracker-power-five/
big_ten <- data.frame(
  School = c('Illinois', 'Indiana', 'Iowa', 'Maryland', 'Michigan', 
             'Michigan State', 'Minnesota', 'Nebraska', 'Northwestern', 
             'Ohio State', 'Oregon', 'Penn State', 'Purdue', 'Rutgers', 
             'UCLA', 'USC', 'Washington', 'Wisconsin'),
  Transfers_Out = c(2, 6, 1, 2, 1, 3, 2, 0, 2, 2, 0, 8, 6, 3, 6, 4, 0, 5)
)

ggplot(big_ten, aes(x = reorder(School, Transfers_Out), y = Transfers_Out)) +
  geom_col(fill = "#0088CE") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 8, 1), minor_breaks = NULL) +
  labs(
    title = "B1G Women's Basketball Transfers Out",
    subtitle = "2025 Transfer Portal Cycle",
    caption = "data: The IX Basketball",
    x = NULL,
    y = "Number of Transfers Out"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank()
  )



#by conference
conferences <- data.frame(
  Conference = c('ACC', 'B1G', 'Big 12', 'BIG EAST', 'SEC'),
  Transfers_Out = c(85, 53, 81, 31, 78)
)


ggplot(conferences, aes(x = Conference, y = Transfers_Out)) +
  geom_col(fill = "purple") +
  geom_text(aes(label = Transfers_Out), vjust = -0.5, size = 4) +
  scale_y_continuous(breaks = seq(0, 90, 10), 
                      minor_breaks = NULL,
                      limits = c(0, 95)) +
  labs(
    title = "Women's Basketball Transfers Out by Conference",
    subtitle = "2025 Transfer Portal Cycle",
    caption = "data: The IX Basketball",
    x = NULL,
    y = "Number of (school) Transfers Out"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank()
  )