library(ggplot2)
library(dplyr)

data <- read.csv("~/Desktop/wehoop/rookie_efg_ast_tov.csv")

data$ATO <- data$AST/data$TOV

ggplot(data, aes(x = ATO, y = eFGp)) +
  geom_point() +
  geom_point(
    data = subset(data, Player == "Paige Bueckers"),
    color = "red", size = 4
  ) +
  ggrepel::geom_text_repel(
    data = subset(data, Player == "Paige Bueckers"),
    aes(label = Player),
    nudge_y = 0.03,
    color = "red"
  ) +
  xlab("Assist to turnover ratio") +
  ylab("Effective Field Goal Percentage") +
  labs(
    title = "Historical Rookie Efficiency in the WNBA",
    subtitle = "Rookie seasons with >=10 games, ast, tov"
  ) +
  theme_minimal()


data %>% arrange(desc(ATO)) %>% select("Player", "ATO")
