library(dplyr)
library(stringr)
library(ggplot2)

df <- read.csv("/Users/maddy/Desktop/wehoop/draft_lottery_data.csv")

df_clean <- df %>%
  mutate(team_lottery = str_trim(gsub("\\s*\\(.*\\)", "", team)))

df_clean <- df_clean %>%
  mutate(
    balls = as.numeric(str_extract(odds, "^[0-9]+")),
    prob1 = balls / 1000
  )

df_clean

################ first pick luck
df_luck <- df_clean %>%
  mutate(
    got_1 = as.integer(pick == 1),
    luck1 = got_1 - prob1
  )

franchise_luck <- df_luck %>%
  group_by(team_lottery) %>%
  summarise(
    n_lotteries = n_distinct(draft_year),
    total_luck1 = sum(luck1, na.rm = TRUE),
    avg_luck1   = mean(luck1, na.rm = TRUE)
  ) %>%
  arrange(desc(total_luck1))

print(franchise_luck, n=22)

df_clean %>% filter(team_lottery=="Seattle Storm")%>% select(team_lottery, draft_year, original_order, pick, prob1) %>%arrange(desc(draft_year))


###################### general luck
df_luck2 <- df_clean %>%
  mutate(
    luck_pick = original_order - pick
  )

team_luck_pick <- df_luck2 %>%
  group_by(team_lottery) %>%
  summarise(
    n_lotteries   = n_distinct(draft_year),
    total_luckpos = sum(luck_pick, na.rm = TRUE),
    avg_luckpos   = mean(luck_pick, na.rm = TRUE)
  ) %>%
  arrange(desc(total_luckpos))
  
print(team_luck_pick, n=22)

df_luck2 %>% filter(team_lottery=="Indiana Fever") %>% select(team_lottery, draft_year, original_order, pick, luck_pick)%>% arrange(desc(draft_year))


df_luck2 %>% filter(team_lottery=="Phoenix Mercury") %>% select(team_lottery, draft_year, original_order, pick, luck_pick) %>% arrange(desc(draft_year))

library(ggplot2)

team_luck_pick %>%
  mutate(
    team_lottery = reorder(team_lottery, total_luckpos),
    luck_sign = if_else(total_luckpos >= 0, "Moved up overall", "Moved down overall")
  ) %>%
  ggplot(aes(x = team_lottery, y = total_luckpos, fill = luck_sign)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "black") +
  geom_text(
    aes(
      label = paste0("(", n_lotteries, ")"),
      hjust = if_else(total_luckpos >= 0, -0.1, 1.1)
    ),
    size = 3
  ) +
  coord_flip() +
  scale_y_continuous(
    breaks = function(x) seq(from = floor(min(x)), to = ceiling(max(x)), by = 1)
  ) +
  scale_fill_manual(values = c("Moved up overall" = "#F57B20",
                               "Moved down overall" = "#4C4C4D")) +
  labs(
    x = NULL,
    y = "Total positional luck (sum of original_order − pick)",
    fill = NULL,
    title = "WNBA Draft Lottery: Overall Positional Luck by Team",
    subtitle = "Bar labels show number of lottery appearances"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
