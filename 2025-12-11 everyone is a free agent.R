library(dplyr)
library(stringr)
library(ggplot2)
library(wehoop)
library(lubridate)
library(forcats)

df <- read.csv("/Users/maddy/Desktop/wehoop/wnba_contracts26.csv")


df_plot <- df |>
  mutate(
    # clean up type labels if you want
    type = case_when(
      type %in% c("rookie") ~ "Rookie",
      type %in% c("reserved") ~ "Reserved",
      type %in% c("RFA") ~ "RFA",
      type %in% c("UFA") ~ "UFA",
      type %in% c("CLUB") ~ "Reserved",a
      type %in% c("veteran") ~ "Veteran",
      TRUE ~ type
    ),
    type = factor(type),
    team = factor(team)
  ) |>
  count(team, type, name = "n")   # count rows per team/type

df_plot <- df_plot |>
  mutate(
    type = factor(
      type,
      levels = c("Reserved", "RFA", "UFA","Rookie", "Veteran")
    )
  )

df_plot <- df_plot |>
  group_by(team) |>
  mutate(
    ufa_rfa_total = sum(dplyr::if_else(type %in% c("Veteran", "Rookie"), n, 0L))
  ) |>
  ungroup() |>
  mutate(
    team = fct_reorder(team, ufa_rfa_total, .desc = TRUE)
  )

contract_cols <- c(
  "UFA"      = "#d73027",  # strong red
  "RFA"      = "#fc8d59",  # light red / orange
  "Reserved" = "#fdae61",  # orangey red
  "Rookie"   = "#4575b4",  # strong blue
  "Veteran"  = "#91bfdb"   # light blue
)

ggplot(df_plot, aes(x = team, y = n, fill = type)) +
  geom_col(position = "stack") +
  labs(
    x = "Team",
    y = "Number of players",
    fill = "Contract status",
    title = "WNBA offseason contract statuses by 2025 team"
  ) +
  scale_fill_manual(values = contract_cols) +
  scale_y_continuous(breaks = seq(0, 15, 1)) +
  theme_minimal() +
  theme(
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
