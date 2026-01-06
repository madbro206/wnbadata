library(dplyr)
library(stringr)
library(ggplot2)
library(wehoop)

df <- read.csv("/Users/maddy/Desktop/wbb_rosters_2025_26.csv")

colnames(df)

unique(df$position)

unique(df$position_clean)

df %>%filter(position=="BP") %>% select(team, name, position, position_clean)
df %>%filter(position=="Foward") %>% select(team, name, position, position_clean)


#basic breakdown of all position values
position_breakdown <- df %>%
  filter(!is.na(position) & position != "") %>%
  count(position, sort = TRUE) %>%
  mutate(
    percentage = n / sum(n) * 100,
    pct_label = paste0(round(percentage, 1), "%")
  )


#create categories for broader analysis
position_categories <- df %>%
  filter(!is.na(position) & position != "") %>%
  mutate(
    position_type = case_when(
      # Traditional single letter
      position %in% c("G", "F", "C") ~ "Traditional (G/F/C)",
      
      # Traditional combo
      position %in% c("G/F", "F/C", "F/G", "C/F") ~ "Traditional Combo",
      
      # Specific positions (PG, SG, SF, PF, CG)
      position %in% c("PG", "SG", "SF", "PF", "CG", "PG/SG", "SF/PF", "PF/C", "G/SF") ~ "Specific Position",
      
      # Full word versions
      position %in% c("Guard", "Forward", "Center", "Guard/Forward", 
                      "Forward/Center", "Point Guard", "Small Forward",
                      "Guard/Small Forward") ~ "Full Word",
      
      # Letter codes (M/W/Y/S/P system)
      position %in% c("M", "W", "Y", "S", "P", "M/W", "W/Y", "M/F", 
                      "S/Y", "S/W", "Y/W", "W/P", "W/F") ~ "Letter Code System",
      
      # Descriptive
      position %in% c("Post", "Wing", "Shooter") ~ "Descriptive",
      
      # Unknown/Other
      position == "BP" ~ "Unknown Code",
      
      # Typos
      position == "Foward" ~ "Typo",
      
      TRUE ~ "Other"
    )
  ) %>%
  count(position_type, sort = TRUE) %>%
  mutate(
    percentage = n / sum(n) * 100,
    pct_label = paste0(round(percentage, 1), "%")
  )

print(position_categories)

# School-level analysis: which schools use weird systems
weird_positions <- c("M", "W", "Y", "S", "P", "M/W", "W/Y", "M/F", 
                     "S/Y", "S/W", "Y/W", "W/P", "W/F", "Shooter", "BP", "Post")

schools_with_weird <- df %>%
  filter(position %in% weird_positions) %>%
  distinct(team, position) %>%
  group_by(team) %>%
  summarise(
    positions_used = paste(unique(position), collapse = ", "),
    n_weird = n()
  ) %>%
  arrange(desc(n_weird))

print(schools_with_weird)

#percentage teams with nontraditional systems
teams_by_system <- df %>%
  filter(!is.na(position) & position != "") %>%
  mutate(
    is_weird = position %in% weird_positions
  ) %>%
  group_by(team) %>%
  summarise(
    uses_weird = any(is_weird)
  ) %>%
  count(uses_weird) %>%
  mutate(percentage = n / sum(n) * 100)

#print(teams_by_system)


######################################################
#viz: Traditional position distribution
position_dist <- position_breakdown %>%
  filter(position %in% c("G", "F", "C")) 

viz1 <- ggplot(position_dist, aes(x = position, y = percentage, fill = position)) +
  geom_col() +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -.2, size = 6, fontface = "bold") +
  labs(title = "NCAAW Players by Position",
       subtitle = "(includes players labeled EXACTLY G, F, or C)",
       y = "Percentage of Players",
       x = "") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 18))

viz1

#ggsave("position_distribution.png", viz1, width = 8, height = 6)