library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(wehoop)
library(lubridate)
library(ggrepel)

# -------------------------------------------------
# 1. Load data
# -------------------------------------------------
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- load_wnba_player_box(season = 2006:2025)
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp(season = 2006:2025)
})
tictoc::toc()

# -------------------------------------------------
# 2. Unrivaled rosters + name standardization
# -------------------------------------------------
breeze <- c("Aari McDonald", "Kate Martin", "Cameron Brink", "Dominique Malonga", "Rickea Jackson", "Paige Bueckers")
hive   <- c("Monique Billings", "Saniya Rivers", "Natisha Hiedeman", "Ezi Magbegor", "Sonia Citron", "Kelsey Mitchell")
laces  <- c("Jackie Young", "Alyssa Thomas", "Naz Hillmon", "Maddy Siegrist", "Jordin Canada", "Brittney Sykes")
owls   <- c("Napheesa Collier", "Aaliyah Edwards", "Rebecca Allen", "Skylar Diggins", "Marina Mabrey", "Rachel Banham")
mist   <- c("Breanna Stewart", "Arike Ogunbowale", "Veronica Burton", "Alanna Smith", "Li Yueru", "Allisha Gray")
phantom<- c("Tiffany Hayes", "Natasha Cloud", "Dana Evans", "Aliyah Boston", "Kiki Iriafen", "Kelsey Plum")
rose   <- c("Shakira Austin", "Lexie Hull", "Sug Sutton", "Azura Stevens", "Chelsea Gray", "Kahleah Copper")
vinyl  <- c("Brittney Griner", "Rae Burrell", "Erica Wheeler", "Dearica Hamby", "Rhyne Howard", "Courtney Williams")

unrivaled_players <- c(breeze, hive, laces, owls, mist, phantom, rose, vinyl)

standardize_name <- function(name) {
  name %>%
    str_trim() %>%
    str_replace_all("-Smith", "") %>%   # Skylar Diggins-Smith -> Skylar Diggins
    str_replace_all("-", " ") %>%
    str_squish()
}

wnba_player_box_clean <- wnba_player_box %>%
  mutate(athlete_display_name = standardize_name(athlete_display_name))

unrivaled_players_clean <- standardize_name(unrivaled_players)

# -------------------------------------------------
# 3. Attach names to PBP and filter to Unrivaled
# -------------------------------------------------
player_lookup <- wnba_player_box_clean %>%
  distinct(athlete_id, athlete_display_name)

wnba_pbp_with_player <- wnba_pbp %>%
  left_join(player_lookup, by = c("athlete_id_1" = "athlete_id")) %>%
  mutate(athlete_display_name = standardize_name(athlete_display_name))

wnba_pbp_unrivaled <- wnba_pbp_with_player %>%
  filter(athlete_display_name %in% unrivaled_players_clean)

# -------------------------------------------------
# 4. Free throw subset + make/miss + category
# -------------------------------------------------
ft <- wnba_pbp_unrivaled %>%
  filter(grepl("Free Throw", type_text) & type_text != "Free Throw") %>%
  mutate(
    make = case_when(
      grepl("makes", text, ignore.case = TRUE) ~ 1,
      grepl("misses", text, ignore.case = TRUE) ~ 0,
      TRUE ~ NA_real_
    ),
    ft_category = case_when(
      grepl("1 of 2", type_text) ~ "First of 2",
      grepl("2 of 2", type_text) ~ "Second of 2",
      grepl("1 of 3", type_text) ~ "First of 3",
      grepl("2 of 3", type_text) ~ "Second of 3",
      grepl("3 of 3", type_text) ~ "Third of 3",
      grepl("1 of 1", type_text) ~ "First of 1",
      grepl("Technical", type_text) ~ "First of 1",
      TRUE ~ "Single"
    )
  )

# -------------------------------------------------
# 5. FT% by category (all attempts, Unrivaled players)
# -------------------------------------------------
ft_percentages_unrivaled <- ft %>%
  group_by(ft_category) %>%
  summarize(
    total_attempts = n(),
    total_makes = sum(make, na.rm = TRUE),
    percentage_made = mean(make, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(percentage_made))

print(ft_percentages_unrivaled)

ft_percentages_unrivaled_by_player <- ft %>%
  group_by(athlete_display_name, ft_category) %>%
  summarize(
    total_attempts = n(),
    total_makes = sum(make, na.rm = TRUE),
    percentage_made = mean(make, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(athlete_display_name, ft_category)

print(ft_percentages_unrivaled_by_player)

ft_summary <- tribble(
  ~ft_category,   ~total_attempts, ~total_makes,
  "Second of 3",        303,          252,
  "Third of 3",         303,          250,
  "Second of 2",      11821,         9597,
  "First of 3",         303,          242,
  "First of 1",        3961,         3122,
  "First of 2",       11826,         9125
)


# -------------------------------------------------
# 6. “First FT of trip” logic
# -------------------------------------------------
ft_trips <- ft %>%
  arrange(game_id, period_number, sequence_number) %>%
  group_by(game_id, period_number, athlete_id_1) %>%
  mutate(
    is_ft_for_same_player = lag(grepl("Free Throw", type_text)) &
                            lag(athlete_id_1) == athlete_id_1,
    is_new_trip = if_else(is.na(is_ft_for_same_player) | !is_ft_for_same_player, 1L, 0L),
    trip_id = cumsum(is_new_trip)
  ) %>%
  ungroup()

first_ft_each_trip <- ft_trips %>%
  group_by(game_id, period_number, athlete_id_1, trip_id) %>%
  slice_min(sequence_number, n = 1, with_ties = FALSE) %>%
  ungroup()

# -------------------------------------------------
# 7. First‑FT% vs overall FT% per player
# -------------------------------------------------
first_ft_by_player <- first_ft_each_trip %>%
  group_by(athlete_display_name) %>%
  summarize(
    trips = n(),
    makes_first_ft = sum(make, na.rm = TRUE),
    pct_first_ft = 100 * mean(make, na.rm = TRUE),
    .groups = "drop"
  )

overall_ft_by_player <- ft %>% 
  group_by(athlete_display_name) %>%
  summarize(
    total_fta = n(),
    total_ftm = sum(make, na.rm = TRUE),
    pct_overall = 100 * mean(make, na.rm = TRUE),
    .groups = "drop"
  )

ft_compare <- first_ft_by_player %>%
  inner_join(overall_ft_by_player, by = "athlete_display_name") %>%
  mutate(
    diff_first_minus_overall = pct_first_ft - pct_overall
  )

overperform_first_ft <- ft_compare %>%
  filter(
    trips >= 20,
    diff_first_minus_overall > 1  
  ) %>%
  arrange(desc(diff_first_minus_overall))

print(overperform_first_ft)

################ 
#plots
ft_percentages_unrivaled_plot <- ft_percentages_unrivaled %>%
  mutate(
    ft_category = factor(
      ft_category,
      levels = c("First of 2", "First of 1", "First of 3",
                 "Second of 2", "Third of 3", "Second of 3")
    )
  )

ggplot(ft_percentages_unrivaled_plot,
       aes(x = ft_category, y = percentage_made)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", percentage_made)),
            vjust = -0.4, size = 3.5) +
  labs(
    title = "WNBA Free Throw Percentage by Attempt in Trip",
    subtitle = "Unrivaled players, WNBA data",
    x = "Attempt within trip",
    y = "Free throw percentage"
  ) +
  ylim(0, 90) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(ft_percentages_unrivaled_plot,
  aes(x = ft_label, y = percentage_made, fill = is_first)) +
geom_col(width = 0.7) +
geom_text(aes(label = sprintf("%.1f%%", percentage_made)),
       vjust = -0.4, size = 4) +
scale_fill_manual(values = c("First attempt" = "tomato", "Later attempt" = "steelblue")) +
labs(
title = "WNBA FT% by Attempt in Trip",
subtitle = "Unrivaled players, WNBA data",
x = "Attempt within trip",
y = "FT%",
fill = ""
) +
scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
theme_minimal(base_size = 14) +
theme(axis.text.x = element_text(angle = 0))






first_ft_ranking <- first_ft_by_player %>%
  arrange(desc(pct_first_ft))

print(first_ft_ranking)




label_players <- c("Kelsey Plum", "Jackie Young", "Breanna Stewart",
                   "Paige Bueckers", "Li Yueru", "Kate Martin")

ft_compare_plot <- ft_compare %>%
  mutate(
    label = ifelse(athlete_display_name %in% label_players,
                   athlete_display_name, NA)
  )

ggplot(ft_compare_plot,
       aes(x = pct_overall, y = pct_first_ft)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "gray50") +
  geom_point(aes(size = total_fta), alpha = 0.6, color = "steelblue") +
  geom_text_repel(
    aes(label = label),
    na.rm = TRUE,
    size = 3.5
  ) +
  scale_size_continuous(name = "Total FTA", range = c(2, 6)) +
  labs(
    title = "WNBA FT% vs First-Trip FT% (Unrivaled Players)",
    x = "Overall FT% (all attempts)",
    y = "First-FT% (first attempt of each trip)"
  ) +
  theme_minimal()
