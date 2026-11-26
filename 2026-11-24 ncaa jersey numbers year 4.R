# charts for my ncaa jersey numbers year 4 video (2026-27 season)
#
# data: wehoop wbb player box, 2023-24 through 2026-27 (season = end year, so 2024-2027).
# the 6-9 digit rule changed for the 2023-24 season.
#
# who counts:
#   - players on D1 teams only. wehoop has no division column, so a D1 team = a team with
#     15+ games in a full season (360-363 teams a year). 2026-27 isn't done yet, so it
#     uses the 2025-26 D1 list.
#   - players who actually played (did_not_play == FALSE). rostered-but-DNP doesn't count.
#   - my old videos counted everyone in a wehoop box, including D2/NAIA opponents. that's
#     why they said 80 and 373 and this file says 58 and 290. the "old way" tibble at the
#     bottom reproduces those numbers.

pacman::p_load(wehoop, dplyr, tidyr, ggplot2)

pb <- wehoop::load_wbb_player_box(seasons = 2024:2027)

NEW    <- "orange"     # numbers with a 6, 7, 8 or 9 (same colors as past years)
OLD    <- "skyblue"    # numbers that were always legal
NEVER  <- "#E6E6E3"    # never worn
INK    <- "#15171D"
GRAY   <- "#8A8A86"
CREDIT <- "D1 players who played in a game  ·  data: wehoop  |  chart: @wnbadata"

season_label <- function(s) paste0(s - 1, "-", substr(s, 3, 4))

base_theme <- function() {
  theme_minimal(base_size = 15) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_text(face = "bold", colour = INK, size = 19),
      plot.subtitle = element_text(colour = GRAY, size = 13, margin = margin(b = 14)),
      plot.caption  = element_text(colour = GRAY, size = 11, hjust = 0, margin = margin(t = 14)),
      axis.text     = element_text(colour = INK, size = 13),
      panel.grid.minor = element_blank(),
      plot.margin   = margin(18, 18, 14, 10)
    )
}

# ---- D1 players who played -------------------------------------------------------------

d1_teams <- pb %>%
  filter(season < 2027) %>%
  distinct(season, game_id, team_id) %>%
  count(season, team_id) %>%
  filter(n >= 15) %>%
  select(season, team_id)
d1_teams <- bind_rows(d1_teams, d1_teams %>% filter(season == 2026) %>% mutate(season = 2027L))

players <- pb %>%
  filter(!did_not_play) %>%
  semi_join(d1_teams, by = c("season", "team_id")) %>%
  filter(!is.na(athlete_jersey), athlete_jersey != "") %>%
  distinct(season, athlete_id, athlete_display_name, team_id, team_location, athlete_jersey) %>%
  mutate(new_digit = grepl("[6-9]", athlete_jersey),
         num = suppressWarnings(as.integer(athlete_jersey)))

this_season <- players %>% filter(season == 2027)
as_of <- max(pb$game_date[pb$season == 2027])

# printable checks
by_season <- players %>%
  group_by(season) %>%
  summarise(players   = n_distinct(athlete_id),
            new_digit = n_distinct(athlete_id[new_digit]),
            single    = n_distinct(athlete_id[athlete_jersey %in% c("6", "7", "8", "9")]),
            over_55   = n_distinct(athlete_id[num > 55 & athlete_jersey != "00"]),
            .groups = "drop") %>%
  mutate(pct_new = round(100 * new_digit / players, 1))

same_date_last_year <- pb %>%   # last season through the same date
  filter(season == 2026, game_date <= as_of - 365, !did_not_play) %>%
  semi_join(d1_teams, by = c("season", "team_id")) %>%
  summarise(new_digit = n_distinct(athlete_id[grepl("[6-9]", athlete_jersey)]))

old_way <- pb %>%   # how the 2024 and 2025 videos counted: anyone in a box, any team
  group_by(season) %>%
  summarise(new_digit = n_distinct(athlete_id[grepl("[6-9]", athlete_jersey)]), .groups = "drop")

# ---- 1. hook: all 100 numbers, 38 never worn ------------------------------------------

worn <- players %>% filter(!is.na(num), athlete_jersey != "00") %>% distinct(num) %>% pull(num)

grid <- tibble(num = 0:99) %>%
  mutate(col = num %% 10, row = num %/% 10,
         status = case_when(!num %in% worn ~ "never worn",
                            grepl("[6-9]", num) ~ "new",
                            TRUE ~ "always legal"))

p_grid <- ggplot(grid, aes(col, row)) +
  geom_tile(aes(fill = status), colour = "white", linewidth = 1.2) +
  geom_text(aes(label = num, colour = status == "never worn"), size = 5.5, fontface = "bold") +
  scale_fill_manual(values = c("always legal" = OLD, "new" = NEW, "never worn" = NEVER)) +
  scale_colour_manual(values = c("TRUE" = "#B5B5B0", "FALSE" = INK), guide = "none") +
  scale_y_reverse() +
  coord_equal() +
  labs(title = paste0(sum(grid$status == "never worn"), " numbers nobody has worn yet"),
       subtitle = "every number worn in a D1 game since the rule changed in 2023-24",
       caption = CREDIT, fill = NULL) +
  base_theme() +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank(), legend.position = "top")

# ---- 2. growth by season ---------------------------------------------------------------

growth <- by_season %>%
  mutate(label = ifelse(season == 2027, paste0(season_label(season), "\n(so far)"), season_label(season)))

p_growth <- ggplot(growth, aes(factor(label, levels = label), new_digit)) +
  geom_col(fill = NEW, width = 0.7) +
  geom_text(aes(label = new_digit), vjust = -0.5, size = 7, fontface = "bold", colour = INK) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "D1 players wearing a 6, 7, 8 or 9",
       subtitle = paste0("2026-27 through ", format(as_of, "%b %d")),
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(axis.text.y = element_blank(), panel.grid = element_blank())

# ---- 3. this season's numbers, 0-99 ----------------------------------------------------

freq <- tibble(num = 0:99) %>%
  left_join(this_season %>% filter(athlete_jersey != "00") %>% count(num), by = "num") %>%
  mutate(n = coalesce(n, 0L), new = grepl("[6-9]", num))

p_freq <- ggplot(freq, aes(factor(num), n, fill = new)) +
  geom_col(width = 0.85) +
  scale_fill_manual(values = c("FALSE" = OLD, "TRUE" = NEW), guide = "none") +
  scale_x_discrete(breaks = seq(0, 99, by = 5)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = paste0("jersey numbers in D1 this season"),
       subtitle = "orange = has a 6, 7, 8 or 9",
       caption = CREDIT, x = "jersey number", y = "players") +
  base_theme() +
  theme(panel.grid.major.x = element_blank(),
        axis.title = element_text(colour = GRAY, size = 12))

# ---- 3b. the grid as a heat map: one color, darker = more players this season ---------
# square root scale so the big numbers with 1-7 players still show up next to 0-5's ~150-200

# numbers nobody wears this season are plain white (NA fill), with a light gray outline so the grid still reads
heat <- grid %>% select(num, col, row) %>% left_join(freq %>% select(num, n), by = "num") %>%
  mutate(fill_n = ifelse(n == 0, NA, n))

p_grid_heat <- ggplot(heat, aes(col, row)) +
  geom_tile(aes(fill = fill_n), colour = "#E2E2DE", linewidth = 0.8) +
  geom_text(aes(label = num, colour = n > 60), size = 5.5, fontface = "bold") +
  scale_fill_gradient(low = "#FFE9D4", high = "#C2410C", trans = "sqrt", na.value = "white",
                      breaks = c(1, 10, 50, 100, 200), name = "players") +
  scale_colour_manual(values = c("TRUE" = "white", "FALSE" = INK), guide = "none") +
  scale_y_reverse() +
  coord_equal() +
  labs(title = "how many D1 players wear each number",
       subtitle = paste0("2026-27 through ", format(as_of, "%b %d"), "  ·  darker = more players"),
       caption = CREDIT) +
  base_theme() +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank(), legend.position = "top",
        legend.key.width = unit(2.2, "lines"))

# ---- 4. above 55 (the old max) by season -----------------------------------------------

p_over55 <- ggplot(growth, aes(factor(label, levels = label), over_55)) +
  geom_col(fill = NEW, width = 0.7) +
  geom_text(aes(label = over_55), vjust = -0.5, size = 7, fontface = "bold", colour = INK) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "D1 players wearing a number above 55",
       subtitle = "55 was the highest legal number before 2023-24",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(axis.text.y = element_blank(), panel.grid = element_blank())

# ---- numbers worn in a D1 game before this season --------------------------------------

worn_before <- players %>% filter(season < 2027) %>% distinct(athlete_jersey) %>% pull(athlete_jersey)

# ---- tables: this season's numbers above 55, and brand new numbers ---------------------

over_55_this_season <- this_season %>%
  filter(num > 55) %>%
  distinct(name = athlete_display_name, no = athlete_jersey, school = team_location, num) %>%
  arrange(num, name) %>%
  select(-num)

# a first only counts for numbers with a 6-9 digit, since those were illegal before 2023-24
# (wehoop here only goes back to 2023-24, so it can't vouch for old 0-5 numbers).
# 85 is excluded: melissa guillet wore 85 for mississippi st. in a 2025-26 D1 game that's
# missing from wehoop, so 85 is not a first
first_evers <- this_season %>%
  filter(new_digit, !athlete_jersey %in% c(worn_before, "85")) %>%
  distinct(name = athlete_display_name, no = athlete_jersey, school = team_location, num) %>%
  arrange(num, name) %>%
  select(-num)

# ---- 5. ending: still nobody in 67 or 69 -----------------------------------------------

p_grid_end <- p_grid +
  geom_tile(data = grid %>% filter(num %in% c(67, 69)), width = 1, height = 1, fill = NA, colour = INK, linewidth = 1.6) +
  labs(title = "4 seasons, 0 players in 67 or 69")

# ---- printable checks ------------------------------------------------------------------

by_season
same_date_last_year
old_way
grid %>% filter(status == "never worn") %>% pull(num)
print(over_55_this_season, n = Inf)
print(first_evers, n = Inf)

# ---- chart index -----------------------------------------------------------------------
# p_grid      hook: all 100 numbers, 38 never worn
# p_growth    58 / 290 / 479 / 581: players with a 6-9 digit by season
# p_freq      this season's 0-99 frequency chart, 6-9 numbers in orange
# p_grid_heat the 10x10 grid as a one-color heat map of this season's counts
# p_over55    players above 55 by season
# p_grid_end  ending: grid with 67 and 69 boxed

p_grid
p_growth
p_freq
p_grid_heat
p_over55
p_grid_end
