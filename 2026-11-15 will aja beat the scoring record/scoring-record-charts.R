# charts and numbers for "will aja beat the scoring record" (idea page 11/13/2026)
# sequel to the 5/11/24 "who will break DT's record" video, which drew stewie's career pace out to DT's
# game count ("future stew stew"). code for that one: ~/Desktop/wehoop/2024-5-10 dt scoring record
#
# run the script and the 2 tables in the list at the bottom print (t_when_simple and t_on_pace).
# the other t_ tables feed a chart. type any name in the console to see it.
#
# data (all from her hoop stats, regular season only, exported 11/13-14/2026):
#   data/career-points.csv   every WNBA player's career games played and points, 1997-2026
#   data/season-points.csv   the same by season, plus birthdate (missing for a lot of older players, see below)
#   data/game-logs-5000.csv  game-by-game points for everyone with 5,000+ career points
# player_key is just a number per player, so 2 players with the same name stay separate.
# games = games actually played, not games on the roster.
#
# the old chart used wehoop since 2003. wehoop matches her hoop stats exactly for aja (308 games, 6,792)
# but is missing 9 of DT's games (557 games, 10,525), so this version uses her hoop stats for everything.
#
# projections use the new schedule: 50 games in 2027 and 2028, 52 (the CBA max) from 2029 on.
# the 2027 schedule isn't out yet, so treat those as best case.

library(tidyverse)

DATA_DIR <- "~/Desktop/wnbadata/2026-11-15 will aja beat the scoring record/data"

AJA    <- "#C8102E"
STEWIE <- "#2E9C84"
DT     <- "#E56020"
GRAY   <- "#8A8A86"
LIGHT  <- "#D9D9D6"
INK    <- "#010101"

RECORD  <- 10646   # DT's regular season record
TO_PASS <- RECORD + 1
DT_GAMES <- 565    # basketball reference, see the fix below
DT_PPG  <- RECORD / DT_GAMES   # 18.8, DT's career points per game

careers <- read_csv(file.path(DATA_DIR, "career-points.csv"), show_col_types = FALSE)
seasons <- read_csv(file.path(DATA_DIR, "season-points.csv"), show_col_types = FALSE)
logs    <- read_csv(file.path(DATA_DIR, "game-logs-5000.csv"), show_col_types = FALSE)

stopifnot(nrow(careers) == 1219, n_distinct(logs$player) == 33,
          sum(seasons$points) == sum(careers$points), sum(seasons$games) == sum(careers$games))

# DT didn't play 7/7/2021 at las vegas (no minutes, no stats), but that game is in the export.
# basketball reference has 565 games, so drop it
PHANTOM <- logs$player == "Diana Taurasi" & as.Date(logs$game_date) == as.Date("2021-07-07")
stopifnot(sum(PHANTOM) == 1, logs$points[PHANTOM] == 0)
logs    <- logs[!PHANTOM, ]
careers <- careers |> mutate(games = if_else(player == "Diana Taurasi", games - 1, games))
seasons <- seasons |> mutate(games = if_else(player == "Diana Taurasi" & season == 2021, games - 1, games))

key <- careers |>
  filter(player %in% c("Diana Taurasi", "A'ja Wilson", "Breanna Stewart")) |>
  mutate(ppg = points / games)

stopifnot(nrow(key) == 3,
          key$points[key$player == "Diana Taurasi"] == RECORD, key$games[key$player == "Diana Taurasi"] == DT_GAMES,
          key$points[key$player == "A'ja Wilson"] == 6792, key$games[key$player == "A'ja Wilson"] == 308,
          key$points[key$player == "Breanna Stewart"] == 6860, key$games[key$player == "Breanna Stewart"] == 334)

# ---- the leaderboard ----

# "aja is 11th all time, stewie is 8th"
t_leaders <- careers |>
  arrange(desc(points)) |>
  mutate(rank = row_number(), ppg = round(points / games, 1)) |>
  select(rank, player, first_year, last_year, games, points, ppg) |>
  head(15)

# "fastest ever to 6,792 points: aja 308 games, stewart 332, DT 337"
t_fastest_to_6792 <- logs |>
  arrange(player, game_date) |>
  group_by(player) |>
  mutate(game = row_number(), total = cumsum(points)) |>
  filter(total >= 6792) |>
  slice_min(game) |>
  ungroup() |>
  transmute(player, games_to_6792 = game, date_reached = game_date) |>
  arrange(games_to_6792)

# ---- ages ----

# the export has no birthdate for a lot of players who retired before about 2016, and the age tables
# below would silently skip them. these are their birth years from wikipedia, for everyone missing one
# with 3,855+ career points. only taj's is double checked (wikipedia and wnba.com: 10/20/1970), because
# hers is the one that matters: born a year later, she'd drop under 3,855 after 30. for everyone else,
# a year off either way doesn't change any table's top 5.
MISSING_BIRTH_YEARS <- tribble(
  ~player,                    ~birth_year,
  "Alana Beard",                     1982,
  "Asjha Jones",                     1980,
  "Becky Hammon",                    1977,
  "Cappie Pondexter",                1983,
  "Chamique Holdsclaw",              1977,
  "Deanna Nolan",                    1979,
  "DeLisha Milton-Jones",            1974,
  "Jia Perkins",                     1982,
  "Katie Douglas",                   1979,
  "Katie Smith",                     1974,
  "Lauren Jackson",                  1981,
  "Lindsay Whalen",                  1982,
  "Lisa Leslie",                     1972,
  "Monique Currie",                  1983,
  "Nykesha Sales",                   1976,
  "Sheryl Swoopes",                  1971,
  "Sophia Young-Malcolm",            1983,
  "Swin Cash",                       1979,
  "Taj McWilliams-Franklin",         1970,
  "Tamika Catchings",                1979,
  "Tangela Smith",                   1977,
  "Tina Thompson",                   1975,
  "Vickie Johnson",                  1972,
  "Yolanda Griffith",                1970
)

# season age = season minus birth year, so 2026 is aja's age 30 season and 2012 is DT's
ages <- seasons |>
  left_join(MISSING_BIRTH_YEARS, by = "player") |>
  mutate(birth_year = coalesce(year(birthdate), birth_year),
         age = season - birth_year)

# everyone with 3,855+ career points has an age now
stopifnot(ages |> group_by(player_key) |> filter(sum(points) >= 3855) |> filter(is.na(age)) |> nrow() == 0)

by_age <- ages |>
  filter(!is.na(age)) |>
  group_by(player_key, player) |>
  summarise(after_30  = sum(points[age >= 31]),
            last_season = max(season), last_age = max(age), .groups = "drop") |>
  select(-player_key)

# "3 players have scored 3,855+ after age 30: DT, bonner (still playing) and taj mcwilliams-franklin"
t_after_age_30 <- by_age |> arrange(desc(after_30)) |> select(player, after_30, last_season, last_age) |> head(10)

stopifnot(sum(t_after_age_30$after_30 >= 3855) == 3,
          t_after_age_30$player[1:3] == c("Diana Taurasi", "DeWanna Bonner", "Taj McWilliams-Franklin"))

# ---- how many games it takes at career pace ----

# career game number where each player passes DT if she keeps her career points per game
pace <- key |>
  filter(player != "Diana Taurasi") |>
  mutate(points_needed = TO_PASS - points,
         games_needed  = ceiling(points_needed / ppg),
         passes_at_game = games + games_needed)

stopifnot(pace$passes_at_game[pace$player == "A'ja Wilson"] == 483,
          pace$passes_at_game[pace$player == "Breanna Stewart"] == 519)

# "at career ppg aja passes DT in her 483rd game, stewart in her 519th. DT took 565"
t_games_to_pass <- bind_rows(
  tibble(who = "DT (actual)", games_so_far = DT_GAMES, points_so_far = RECORD, ppg = DT_PPG, passes_at_game = DT_GAMES),
  pace |> transmute(who = if_else(player == "A'ja Wilson", "A'ja (now)", "Stewie (now)"), games_so_far = games,
                    points_so_far = points, ppg, passes_at_game)
) |>
  mutate(ppg = round(ppg, 1), games_left = passes_at_game - games_so_far)

# ---- when, on the calendar ----

SCHEDULE <- tibble(season = 2027:2034, team_games = c(50, 50, rep(52, 6)))

# season and team game where she passes DT, scoring `rate` points per team game
when_passes <- function(points_needed, rate) {
  SCHEDULE |>
    mutate(before = lag(cumsum(team_games), default = 0),
           need_here = points_needed - before * rate) |>
    filter(need_here <= team_games * rate) |>
    slice(1) |>
    transmute(season, game = ceiling(need_here / rate))
}

# 2025 and 2026 were 44-game seasons. "per team game" counts missed games against her
rates <- logs |>
  filter(player %in% c("A'ja Wilson", "Breanna Stewart"), season >= 2025) |>
  group_by(player) |>
  summarise(ppg_2026      = sum(points[season == 2026]) / sum(season == 2026),
            per_team_2026 = sum(points[season == 2026]) / 44,
            per_team_2025_26 = sum(points) / 88) |>
  left_join(pace |> select(player, career_ppg = ppg, points_needed), by = "player")

births <- c("A'ja Wilson" = "1996-08-08", "Breanna Stewart" = "1994-08-27")

# rough date of a team game: seasons run about may 8 to mid september (~130 days), games spread evenly
game_date_est <- function(season, game, team_games) {
  as.Date(paste0(season, "-05-08")) + round(130 * game / team_games)
}

# "she's 33 in every scenario" (aja), "35 or 36" (stewie)
t_when <- rates |>
  pivot_longer(c(ppg_2026, per_team_2026, per_team_2025_26, career_ppg), names_to = "scenario", values_to = "rate") |>
  mutate(scenario = recode(scenario,
                           ppg_2026 = "2026 points per game, plays every game",
                           per_team_2026 = "2026 points per team game (missed games count)",
                           per_team_2025_26 = "2025-26 points per team game (missed games count)",
                           career_ppg = "career points per game, plays every game"),
         when = map2(points_needed, rate, when_passes)) |>
  unnest(when) |>
  left_join(SCHEDULE, by = "season") |>
  # age on the (estimated) date of that game, since both birthdays are in august
  mutate(rate = round(rate, 1),
         about = game_date_est(season, game, team_games),
         age = floor(time_length(interval(as.Date(births[player]), about), "years"))) |>
  select(player, scenario, rate, points_needed, season, game, about, age) |>
  arrange(player, season, game)

# short version for reading on screen: who, which pace, and when she passes DT
t_when_simple <- t_when |>
  transmute(player = if_else(player == "A'ja Wilson", "A'ja", "Stewie"),
            pace = recode(scenario,
                          "2026 points per game, plays every game" = "2026 pace, every game",
                          "career points per game, plays every game" = "career avg, every game",
                          "2026 points per team game (missed games count)" = "2026 pace, missing games",
                          "2025-26 points per team game (missed games count)" = "2025-26 pace, missing games"),
            ppg = rate,
            passes_DT = paste("game", game, "of", season),
            age)
t_when_simple |> print()

# games per team in each season, used to spot short seasons in p_arc
LEAGUE_GAMES <- tibble(season = 1997:2026,   # games per team, from her hoop stats team schedules
                       team_games = c(28, 30, 32, 32, 32, 32, rep(34, 17), 22, 32, 36, 40, 40, 44, 44))

# ---- charts ----

theme_wnbadata <- function() {
  theme_minimal(base_size = 15) +
    theme(plot.title.position = "plot", plot.caption.position = "plot",
          plot.title    = element_text(face = "bold", color = INK, size = 19),
          plot.subtitle = element_text(color = GRAY, size = 12, margin = margin(b = 14)),
          plot.caption  = element_text(color = GRAY, size = 11, hjust = 0, margin = margin(t = 14)),
          axis.text     = element_text(color = INK, size = 12),
          axis.title    = element_text(color = GRAY, size = 12),
          panel.grid.minor = element_blank(),
          plot.margin   = margin(18, 18, 14, 10))
}
CAPTION <- "regular season only  |  data: Her Hoop Stats  |  chart: @wnbadata"
comma <- scales::label_comma()

# 1. the old chart, redone: games played vs total points, with each player's career pace drawn out to the record

pace_lines <- pace |>
  transmute(player, x0 = 0, y0 = 0, x1 = passes_at_game, y1 = TO_PASS,
            label = paste0("future ", if_else(player == "A'ja Wilson", "A'ja", "Stewie"), "\ngame ", passes_at_game))

# label spots, nudged by hand so the names near the top and the middle don't collide
star_labels <- pace_lines |>
  mutate(x = if_else(player == "A'ja Wilson", x1 - 10, x1 + 4),
         y = if_else(player == "A'ja Wilson", y1 + 250, y1 - 300),
         hjust = if_else(player == "A'ja Wilson", 1, 0),
         vjust = if_else(player == "A'ja Wilson", 0, 1))

name_labels <- key |>
  mutate(x = case_when(player == "A'ja Wilson" ~ games - 10, player == "Breanna Stewart" ~ games + 10,
                       TRUE ~ games),
         y = case_when(player == "Diana Taurasi" ~ points + 250, TRUE ~ points),
         hjust = case_when(player == "A'ja Wilson" ~ 1, player == "Breanna Stewart" ~ 0, TRUE ~ 0.5),
         vjust = case_when(player == "Diana Taurasi" ~ 0, player == "A'ja Wilson" ~ -0.4, TRUE ~ 1.2))

p_scatter <- ggplot(careers |> mutate(ppg = points / games), aes(games, points, color = ppg)) +
  geom_hline(yintercept = RECORD, linetype = "dotted", color = GRAY) +
  geom_segment(data = pace_lines, aes(x = x0, y = y0, xend = x1, yend = y1), inherit.aes = FALSE,
               linetype = "dashed", color = INK) +
  geom_point(alpha = 0.5, size = 1.3) +
  geom_point(data = pace_lines, aes(x1, y1), inherit.aes = FALSE, shape = 8, size = 4, stroke = 1.2,
             color = "#F2B705") +
  geom_text(data = star_labels, aes(x, y, label = label, hjust = hjust, vjust = vjust), inherit.aes = FALSE,
            size = 3.5, color = INK, lineheight = 0.9) +
  geom_point(data = key, color = INK, size = 2) +
  geom_text(data = name_labels, aes(x, y, label = player, hjust = hjust, vjust = vjust), inherit.aes = FALSE,
            color = INK, size = 3.5) +
  scale_color_gradient(name = "Points per game") +
  scale_x_continuous(breaks = seq(0, 600, by = 100), limits = c(0, 640)) +
  scale_y_continuous(labels = comma, limits = c(0, 11200)) +
  labs(title = "Games Played vs. Total Points",
       subtitle = "for WNBA players, 1997-2026. dashed lines: career points per game, drawn out to the record",
       x = "Games Played", y = "Points Scored", caption = CAPTION) +
  theme_wnbadata()

# 2. career points by career game, for p_aja_stewie

race <- logs |>
  arrange(player, game_date) |>
  group_by(player) |>
  mutate(game = row_number(), total = cumsum(points)) |>
  ungroup()

# 2b. "stewart has 68 more career points than aja, but aja averages more per game, 22.1 to 20.5"
# just the two of them. dotted line at game 308, where aja is now: stewie had 6,297 at that point

aja_stewie <- race |> filter(player %in% c("A'ja Wilson", "Breanna Stewart"))

at_308 <- aja_stewie |> filter(game == 308)
stopifnot(at_308$total[at_308$player == "A'ja Wilson"] == 6792,
          at_308$total[at_308$player == "Breanna Stewart"] == 6297)

aja_stewie_ends <- aja_stewie |>
  group_by(player) |>
  slice_max(game) |>
  ungroup() |>
  mutate(name = if_else(player == "A'ja Wilson", "A'ja", "Stewie"),
         label = paste0(name, ": ", comma(total), "\nin ", game, " games"),
         # aja above and to the left of her end, stewie below and to the right of hers
         x = if_else(player == "A'ja Wilson", game - 6, game + 6),
         y = if_else(player == "A'ja Wilson", total + 250, total - 250),
         hjust = if_else(player == "A'ja Wilson", 1, 0),
         vjust = if_else(player == "A'ja Wilson", 0, 1))

# stewie reached 6,792 in game 332 (t_fastest_to_6792), 24 games after aja
stewie_to_6792 <- t_fastest_to_6792$games_to_6792[t_fastest_to_6792$player == "Breanna Stewart"]
stopifnot(stewie_to_6792 - 308 == 24)

p_aja_stewie <- ggplot(aja_stewie, aes(game, total, color = player)) +
  geom_vline(xintercept = 308, linetype = "dotted", color = GRAY) +
  geom_line(linewidth = 1.4) +
  geom_point(data = at_308, size = 3) +
  geom_point(data = aja_stewie_ends, size = 3) +
  geom_text(data = aja_stewie_ends, aes(x, y, label = label, hjust = hjust, vjust = vjust), size = 4.2,
            fontface = "bold", lineheight = 0.9, show.legend = FALSE) +
  annotate("text", x = 314, y = 5650, label = paste0("at game 308,\nStewie had ", comma(6297)),
           hjust = 0, vjust = 1, size = 3.9, color = STEWIE, lineheight = 0.9) +
  scale_color_manual(values = c("A'ja Wilson" = AJA, "Breanna Stewart" = STEWIE), guide = "none") +
  scale_x_continuous(breaks = seq(0, 300, by = 100), limits = c(0, 400)) +
  scale_y_continuous(labels = comma, limits = c(0, 7800)) +
  labs(title = "Career points by career game",
       subtitle = "Stewie has 68 more points, but A'ja got to 6,792 in 24 fewer games",
       x = "career game", y = NULL, caption = CAPTION) +
  theme_wnbadata()

# 3. games it takes to pass DT

games_bars <- t_games_to_pass |>
  mutate(who  = recode(who, "DT (actual)" = "DT", "Stewie (now)" = "Stewie", "A'ja (now)" = "A'ja"),
         fill = c(DT = DT, Stewie = STEWIE, "A'ja" = AJA)[who],
         who  = factor(who, c("A'ja", "Stewie", "DT")))

p_games_needed <- ggplot(games_bars, aes(passes_at_game, who, fill = fill)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = passes_at_game), hjust = -0.2, size = 5.5, fontface = "bold", color = INK) +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0, 640), expand = c(0, 0)) +
  labs(title = "Games it takes to score 10,647 points",
       subtitle = "A'ja and Stewie at their career points per game. DT's 565 is her whole career",
       x = "career games", y = NULL, caption = CAPTION) +
  theme_wnbadata() +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(face = "bold", size = 14))

# aja vs stewie, points per game by season
t_ppg_by_season <- logs |>
  filter(player %in% c("A'ja Wilson", "Breanna Stewart")) |>
  group_by(player, season) |>
  summarise(games = n(), points = sum(points), ppg = round(points / games, 1), .groups = "drop")

ppg_ends <- t_ppg_by_season |> filter(season == 2026) |>
  mutate(name = if_else(player == "A'ja Wilson", "A'ja", "Stewie"))

# fill missing seasons with NA so stewie's line breaks at 2019 instead of jumping 2018 to 2020
p_ppg <- ggplot(t_ppg_by_season |> complete(player, season = 2016:2026), aes(season, ppg, color = player)) +
  geom_line(linewidth = 1.3, na.rm = TRUE) +
  geom_point(size = 3, na.rm = TRUE) +
  geom_text(data = ppg_ends, aes(label = paste0(name, " ", sprintf("%.1f", ppg))), hjust = -0.2, size = 4.5,
            fontface = "bold") +
  scale_color_manual(values = c("A'ja Wilson" = AJA, "Breanna Stewart" = STEWIE), guide = "none") +
  scale_x_continuous(breaks = 2016:2026, limits = c(2016, 2027.6)) +
  scale_y_continuous(limits = c(14, 28)) +
  labs(title = "Points per game by season",
       subtitle = "Stewie missed 2019 (Achilles)",
       x = NULL, y = NULL, caption = CAPTION) +
  theme_wnbadata() +
  theme(panel.grid.major.x = element_blank())

# 4b. "nobody keeps the same pace forever": DT's points per game by age, with aja's so far
# both started at 22. hollow points = played under half the team's games (DT: 8 games at 30, 6 at 37)

t_arc <- logs |>
  filter(player %in% c("Diana Taurasi", "A'ja Wilson")) |>
  group_by(player, season) |>
  summarise(games = n(), points = sum(points), .groups = "drop") |>
  left_join(LEAGUE_GAMES, by = "season") |>
  mutate(age = season - if_else(player == "Diana Taurasi", 1982, 1996),
         ppg = points / games,
         short_season = games < team_games / 2)

# DT's parabola: weighted by games so the 8- and 6-game seasons barely count. peaks around age 27-28
dt_fit <- lm(ppg ~ age + I(age^2), data = filter(t_arc, player == "Diana Taurasi"), weights = games)
dt_peak_age <- -coef(dt_fit)[["age"]] / (2 * coef(dt_fit)[["I(age^2)"]])
dt_curve <- tibble(age = seq(22, 42, by = 0.25)) |> mutate(ppg = predict(dt_fit, newdata = pick(everything())))

stopifnot(round(dt_peak_age) == 27,
          # aja's 3 best seasons are her last 3
          t_arc |> filter(player == "A'ja Wilson") |> slice_max(ppg, n = 3) |> pull(season) |> sort() == 2024:2026)

ARC_COLORS <- c("Diana Taurasi" = DT, "A'ja Wilson" = AJA)

arc_labels <- t_arc |>
  group_by(player) |>
  slice_max(age) |>
  ungroup() |>
  mutate(name = if_else(player == "A'ja Wilson", "A'ja", "DT"))

# the connecting line skips short seasons and missing ones (DT sat out 2015, age 33)
arc_lines <- t_arc |>
  mutate(ppg = if_else(short_season, NA_real_, ppg)) |>
  complete(player, age = 22:42) |>
  filter(player == "Diana Taurasi" | age <= 30)

p_arc <- ggplot(t_arc, aes(age, ppg, color = player)) +
  geom_line(data = dt_curve, aes(age, ppg), inherit.aes = FALSE, color = DT, linetype = "dashed",
            linewidth = 0.9, alpha = 0.7) +
  geom_line(data = arc_lines, linewidth = 1, alpha = 0.5, na.rm = TRUE) +
  geom_point(data = filter(t_arc, !short_season), size = 3) +
  geom_point(data = filter(t_arc, short_season), size = 3, shape = 21, fill = "white", stroke = 1.2) +
  geom_text(data = filter(t_arc, short_season), aes(label = paste(games, "games")), vjust = 2, size = 3.4,
            show.legend = FALSE) +
  geom_text(data = arc_labels, aes(label = name), hjust = -0.4, size = 5, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = ARC_COLORS, guide = "none") +
  scale_x_continuous(breaks = seq(22, 42, by = 2), limits = c(22, 43.5)) +
  scale_y_continuous(limits = c(0, 28)) +
  labs(title = "Points per game by age",
       subtitle = "dashed: a parabola fit to DT's seasons, weighted by games played.\nhollow: played under half the season",
       x = "age (season year minus birth year)", y = NULL, caption = CAPTION) +
  theme_wnbadata() +
  theme(panel.grid.major.x = element_blank())

# 4c. how much can she slow down? points per game she needs to break it by the end of each season
# "plays every game" uses the full 50/52 schedule. "plays like recently" uses the share of games she
# played in 2024-26 (119 of 128 = 93%)

aja_recent_share <- t_arc |> filter(player == "A'ja Wilson", season >= 2024) |>
  summarise(share = sum(games) / sum(team_games)) |> pull(share)

t_needed <- SCHEDULE |>
  filter(season <= 2032) |>
  mutate(games_left = cumsum(team_games),
         every_game = (TO_PASS - 6792) / games_left,
         plays_like_recently = (TO_PASS - 6792) / (games_left * aja_recent_share),
         age_at_end = season - 1996)

stopifnot(round(aja_recent_share, 2) == 0.93,
          round(t_needed$every_game[t_needed$season == 2030], 1) == 18.9,
          round(DT_PPG, 1) == 18.8)

# by 2027 or 2028 would take 77.1 or 38.5 a game, so start at 2029
needed_bars <- t_needed |> filter(season >= 2029) |>
  mutate(label = sprintf("%.1f", every_game), by = paste("by the end of\n", season))

p_needed <- ggplot(needed_bars, aes(by, every_game)) +
  geom_col(fill = AJA, width = 0.65) +
  geom_text(aes(label = label), vjust = 1.6, size = 6, fontface = "bold", color = "white") +
  geom_hline(yintercept = 26.2, linetype = "dashed", color = INK) +
  annotate("text", x = 4.45, y = 26.2, label = "A'ja in 2026: 26.2", hjust = 1, vjust = -0.6, size = 4, color = INK) +
  geom_hline(yintercept = DT_PPG, linetype = "dotted", color = DT, linewidth = 0.9) +
  annotate("text", x = 4.45, y = DT_PPG, label = "DT's career: 18.8", hjust = 1, vjust = -0.6, size = 4, color = DT) +
  scale_y_continuous(limits = c(0, 29), expand = c(0, 0)) +
  labs(title = "Points per game A'ja needs to pass DT",
       subtitle = "3,855 points to go, if she plays every game of the 50 and 52 game seasons",
       x = NULL, y = NULL, caption = CAPTION) +
  theme_wnbadata() +
  theme(panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 13))

# 6. who else is "on pace"? every active player who scores more per game than DT did (18.8)
# only aja and stewie are also past halfway to the record. needs 20+ games so one hot stretch doesn't count

HALFWAY  <- RECORD / 2

t_on_pace <- careers |>
  filter(last_year == 2026, games >= 20, points / games > DT_PPG) |>
  mutate(ppg = points / games,
         passes_at_game = ceiling(TO_PASS / ppg),
         past_halfway = points >= HALFWAY) |>
  arrange(passes_at_game) |>
  select(player, first_year, games, points, ppg, passes_at_game, past_halfway)
t_on_pace |> mutate(ppg = round(ppg, 1)) |> print()

stopifnot(nrow(t_on_pace) == 6, sum(t_on_pace$past_halfway) == 2,
          all(t_on_pace$player[t_on_pace$past_halfway] %in% c("A'ja Wilson", "Breanna Stewart")))

on_pace_rows <- bind_rows(
  t_on_pace,
  tibble(player = "Diana Taurasi", games = DT_GAMES, points = RECORD, ppg = DT_PPG, passes_at_game = DT_GAMES, past_halfway = NA)
) |>
  mutate(row = rev(row_number()),
         group = case_when(is.na(past_halfway) ~ "dt", past_halfway ~ "halfway", TRUE ~ "not yet"),
         games_text = if_else(player == "Diana Taurasi", "565 (her career)", as.character(passes_at_game)))

TABLE_COLS <- c(player = 0, points = 5.2, ppg = 7.4, games_text = 9.6)   # x position of each column

p_on_pace <- ggplot(on_pace_rows, aes(y = row)) +
  geom_rect(data = filter(on_pace_rows, group == "halfway"),
            aes(xmin = -0.2, xmax = 11, ymin = row - 0.45, ymax = row + 0.45), fill = AJA, alpha = 0.1) +
  geom_text(aes(x = TABLE_COLS["player"], label = player, color = group), hjust = 0, size = 5, fontface = "bold") +
  geom_text(aes(x = TABLE_COLS["points"], label = comma(points), color = group), size = 5) +
  geom_text(aes(x = TABLE_COLS["ppg"], label = sprintf("%.1f", ppg), color = group), size = 5) +
  geom_text(aes(x = TABLE_COLS["games_text"], label = games_text, color = group), size = 5) +
  annotate("text", x = TABLE_COLS[-1], y = nrow(on_pace_rows) + 1,
           label = c("career\npoints", "points\nper game", "passes DT\nin game"),
           size = 4, color = GRAY, lineheight = 0.9) +
  annotate("segment", x = -0.2, xend = 11, y = 1.5, yend = 1.5, color = LIGHT) +
  scale_color_manual(values = c(halfway = INK, `not yet` = GRAY, dt = DT), guide = "none") +
  scale_x_continuous(limits = c(-0.2, 11)) +
  scale_y_continuous(limits = c(0.5, nrow(on_pace_rows) + 1.6)) +
  labs(title = "Active players who score more per game than DT did",
       subtitle = "\"passes DT in game\" = the career game they'd pass 10,646 at their career points per game",
       x = NULL, y = NULL, caption = CAPTION) +
  theme_wnbadata() +
  theme(axis.text = element_blank(), panel.grid = element_blank())

# 0. "DT holds the record with 10,646 points, and aja is currently 11th with 6,792"
# simple all-time leaderboard, top 11 so aja is the last bar

leader_bars <- t_leaders |>
  head(11) |>
  mutate(fill = case_when(player == "A'ja Wilson" ~ AJA, player == "Breanna Stewart" ~ STEWIE,
                          player == "Diana Taurasi" ~ DT, TRUE ~ LIGHT),
         label = paste0(rank, ". ", player),
         label = fct_reorder(label, points))

stopifnot(leader_bars$player[1] == "Diana Taurasi", leader_bars$player[11] == "A'ja Wilson")

p_leaders <- ggplot(leader_bars, aes(points, label, fill = fill)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = comma(points)), hjust = -0.15, size = 4.5, fontface = "bold", color = INK) +
  scale_fill_identity() +
  scale_x_continuous(labels = comma, limits = c(0, 12200), expand = c(0, 0)) +
  labs(title = "Most points in WNBA history",
       subtitle = "regular season, through 2026",
       x = NULL, y = NULL, caption = CAPTION) +
  theme_wnbadata() +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 12))

# 7. "she got there in just 308 games, the fastest ever. DT needed 337"
# everyone who has reached 6,792 regular season points, and how many games it took

fastest_bars <- t_fastest_to_6792 |>
  mutate(fill = case_when(player == "A'ja Wilson" ~ AJA, player == "Breanna Stewart" ~ STEWIE,
                          player == "Diana Taurasi" ~ DT, TRUE ~ LIGHT),
         player = fct_reorder(player, games_to_6792, .desc = TRUE))

p_fastest <- ggplot(fastest_bars, aes(games_to_6792, player, fill = fill)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = games_to_6792), hjust = -0.2, size = 4.5, fontface = "bold", color = INK) +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0, 640), expand = c(0, 0)) +
  labs(title = "Games it took to reach 6,792 points",
       subtitle = "every WNBA player who has scored 6,792+ regular season points",
       x = "career games", y = NULL, caption = CAPTION) +
  theme_wnbadata() +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 12))

# 9. "only 3 players have ever scored at least that many after their age 30 season"

after30_bars <- t_after_age_30 |>
  head(8) |>
  mutate(did_it = after_30 >= TO_PASS - 6792,
         fill = case_when(player == "Diana Taurasi" ~ DT, did_it ~ INK, TRUE ~ LIGHT),
         player = fct_reorder(player, after_30))

stopifnot(sum(after30_bars$did_it) == 3)

p_after30 <- ggplot(after30_bars, aes(after_30, player, fill = fill)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = TO_PASS - 6792, linetype = "dashed", color = AJA, linewidth = 0.9) +
  annotate("text", x = TO_PASS - 6792, y = 8.6, label = "what A'ja needs: 3,855", hjust = 1.05, size = 4,
           color = AJA, fontface = "bold") +
  # numbers inside the bars so the dashed line doesn't run through them
  geom_text(aes(x = 80, label = comma(after_30), color = fill == LIGHT), hjust = 0, size = 4.5, fontface = "bold") +
  scale_color_manual(values = c(`TRUE` = INK, `FALSE` = "white"), guide = "none") +
  scale_fill_identity() +
  scale_x_continuous(labels = comma, limits = c(0, 5900), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1))) +
  labs(title = "Most points after their age 30 season",
       subtitle = "regular season points from the season after they turned 30 on.\nBonner played in 2026 at 39",
       x = NULL, y = NULL, caption = CAPTION) +
  theme_wnbadata() +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 12))

# ---- numbers (type any of these in the console) ----

# t_when               season and game each scenario passes DT, and her age (t_when_simple is the short one)
# t_on_pace            active players who score more per game than DT. only aja and stewie are past halfway

# ---- charts (in script order) ----
p_scatter       # hook: "will aja wilson break the record?" the old chart redone, future aja game 483, future stewie 519
p_leaders       # "DT holds the record with 10,646 ... aja is currently 11th with 6,792"
p_fastest       # "she got there in just 308 games, the fastest ever. DT needed 337"
p_aja_stewie    # "stewart has 68 more career points ... aja averages more per game, 22.1 to 20.5"
p_ppg           # "stewart has 68 more points ... aja averages more per game, 22.1 to 20.5"
p_games_needed  # "aja breaks the record in her 483rd game, stewie in her 519th ... DT 565"
p_arc           # "nobody keeps the same pace forever": DT peaked mid 20s, aja's best 3 are her last 3
p_after30       # "only 3 players have ever scored at least that many after their age 30 season"
p_needed        # "she only needs 18.9 a game to break it by the end of 2030", about DT's career average
p_on_pace       # "technically 4 other active players also score more per game than DT did"
