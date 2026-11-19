# How hard is every D1 team's non-conference schedule? (2026-27 NCAA WBB)
#
# Early in the season nobody knows how good anyone is yet, so opponent strength is a
# preseason guess: last season's her hoop stats rating, pulled toward average by how much
# ratings usually carry over (2010-2026: year-to-year correlation ~0.85, slope ~0.86).
#
# The schedule score is "expected losses for a top-25 team": put a typical top-25 team
# (the 25th best preseason rating) in this team's shoes, same opponents, same home/away/neutral,
# and add up its chance of losing each game. Average opponent rating overrates cupcake-heavy
# schedules being "balanced" by one great game, this doesn't.
#
# Data: Her Hoop Stats (every D1 game and schedule since 2010-11, and each season's final ratings).
# The data/ folder isn't included in this repo, so the code is here to read but won't run without it.

library(tidyverse)

data_dir <- "~/Desktop/wnbadata/2026-11-19 uconn non conference schedule/data"
games        <- read_csv(file.path(data_dir, "games.csv"), col_types = cols(game_time = col_character()), show_col_types = FALSE)
team_seasons <- read_csv(file.path(data_dir, "team_seasons.csv"), show_col_types = FALSE)
ratings      <- read_csv(file.path(data_dir, "ratings.csv"), show_col_types = FALSE)
scores       <- read_csv(file.path(data_dir, "scores.csv"), show_col_types = FALSE)

games <- games |>
  # Games with a TBA tip time are listed at 11:59 pm Eastern the night before, so move those to the real date
  mutate(game_date = if_else(game_time == "23:59", game_date + 1, game_date)) |>
  # Drop games that won't be played: cancelled or postponed this season, and in past seasons
  # anything without a box score (cancelled games stay on the schedule)
  filter(!game_status %in% c("CANCELLED", "POSTPONED"),
         end_year == 2027 | has_box_score == 1)

# ---- Model constants (all estimated from her hoop stats data) ----
# Ratings are the ones herhoopstats.com shows (D1 vs D1 games only). D2 teams have their own D2 ratings.
carryover    <- 0.86   # slope of next season's rating on this season's, 2010-11 to 2025-26 pairs
rating_mean  <- 0.46   # D1 average HHS rating
prior_sd     <- 9.35   # rating points a preseason guess typically misses by (2025 -> 2026)
pts_per_rtg  <- 0.802  # game margin (points) per point of rating difference, 2025-26 D1 vs D1
home_court   <- 3.14   # points
game_sd      <- 11.37  # points, spread of real margins around the expected margin
d2_rating    <- -44.5  # what a D2 opponent plays like on the D1 scale (2025-26 D1 vs D2 results)
other_rating <- -65.9  # D3, NAIA and others

# ---- Preseason ratings for season `yr` from season `yr - 1` ----
preseason_ratings <- function(yr) {
  last <- ratings |>
    filter(end_year == yr - 1) |>
    select(team_id, level, rating) |>
    pivot_wider(names_from = level, values_from = rating)
  if (!"d2" %in% names(last)) last$d2 <- NA_real_  # no D2 ratings in some early seasons
  d2_mean <- mean(last$d2, na.rm = TRUE)

  team_seasons |>
    filter(end_year == yr) |>
    left_join(last, by = "team_id") |>
    mutate(
      prior = case_when(
        division == 1 & !is.na(d1) ~ rating_mean + carryover * (d1 - rating_mean),
        # moved up from D2: start from the D2 level and keep how good they were within D2
        division == 1 & !is.na(d2) ~ d2_rating + carryover * (d2 - d2_mean),
        division == 1 ~ d2_rating,
        division == 2 ~ d2_rating,
        TRUE ~ other_rating
      ),
      prior_source = case_when(
        division == 1 & !is.na(d1) ~ "last season D1",
        division == 1 & !is.na(d2) ~ "last season D2",
        division == 1 ~ "no rating",
        TRUE ~ "non-D1"
      )
    ) |>
    select(team_id, team, conf, division, prior, prior_source, rating_last = d1)
}

# ---- One row per D1 team per game ----
team_games <- function(yr, team_ratings) {
  # Regular season only: drop NCAA tournament games and anything from March on (WNIT, WBI,
  # conference tournaments). A few late-season non-conference games could be lost, none for 2027.
  g <- games |> filter(end_year == yr, is.na(ncaa_round), game_date < make_date(yr, 3, 1))
  both_sides <- bind_rows(
    g |> transmute(gid, game_date, neutral_site, team_id = home_id, opp_id = away_id, side = "home"),
    g |> transmute(gid, game_date, neutral_site, team_id = away_id, opp_id = home_id, side = "away")
  )
  both_sides |>
    mutate(venue = if_else(neutral_site == 1, "neutral", side)) |>
    inner_join(team_ratings |> filter(division == 1) |> select(team_id, team, conf), by = "team_id") |>
    left_join(
      team_ratings |> select(opp_id = team_id, opp = team, opp_conf = conf, opp_division = division, opp_prior = prior),
      by = "opp_id"
    ) |>
    left_join(scores |> select(gid, team_id, pts, opp_pts), by = c("gid", "team_id")) |>
    mutate(
      tbd = is.na(opp) | opp %in% c("TBA", "TBD"),
      # 2011-2012 list independents as "-", "DI Independent" or the school's own name
      indep = conf %in% c("-", "DI Independent") | conf == team,
      opp_indep = is.na(opp_conf) | opp_conf %in% c("-", "DI Independent") | opp_conf == opp,
      nonconf = is.na(opp_division) | opp_division != 1 | indep | opp_indep | opp_conf != conf
    )
}

# Chance a team rated `rating` beats each opponent, with the venue from that team's side
win_prob <- function(rating, opp_rating, venue, extra_sd = prior_sd) {
  edge <- pts_per_rtg * (rating - opp_rating) +
    case_when(venue == "home" ~ home_court, venue == "away" ~ -home_court, TRUE ~ 0)
  pnorm(edge / sqrt(game_sd^2 + (pts_per_rtg * extra_sd)^2))
}

score_schedules <- function(tg, team_ratings, opp_rating_col = "opp_prior", extra_sd = prior_sd) {
  d1 <- team_ratings |> filter(division == 1)
  top25_cut <- sort(d1$prior, decreasing = TRUE)[25]  # also the benchmark team's rating
  top50_cut <- sort(d1$prior, decreasing = TRUE)[50]

  tg |>
    filter(nonconf, !tbd) |>
    mutate(
      opp_r = .data[[opp_rating_col]],
      top25_loss = 1 - win_prob(top25_cut, opp_r, venue, extra_sd)
    ) |>
    group_by(team_id, team, conf) |>
    summarize(
      games = n(),
      exp_losses_top25 = sum(top25_loss),
      loss_pct_top25 = mean(top25_loss),
      vs_top25 = sum(opp_r >= top25_cut),
      vs_top50 = sum(opp_r >= top50_cut),
      vs_non_d1 = sum(is.na(opp_division) | opp_division != 1),
      road = sum(venue == "away"),
      avg_opp = mean(opp_r),
      .groups = "drop"
    ) |>
    mutate(rank = min_rank(desc(exp_losses_top25)))
}

# ---- 2026-27 ----
ratings_2027 <- preseason_ratings(2027)
tg_2027 <- team_games(2027, ratings_2027)
sos_2027 <- score_schedules(tg_2027, ratings_2027) |>
  left_join(
    ratings_2027 |> filter(division == 1) |>
      transmute(team_id, prior, preseason_rank = min_rank(desc(prior)), prior_source),
    by = "team_id"
  ) |>
  left_join(tg_2027 |> filter(nonconf) |> count(team_id, name = "tbd_games", wt = tbd), by = "team_id") |>
  arrange(rank)

# ---- Backtest: would last year's preseason version have held up? ----
# 2025-26 schedules scored twice: with preseason guesses (from 2024-25) and with the
# end-of-season 2025-26 ratings, where opponent strength is known (no extra uncertainty).
ratings_2026_pre <- preseason_ratings(2026)
tg_2026 <- team_games(2026, ratings_2026_pre)
ratings_2026_end <- ratings |>
  filter(end_year == 2026, level == "d1") |>
  select(opp_id = team_id, opp_end = rating)
tg_2026 <- tg_2026 |>
  left_join(ratings_2026_end, by = "opp_id") |>
  mutate(opp_end = coalesce(opp_end, opp_prior))

sos_2026_pre <- score_schedules(tg_2026, ratings_2026_pre)
sos_2026_end <- score_schedules(tg_2026, ratings_2026_pre, "opp_end", extra_sd = 0)
backtest <- inner_join(
  sos_2026_pre |> select(team, conf, pre = exp_losses_top25, pre_rank = rank),
  sos_2026_end |> select(team, end = exp_losses_top25, end_rank = rank),
  by = "team"
)

# ---- History: every season since 2010-11, scored the same way ----
# Each season uses its own preseason guesses (from the season before), its own top-25
# benchmark and its own ranking, so 2026-27 is compared like for like.
# Team-seasons with fewer than 8 conference games are left out: independents and teams moving
# between conferences (17 in 2011, 14 in 2012, 7 in 2013), where every game counts as non-conference.
sos_history <- map(2011:2027, \(yr) {
  r <- preseason_ratings(yr)
  tg <- team_games(yr, r)
  conf_games <- tg |> filter(!tbd) |> group_by(team_id) |> summarize(conf_games = sum(!nonconf))
  score_schedules(tg, r) |>
    mutate(end_year = yr, teams = n()) |>
    left_join(conf_games, by = "team_id") |>
    left_join(r |> filter(division == 1) |> transmute(team_id, preseason_rank = min_rank(desc(prior))), by = "team_id")
}) |>
  list_rbind() |>
  filter(conf_games >= 8) |>
  mutate(
    all_time_rank = min_rank(desc(exp_losses_top25)),
    all_time_rank_per_game = min_rank(desc(loss_pct_top25))
  )

# Where 2026-27 UConn sits among every D1 non-conference schedule since 2010-11
sos_history |>
  arrange(all_time_rank) |>
  select(all_time_rank, end_year, team, conf, games, exp_losses_top25, vs_top25, vs_top50, road, preseason_rank) |>
  print(n = 15)

# Most games against the preseason top 25
sos_history |>
  arrange(desc(vs_top25), all_time_rank) |>
  select(end_year, team, games, vs_top25, all_time_rank) |>
  print(n = 10)

# ---- Numbers for the script ----
sos_2027 |>
  select(rank, team, conf, games, exp_losses_top25, vs_top25, vs_top50, vs_non_d1, road, preseason_rank) |>
  print(n = 25)

# Preseason top 25 teams, easiest schedule first
sos_2027 |>
  filter(preseason_rank <= 25) |>
  arrange(exp_losses_top25) |>
  select(preseason_rank, team, conf, games, exp_losses_top25, rank, vs_top25, vs_top50) |>
  print(n = 25)

# Conference averages
sos_2027 |>
  group_by(conf) |>
  summarize(teams = n(), exp_losses_top25 = mean(exp_losses_top25), games = mean(games), road = mean(road)) |>
  arrange(desc(exp_losses_top25)) |>
  print(n = 32)

# Backtest: how much last season's preseason ranking moved by the end of the season
backtest |>
  summarize(
    rank_corr = cor(pre, end, method = "spearman"),
    median_rank_move = median(abs(pre_rank - end_rank)),
    end_top10_were_pre_top15 = sum(end_rank <= 10 & pre_rank <= 15)
  )

# ---- Charts ----
POWER  <- "#7048B6"   # ACC, Big East, Big Ten, Big 12, SEC
GRAY   <- "#8A8A86"
INK    <- "#15171D"
CREDIT <- "2026-27 non-conference schedules as of 11/17  ·  data: her hoop stats  |  chart: @wnbadata"
power_confs <- c("ACC", "Big East", "Big Ten", "Big 12", "SEC")

base_theme <- function() {
  theme_minimal(base_size = 15) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_text(face = "bold", color = INK, size = 19),
      plot.subtitle = element_text(color = GRAY, size = 13, margin = margin(b = 14)),
      plot.caption  = element_text(color = GRAY, size = 11, hjust = 0, margin = margin(t = 14)),
      axis.text     = element_text(color = INK, size = 13),
      legend.position = "top",
      legend.justification = "left",
      legend.text   = element_text(color = INK, size = 12),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#E3E3E0", linewidth = 0.3),
      plot.margin   = margin(18, 18, 14, 10)
    )
}

# 1. The 15 hardest non-conference schedules
hardest <- sos_2027 |>
  slice_min(rank, n = 15, with_ties = FALSE) |>
  mutate(
    # highlight only the teams the script names, so the colors don't depend on what counts as a power conference
    hl = if_else(team %in% c("UConn", "South Carolina", "Southern U."), "in the script", "other"),
    label = paste0(rank, ". ", team),
    label = fct_reorder(label, exp_losses_top25),
    # bar length = expected losses for a team like the 25th best playing this schedule
    value_label = paste0(format(round(exp_losses_top25, 1), nsmall = 1), " of ", games)
  )

# Made for a tall export (about 6 x 10.7 in, or 1080 x 1920 px)
hardest_chart <- ggplot(hardest, aes(x = exp_losses_top25, y = label, fill = hl)) +
  geom_col(width = 0.74) +
  geom_text(aes(label = value_label), hjust = -0.08, color = INK, size = 5.6) +
  scale_fill_manual(values = c(`in the script` = POWER, other = GRAY), guide = "none") +
  scale_x_continuous(limits = c(0, 10.4), expand = c(0, 0)) +
  labs(
    title = "the hardest\nnon-conference schedules",
    subtitle = "games a team like the 25th best\nin the country would expect to lose\nplaying the same schedule",
    caption = "2026-27 schedules as of 11/17\ndata: her hoop stats  |  chart: @wnbadata", x = NULL, y = NULL
  ) +
  base_theme() +
  theme(
    axis.text.x = element_blank(), panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", color = INK, size = 26, lineheight = 1),
    plot.subtitle = element_text(color = GRAY, size = 17, lineheight = 1.05, margin = margin(b = 16)),
    plot.caption = element_text(color = GRAY, size = 13, hjust = 0, lineheight = 1.1, margin = margin(t = 14)),
    axis.text.y = element_text(color = INK, size = 15, lineheight = 0.95)
  )

ordinal <- function(n) {
  suffix <- case_when(n %% 100 %in% 11:13 ~ "th", n %% 10 == 1 ~ "st", n %% 10 == 2 ~ "nd", n %% 10 == 3 ~ "rd", TRUE ~ "th")
  paste0(n, suffix)
}

# 2. The top teams this year: schedule rank, games vs last year's top 25, true road games
top_team_names <- c("UConn", "South Carolina", "Texas", "UCLA", "Southern California")
top_teams <- sos_2027 |>
  filter(team %in% top_team_names) |>
  mutate(
    team = if_else(team == "Southern California", "USC", team),
    team = fct_reorder(team, exp_losses_top25),
    hl = if_else(team == "UConn", "UConn", "other")
  )

top_teams_chart <- ggplot(top_teams, aes(x = exp_losses_top25, y = team, fill = hl)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = paste0(ordinal(rank), "  ·  ", vs_top25, " of ", games, " vs top 25")), hjust = -0.06, color = INK, size = 4.6) +
  scale_fill_manual(values = c(UConn = POWER, other = GRAY), guide = "none") +
  scale_x_continuous(limits = c(0, 10.8), expand = c(0, 0)) +
  labs(
    title = "non-conference schedules of the top teams",
    subtitle = "schedule rank out of 363\ntop 25 = top 25 in her hoop stats rating last season",
    caption = CREDIT, x = NULL, y = NULL
  ) +
  base_theme() +
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank(), axis.text.y = element_text(size = 16))

# 3. Most non-conference games against last season's top 25 since 2010-11 (every team-season with 7+)
top25_games <- sos_history |>
  filter(vs_top25 >= 7) |>
  arrange(desc(vs_top25), end_year) |>
  mutate(
    season = paste0(end_year - 1, "-", str_sub(end_year, 3, 4)),
    label = fct_rev(fct_inorder(paste(team, season))),
    hl = if_else(team == "UConn" & end_year == 2027, "this year", "before")
  )

top25_games_chart <- ggplot(top25_games, aes(x = vs_top25, y = label, fill = hl)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = paste0(vs_top25, " of ", games)), hjust = -0.12, color = INK, size = 4.6) +
  scale_fill_manual(values = c(`this year` = POWER, before = GRAY), guide = "none") +
  scale_x_continuous(limits = c(0, 10.6), expand = c(0, 0)) +
  labs(
    title = "most non-conference games vs last season's top 25",
    subtitle = "every team since 2010-11 with 7 or more\ntop 25 = top 25 in her hoop stats rating the season before",
    caption = "data: her hoop stats  |  chart: @wnbadata", x = NULL, y = NULL
  ) +
  base_theme() +
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())

# 4. One season's rating vs the next: good teams stay good, just a little less good
season_ratings <- ratings |>
  filter(level == "d1") |>
  transmute(team_id, end_year, rating)
carry_pairs <- season_ratings |>
  inner_join(season_ratings |> mutate(end_year = end_year - 1), by = c("team_id", "end_year"), suffix = c("", "_next")) |>
  filter(end_year <= 2025)  # 2026-27 ratings are only a few games old

example_x <- rating_mean + 10
example_y <- rating_mean + carryover * 10

carryover_chart <- ggplot(carry_pairs, aes(x = rating, y = rating_next)) +
  geom_point(color = GRAY, alpha = 0.18, size = 1.3, stroke = 0) +
  geom_abline(slope = 1, intercept = 0, color = GRAY, linewidth = 0.6, linetype = "dashed") +
  geom_abline(slope = carryover, intercept = rating_mean * (1 - carryover), color = POWER, linewidth = 1.6) +
  annotate("segment", x = example_x, xend = example_x, y = -56, yend = example_y, color = INK, linewidth = 0.4, linetype = "dotted") +
  annotate("segment", x = -56, xend = example_x, y = example_y, yend = example_y, color = INK, linewidth = 0.4, linetype = "dotted") +
  annotate("point", x = example_x, y = example_y, color = POWER, size = 4.6) +
  # The note sits in the empty top-left corner, starting at the left edge, so it fits at any export size,
  # with an arrow down to the example point
  annotate("text", x = -53, y = 66, hjust = 0, vjust = 1, color = INK, size = 6.6, lineheight = 0.95,
           label = "10 better than\naverage one year\n= about 9 better the next") +
  annotate("curve", x = -28, y = 28, xend = example_x - 2, yend = example_y + 2, curvature = 0.3,
           color = INK, linewidth = 0.6, arrow = arrow(length = unit(0.12, "inches"), type = "closed")) +
  annotate("text", x = 56, y = 64, hjust = 1, color = GRAY, size = 5.8, label = "no change") +
  scale_x_continuous(limits = c(-56, 68), breaks = seq(-40, 60, 20), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-56, 68), breaks = seq(-40, 60, 20), expand = c(0, 0)) +
  coord_equal() +
  labs(
    title = "her hoop stats rating\none season vs the next",
    subtitle = "every D1 team, 2009-10 to 2025-26\npurple line = the usual carryover",
    caption = "data: her hoop stats  |  chart: @wnbadata",
    x = "rating one season", y = "rating the next season"
  ) +
  base_theme() +
  theme(
    panel.grid.major.y = element_line(color = "#E3E3E0", linewidth = 0.3),
    # bigger text than the other charts so it reads on a phone
    plot.title = element_text(face = "bold", color = INK, size = 28, lineheight = 1),
    plot.subtitle = element_text(color = GRAY, size = 19, margin = margin(b = 14)),
    plot.caption = element_text(color = GRAY, size = 15, hjust = 0, margin = margin(t = 14)),
    axis.text = element_text(color = INK, size = 19),
    axis.title = element_text(color = GRAY, size = 19)
  )

# 5. Schedule tables (printed, not charted)
# Printable table of one team's non-conference schedule: venue, each opponent's final 2025-26 D1 rank, top 25, power conference, result
# (the same numbers herhoopstats.com shows). Non-D1 opponents have no rating or rank.
# schedule_table("UConn"), schedule_table("Southern U.")
schedule_table <- function(team_name) {
  last_season <- ratings_2027 |>
    filter(division == 1) |>
    transmute(opp_id = team_id, rating_2025_26 = round(rating_last, 1), rank_2025_26 = min_rank(desc(rating_last)))
  tg_2027 |>
    filter(team == team_name, nonconf, !tbd) |>
    left_join(last_season, by = "opp_id") |>
    arrange(game_date) |>
    transmute(date = game_date, opp, venue, rank_2025_26,
              top_25 = !is.na(rank_2025_26) & rank_2025_26 <= 25,
              power_conf = opp_conf %in% power_confs,
              result = if_else(is.na(pts), "", paste0(if_else(pts > opp_pts, "W ", "L "), pts, "-", opp_pts)))
}

# Chart index, in script order:
#   top25_games_chart   9 of 12 vs last season's top 25, old high of 8 (UConn 3 times)
#   carryover_chart     "10 points better becomes about 9"
#   top_teams_chart     UConn 1st, South Carolina 2nd (with Texas, UCLA, USC)
#   hardest_chart       full top 15, Southern tied with South Carolina
top25_games_chart
carryover_chart
top_teams_chart
hardest_chart

schedule_table("Southern U.")
schedule_table("UConn")
schedule_table("South Carolina")
