# =========================================
# How much does WNBA playoff seeding matter?
# =========================================
# Every WNBA playoff series and game, 1997-2025, scraped from Basketball Reference,
# with seeds from the Wikipedia playoff brackets.
#
# Everything here runs on public data. Section 1 rebuilds the three CSVs in data/
# from scratch; it only runs if those files are missing, because it makes ~60 polite
# web requests and takes a few minutes.

# ============================
# Libraries
# ============================
library(rvest)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(gt)

DATA_DIR <- "data"
SEASONS  <- 1997:2025

# ============================
# 1. Build the data (skipped when data/ is already populated)
# ============================
# Basketball Reference gives results and, through the "@" in each game line, where every
# game was played. It does NOT give playoff seeds, and its standings order gets cross
# conference ties wrong (in 2016 it has Atlanta ahead of Indiana; the real seeds were
# Indiana 5, Atlanta 6). So seeds come from the Wikipedia bracket templates instead.

fetch <- function(url, path, sleep = 4) {
  if (!file.exists(path)) {
    download.file(url, path, quiet = TRUE,
                  headers = c(`User-Agent` = "wnba-analysis (github.com/madbro206)"))
    Sys.sleep(sleep)  # be nice to the servers
  }
  path
}

# Basketball Reference hides most tables inside HTML comments, so strip the markers first
read_uncommented <- function(path) {
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  read_html(gsub("<!--|-->", "", html))
}

scrape_season <- function(season, cache) {
  page <- read_uncommented(fetch(
    sprintf("https://www.basketball-reference.com/wnba/years/%d.html", season),
    file.path(cache, sprintf("bref_%d.html", season))))

  standings <- map_dfr(c("standings_e", "standings_w"), function(id) {
    tbl <- html_element(page, paste0("table#", id))
    if (inherits(tbl, "xml_missing")) return(NULL)
    d <- html_table(tbl)
    names(d)[1] <- "team"
    mutate(d, conf = sub("standings_", "", id))
  }) |>
    filter(!is.na(suppressWarnings(as.numeric(W)))) |>
    transmute(season, conf,
              team = str_trim(str_remove_all(team, "\\*|\\(\\d+\\)")),
              playoff = str_detect(team, "\\*") | TRUE,
              W = as.numeric(W), L = as.numeric(L), wpct = W / (W + L))

  # the playoff table is one row per series ("... over ... (2-1)") followed by its games
  rows  <- html_elements(html_element(page, "table#all_playoffs"), "tr")
  text  <- str_squish(html_text2(rows))
  is_series <- str_detect(text, " over ")
  series_idx <- cumsum(is_series)

  series <- tibble(season, series_idx = series_idx[is_series], raw = text[is_series]) |>
    mutate(pmap_dfr(list(raw, season), function(raw, season) {
      before <- str_remove(raw, " over .*$")
      after  <- str_remove(str_match(raw, " over (.*)$")[, 2], " \\(\\d-\\d\\).*$")
      teams  <- standings$team
      winner <- teams[str_ends(before, fixed(teams))]
      winner <- winner[which.max(nchar(winner))]
      loser  <- teams[teams == after]
      # Basketball Reference occasionally uses a team's older name in the series line
      if (!length(loser)) loser <- teams[str_detect(after, fixed(word(teams, 1)))][1]
      score <- str_match(raw, "\\((\\d)-(\\d)\\)")
      tibble(round = str_trim(str_remove(before, paste0(fixed(winner), "$"))),
             winner = winner, loser = loser,
             ww = as.integer(score[, 2]), wl = as.integer(score[, 3]))
    }))

  # game rows: "Game 1 Thu, October 10 Minnesota Lynx 95 @ New York Liberty 93".
  # the table also carries an outer row with every game glued together, so drop any row
  # holding more than one "Game N".
  games <- tibble(season, series_idx, raw = text) |>
    filter(str_detect(raw, "^Game \\d"), str_count(raw, "Game \\d") == 1) |>
    distinct() |>
    mutate(m = str_match(raw, "^Game (\\d) (.+?) ([A-Z][A-Za-z. ]+?) (\\d+) @ ([A-Z][A-Za-z. ]+?) (\\d+)$"),
           game = as.integer(m[, 2]), away = m[, 4], away_pts = as.integer(m[, 5]),
           home = m[, 6], home_pts = as.integer(m[, 7])) |>
    select(season, series_idx, game, away, away_pts, home, home_pts)

  list(standings = standings, series = series, games = games)
}

# Wikipedia bracket templates: "| RD1-seed1 = 5" paired with "| RD1-team1 = [[...|Indiana Fever]]"
scrape_seeds <- function(season, cache) {
  lines <- readLines(fetch(
    sprintf("https://en.wikipedia.org/w/index.php?title=%d_WNBA_playoffs&action=raw", season),
    file.path(cache, sprintf("wiki_%d.txt", season)), sleep = 1), warn = FALSE)
  seeds <- str_match(lines, "RD(\\d)-seed(\\d+)\\s*=\\s*'*([EW]?\\d+)")
  teams <- str_match(lines, "RD(\\d)-team(\\d+)\\s*=\\s*(.*)$")
  inner_join(
    tibble(rd = seeds[, 2], slot = seeds[, 3], seed = seeds[, 4]) |> filter(!is.na(rd)),
    tibble(rd = teams[, 2], slot = teams[, 3], raw = teams[, 4]) |> filter(!is.na(rd)) |>
      mutate(label = str_match(raw, "\\[\\[(?:[^|\\]]*\\|)?([^\\]]+)\\]\\]")[, 2],
             label = coalesce(label, str_squish(str_remove_all(raw, "'|\\{\\{.*?\\}\\}|<.*?>")))),
    by = c("rd", "slot")) |>
    distinct(label, seed) |>
    mutate(season = season)
}

build_playoff_data <- function(seasons = SEASONS, dir = DATA_DIR) {
  cache <- file.path(tempdir(), "wnba-playoff-pages")
  dir.create(cache, showWarnings = FALSE, recursive = TRUE)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  scraped   <- map(seasons, scrape_season, cache = cache)
  standings <- bind_rows(map(scraped, "standings"))
  games     <- bind_rows(map(scraped, "games"))
  seeds     <- map_dfr(seasons, scrape_seeds, cache = cache)

  # Wikipedia brackets sometimes list a city ("Houston"), Basketball Reference the full
  # club name ("Houston Comets"), so match on the labels that season actually has
  seeds$team <- map2_chr(seeds$label, seeds$season, function(label, season) {
    candidates <- standings$team[standings$season == season]
    hit <- candidates[candidates == label]
    if (!length(hit)) hit <- candidates[str_starts(candidates, fixed(label))]
    if (length(hit) == 1) hit else NA_character_
  })
  seed_of <- function(team, season) {
    s <- seeds$seed[seeds$team == team & seeds$season == season]
    if (length(s)) s[1] else NA_character_
  }

  series <- bind_rows(map(scraped, "series")) |>
    left_join(standings |> select(season, winner = team, wW = W, wL = L), by = c("season", "winner")) |>
    left_join(standings |> select(season, loser = team, lW = W, lL = L), by = c("season", "loser")) |>
    mutate(w_seed = map2_chr(winner, season, seed_of),
           l_seed = map2_chr(loser, season, seed_of),
           bestof = 2L * pmax(ww, wl) - 1L,
           # seeds are only comparable inside a conference before 2016 ("E2" vs "W1" is not
           # a seeding question), so cross conference Finals are excluded from every stat
           comparable = coalesce(str_extract(w_seed, "[EW]"), "") ==
                        coalesce(str_extract(l_seed, "[EW]"), ""),
           w_n = as.integer(str_extract(w_seed, "\\d+")),
           l_n = as.integer(str_extract(l_seed, "\\d+")),
           higher_won = if_else(comparable, w_n < l_n, NA),
           # the opening round each season: the first round every playoff team has to play
           opening = round %in% c("First Round", "Eastern Conference First Round",
                                  "Western Conference First Round") |
             (season %in% 2000:2015 & str_detect(round, "Conference Semifinals")) |
             (season %in% 1997:1998 & round == "Semifinals"),
           fmt = case_when(bestof == 1 ~ "single game",
                           season <= 2009 ~ "bo3 1-2 (98-09)",
                           season <= 2015 ~ "bo3 1-1-1 (10-15)",
                           season <= 2024 ~ "bo3 2-1 (22-24)",
                           TRUE ~ "bo3 1-1-1 (25)"))

  games <- games |>
    left_join(series |> select(season, series_idx, fmt, bestof, winner, loser, w_n, l_n, comparable),
              by = c("season", "series_idx")) |>
    mutate(higher = case_when(comparable & w_n < l_n ~ winner, comparable & l_n < w_n ~ loser),
           home_is_higher = home == higher, home_won = home_pts > away_pts)

  write.csv(series |> select(season, series_idx, round, opening, fmt, bestof, winner, w_seed,
                             wW, wL, loser, l_seed, lW, lL, ww, wl, comparable, higher_won),
            file.path(dir, "playoff_series.csv"), row.names = FALSE)
  write.csv(games |> select(season, series_idx, fmt, game, away, away_pts, home, home_pts,
                            higher, home_is_higher, home_won),
            file.path(dir, "playoff_games.csv"), row.names = FALSE)
  write.csv(standings, file.path(dir, "standings.csv"), row.names = FALSE)
  invisible(TRUE)
}

if (!all(file.exists(file.path(DATA_DIR, c("playoff_series.csv", "playoff_games.csv", "standings.csv"))))) {
  message("data/ is empty, scraping Basketball Reference and Wikipedia (a few minutes)")
  build_playoff_data()
}

series    <- read.csv(file.path(DATA_DIR, "playoff_series.csv"))
games     <- read.csv(file.path(DATA_DIR, "playoff_games.csv"))
standings <- read.csv(file.path(DATA_DIR, "standings.csv"))

# ============================
# 2. Scope: first rounds only
# ============================
# The first round has been a best of 3 in 1998, 2000-2015 and 2022-2025 (82 series).
# 1997, 1999 and 2016-2021 opened with single elimination games, which are kept for the
# upset rate but left out of anything that needs three games.
seed_number  <- function(x) as.integer(gsub("[^0-9]", "", x))  # "E3" and "3" both mean 3
first_rounds <- series |>
  filter(opening, comparable) |>
  mutate(w_n = seed_number(w_seed), l_n = seed_number(l_seed))
bo3 <- first_rounds |> filter(bestof == 3)

bo3_games <- games |>
  semi_join(bo3, by = c("season", "series_idx")) |>
  filter(!is.na(home_is_higher)) |>
  mutate(higher_won_game = (home_won & home_is_higher) | (!home_won & !home_is_higher))

# ============================
# 3. The two numbers everything rests on
# ============================
# Every best of 3 format hands the higher seed two home games and one road game. What
# changes between formats is when the road game happens, which is what the sweep and
# game 3 rates below pick up.
home_road <- bo3_games |>
  group_by(where = if_else(home_is_higher, "at home", "on the road")) |>
  summarise(games = n(), wins = sum(higher_won_game), pct = wins / games, .groups = "drop")
home_road

p_home <- home_road$pct[home_road$where == "at home"]      # 0.735
q_road <- home_road$pct[home_road$where == "on the road"]  # 0.562

# what the order DOES change: a sweep needs two home wins under 2-1 (p^2) but a home win
# and a road win under 1-1-1 and 1-2 (p*q)
tibble(sweep_2_1 = p_home^2, sweep_split = p_home * q_road,
       game3_2_1 = 1 - p_home^2 - (1 - p_home)^2,
       game3_split = p_home * (1 - q_road) + (1 - p_home) * q_road)

# ============================
# 4. Chart setup
# ============================
HI     <- "#C8102E"
GRAY   <- "#8A8A86"
LIGHT  <- "#B4B2A9"
INK    <- "#15171D"
CREDIT <- "wnba playoffs 1997-2025  |  chart: @wnbadata\ndata: basketball reference, wikipedia"

base_theme <- function() {
  theme_minimal(base_size = 15) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_text(face = "bold", colour = INK, size = 19),
      plot.subtitle = element_text(colour = GRAY, size = 13, margin = margin(b = 14)),
      plot.caption  = element_text(colour = GRAY, size = 11, hjust = 0, margin = margin(t = 14)),
      axis.text     = element_text(colour = INK, size = 13),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3),
      plot.margin   = margin(18, 18, 14, 10)
    )
}

FMT_LABELS <- c("bo3 1-2 (98-09)"   = "1-2\n1998-2009",
                "bo3 1-1-1 (10-15)" = "1-1-1\n2010-2015",
                "bo3 2-1 (22-24)"   = "2-1\n2022-2024",
                "bo3 1-1-1 (25)"    = "1-1-1\n2025")
fmt_factor <- function(x) factor(FMT_LABELS[x], levels = FMT_LABELS)

# ============================
# 5. Charts
# ============================

# 5a. home court is the whole edge
p_home_road <- ggplot(home_road, aes(x = where, y = pct)) +
  geom_col(aes(fill = where == "at home"), width = 0.55, show.legend = FALSE) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = GRAY, linewidth = 0.5) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), vjust = -0.5, colour = INK,
            size = 7, fontface = "bold") +
  geom_text(aes(y = 0.03, label = paste0(wins, " of\n", games, " games")), colour = "white",
            size = 4, vjust = 0, lineheight = 0.9) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = LIGHT)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent, expand = expansion(mult = c(0, 0.02))) +
  labs(title = "how often the higher seed wins a game, home vs away",
       subtitle = "best of 3 first rounds only. dashed line: coin flip",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# 5b. every format is the same three games in a different order
schedule <- tribble(
  ~format,              ~game, ~host,
  "1-2\n1998-2009",     1,     "lower seed",
  "1-2\n1998-2009",     2,     "higher seed",
  "1-2\n1998-2009",     3,     "higher seed",
  "1-1-1\n2010-2015",   1,     "higher seed",
  "1-1-1\n2010-2015",   2,     "lower seed",
  "1-1-1\n2010-2015",   3,     "higher seed",
  "2-1\n2022-2024",     1,     "higher seed",
  "2-1\n2022-2024",     2,     "higher seed",
  "2-1\n2022-2024",     3,     "lower seed",
  "1-1-1\n2025-2026",   1,     "higher seed",
  "1-1-1\n2025-2026",   2,     "lower seed",
  "1-1-1\n2025-2026",   3,     "higher seed"
) |>
  mutate(format = factor(format, levels = rev(unique(format))), game = paste("game", game))

# the hardcoded grid above is checked against where the games were actually played
bo3_games |>
  group_by(fmt, game) |>
  summarise(share_hosted_by_higher = mean(home_is_higher), games = n(), .groups = "drop")

p_schedule <- ggplot(schedule, aes(x = game, y = format, fill = host)) +
  geom_tile(colour = "white", linewidth = 3, width = 0.95, height = 0.85) +
  geom_text(aes(label = if_else(host == "higher seed", "home", "road")),
            colour = "white", size = 5, fontface = "bold") +
  scale_fill_manual(values = c(`higher seed` = HI, `lower seed` = LIGHT), guide = "none") +
  scale_x_discrete(position = "top") +
  labs(title = "same games, different order",
       subtitle = "where the higher seed plays each first round game.\nevery format: two home games, one road game",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.y = element_blank(), axis.text.x = element_text(face = "bold"))

# 5d. sweeps by format
sweeps <- bo3 |>
  group_by(fmt) |>
  summarise(series = n(), swept = sum(higher_won & wl == 0), pct = swept / series, .groups = "drop") |>
  mutate(label = fmt_factor(fmt),
         predicted = if_else(fmt == "bo3 2-1 (22-24)", p_home^2, p_home * q_road))

p_sweep <- ggplot(sweeps, aes(x = label, y = pct)) +
  geom_col(aes(fill = fmt == "bo3 2-1 (22-24)"), width = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(ymin = predicted, ymax = predicted), width = 0.78, linetype = "dashed",
                colour = INK, linewidth = 0.6) +
  geom_text(aes(y = pmax(pct, predicted), label = scales::percent(pct, accuracy = 1)), vjust = -0.5,
            colour = INK, size = 6.4, fontface = "bold") +
  geom_text(aes(y = 0.03, label = paste0(swept, " of\n", series, " series")), colour = "white",
            size = 3.8, vjust = 0, lineheight = 0.9) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = LIGHT)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent, expand = expansion(mult = c(0, 0.02))) +
  labs(title = "sweeps by format",
       subtitle = paste0("how often the higher seed swept its first round 2-0.\ndashed line: what the math predicts\n(",
                         scales::percent(p_home * q_road, accuracy = 1), " when games 1 and 2 are split, ",
                         scales::percent(p_home^2, accuracy = 1), " when both are home)"),
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# 5e. and the flip side: how often a series reaches game 3
game3 <- bo3 |>
  group_by(fmt) |>
  summarise(series = n(), went3 = sum(ww + wl == 3), pct = went3 / series, .groups = "drop") |>
  mutate(label = fmt_factor(fmt),
         predicted = if_else(fmt == "bo3 2-1 (22-24)",
                             1 - p_home^2 - (1 - p_home)^2,
                             p_home * (1 - q_road) + (1 - p_home) * q_road))

p_game3 <- ggplot(game3, aes(x = label, y = pct)) +
  geom_col(aes(fill = fmt != "bo3 2-1 (22-24)"), width = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(ymin = predicted, ymax = predicted), width = 0.78, linetype = "dashed",
                colour = INK, linewidth = 0.6) +
  geom_text(aes(y = pmax(pct, predicted), label = scales::percent(pct, accuracy = 1)), vjust = -0.5,
            colour = INK, size = 6.4, fontface = "bold") +
  geom_text(aes(y = 0.03, label = paste0(went3, " of\n", series, " series")), colour = "white",
            size = 3.8, vjust = 0, lineheight = 0.9) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = LIGHT)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent, expand = expansion(mult = c(0, 0.02))) +
  labs(title = "1-1-1 means more game 3s",
       subtitle = "share of best of 3 first rounds that went to game 3.\ndashed line: what the math predicts",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme()

# 5f. upsets, every first round format including the single game years
upsets <- first_rounds |>
  mutate(setup = case_when(bestof == 1 & season <= 1999 ~ "1 game\n1997, 99",
                           fmt == "bo3 1-2 (98-09)"     ~ "1-2\n1998-09",
                           fmt == "bo3 1-1-1 (10-15)"   ~ "1-1-1\n2010-15",
                           bestof == 1                  ~ "1 game\n2016-21",
                           fmt == "bo3 2-1 (22-24)"     ~ "2-1\n2022-24",
                           fmt == "bo3 1-1-1 (25)"      ~ "1-1-1\n2025")) |>
  group_by(setup) |>
  summarise(series = n(), lower_won = sum(!higher_won), pct = lower_won / series, .groups = "drop") |>
  mutate(setup = factor(setup, levels = c("1 game\n1997, 99", "1-2\n1998-09", "1-1-1\n2010-15",
                                          "1 game\n2016-21", "2-1\n2022-24", "1-1-1\n2025")))
overall_upset <- mean(!first_rounds$higher_won)

p_upsets <- ggplot(upsets, aes(x = setup, y = pct)) +
  geom_col(aes(fill = setup == "2-1\n2022-24"), width = 0.6, show.legend = FALSE) +
  geom_hline(yintercept = overall_upset, linetype = "dashed", colour = INK, linewidth = 0.6) +
  geom_text(aes(label = paste0(lower_won, " of ", series), colour = setup == "2-1\n2022-24"),
            vjust = -0.5, size = 4.6, fontface = "bold", show.legend = FALSE) +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = LIGHT)) +
  scale_colour_manual(values = c(`TRUE` = HI, `FALSE` = INK)) +
  scale_y_continuous(limits = c(0, 0.4), labels = scales::percent, expand = expansion(mult = c(0, 0.02))) +
  labs(title = "lower seeds get out about 1 in 4 times",
       subtitle = paste0("how often the lower seed won the first round, by format.\ndashed line: all first rounds (",
                         scales::percent(overall_upset, accuracy = 1), ")"),
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(axis.text.x = element_text(size = 10.5))

# 5g. and the exception: every champion's regular season rank (ties share the better rank)
champs <- series |>
  filter(round == "Finals") |>
  select(season, team = winner) |>
  left_join(standings |>
              group_by(season) |>
              mutate(rank = rank(-wpct, ties.method = "min")) |>
              ungroup() |>
              select(season, team, rank),
            by = c("season", "team"))

p_champs <- ggplot(champs, aes(x = rank, y = season)) +
  geom_point(aes(colour = rank > 4), size = 3.6, show.legend = FALSE) +
  annotate("text", x = 5.75, y = 2021, label = "2021 sky\n16-16", hjust = 1, colour = HI,
           size = 4.2, fontface = "bold", lineheight = 0.9) +
  scale_colour_manual(values = c(`TRUE` = HI, `FALSE` = INK)) +
  scale_x_continuous(breaks = 1:6, labels = c("1st", "2nd", "3rd", "4th", "5th", "6th"),
                     limits = c(0.7, 6.3), position = "top") +
  scale_y_reverse(breaks = seq(1997, 2025, 4)) +
  labs(title = "one champion outside the top 4",
       subtitle = "every wnba champion's regular season record, league rank",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#E3E3E0", linewidth = 0.3))

# ============================
# 6. Tables: the two formats side by side
# ============================
nickname <- function(team) sub(".* ", "", team)

round_table <- function(years, title, subtitle) {
  rows <- first_rounds |>
    filter(season %in% years) |>
    left_join(games, by = c("season", "series_idx"), suffix = c("", "_g")) |>
    mutate(higher_team = if_else(w_n < l_n, winner, loser),
           lower_team  = if_else(w_n < l_n, loser, winner),
           higher_seed = pmin(w_n, l_n), lower_seed = pmax(w_n, l_n),
           hi_pts = if_else(home == higher_team, home_pts, away_pts),
           lo_pts = if_else(home == higher_team, away_pts, home_pts),
           cell = paste0(if_else(hi_pts > lo_pts, "W ", "L "), hi_pts, "-", lo_pts,
                         if_else(home == higher_team, " (home)", " (road)")),
           matchup = paste0(higher_seed, " ", nickname(higher_team), " vs ",
                            lower_seed, " ", nickname(lower_team)),
           winner_label = paste0(nickname(winner), " ", ww, "-", wl),
           went_to_game3 = ww + wl == 3) |>
    select(season, matchup, winner_label, went_to_game3, game, cell) |>
    pivot_wider(names_from = game, values_from = cell, names_prefix = "game ") |>
    arrange(season, matchup)

  rows |>
    select(season, matchup, `game 1`, `game 2`, `game 3`, winner_label, went_to_game3) |>
    gt(groupname_col = if (length(years) > 1) "season" else NULL) |>
    cols_hide(c(went_to_game3, if (length(years) == 1) "season")) |>
    sub_missing(missing_text = "") |>
    cols_label(matchup = "", winner_label = "series") |>
    tab_header(title = md(paste0("**", title, "**")), subtitle = subtitle) |>
    tab_style(style = cell_fill(color = "#F6DCE0"), locations = cells_body(rows = went_to_game3)) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = winner_label)) |>
    tab_source_note("game results from the higher seed's side. data: basketball reference, seeds: wikipedia") |>
    tab_options(heading.align = "left", table.font.color = INK, table.font.size = px(14),
                heading.title.font.weight = "bold", row_group.font.weight = "bold",
                column_labels.font.weight = "bold", source_notes.font.size = px(11))
}

t_2022_2024 <- round_table(2022:2024, "first round playoff results, 2022-2024",
                           "9 of 12 were sweeps. highlighted: went to game 3")
t_2025      <- round_table(2025, "first round playoff results, 2025",
                           "only 1 of 4 was a sweep. highlighted: went to game 3")

# render one at a time
p_home_road
p_schedule
p_sweep
p_game3
p_upsets
p_champs
t_2022_2024
t_2025
