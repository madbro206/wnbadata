# teams up 2-0 in a wnba best of 5 (wehoop / espn, playoffs 2002-2026)
#
# 1. every best of 5 series, and the 19 that started 2-0 (all 19 won by the team that led)
# 2. the napkin math: if every game is a coin flip, how unlikely is 0 comebacks in 19?
# 3. how much worse are the teams that go down 0-2? (regular season record and point margin)
#
# needs the arrow package installed, or wehoop returns empty rows for older seasons.

library(wehoop)
library(dplyr)
library(tidyr)
library(ggplot2)

schedule <- as_tibble(load_wnba_schedule(seasons = 2002:2026))

# playoff games from the last day aren't in wehoop's nightly data yet, so fill them in from espn
new_ids <- schedule %>%
  filter(season_type == 3, status_type_name != "STATUS_FINAL", home_abbreviation != "TBD",
         as.POSIXct(date, format = "%Y-%m-%dT%H:%MZ", tz = "UTC") < Sys.time()) %>%
  pull(game_id)

for (id in new_ids) {
  b <- tryCatch(as_tibble(espn_wnba_team_box(game_id = id)), error = function(e) NULL)
  if (!any(b[["team_winner"]] %in% TRUE)) next                  # not final yet
  i <- schedule$game_id == id
  schedule$home_score[i] <- b$team_score[b$team_home_away == "home"]
  schedule$away_score[i] <- b$team_score[b$team_home_away == "away"]
  schedule$status_type_name[i] <- "STATUS_FINAL"
}

# regular season strength: record and average point margin -----------------------------

real_teams <- schedule %>%                                # drops all-star "teams"
  filter(season_type == 2) %>%
  pivot_longer(c(home_id, away_id), values_to = "team_id") %>%
  count(season, team_id) %>%
  filter(n >= 10)

reg <- schedule %>%
  filter(season_type == 2, status_type_name == "STATUS_FINAL",
         !notes_headline %in% "WNBA Commissioner's Cup Championship") %>%
  semi_join(real_teams, by = c("season", home_id = "team_id")) %>%
  semi_join(real_teams, by = c("season", away_id = "team_id")) %>%
  mutate(margin = as.numeric(home_score) - as.numeric(away_score))

team_season <- bind_rows(
  reg %>% transmute(season, team = home_display_name, margin),
  reg %>% transmute(season, team = away_display_name, margin = -margin)) %>%
  group_by(season, team) %>%
  summarise(w = sum(margin > 0), l = sum(margin < 0), pt_margin = mean(margin), .groups = "drop") %>%
  mutate(win_pct = w / (w + l))

# every playoff series -------------------------------------------------------------------

po <- schedule %>%
  filter(season_type == 3, status_type_name == "STATUS_FINAL") %>%
  transmute(season, date = as.POSIXct(date, format = "%Y-%m-%dT%H:%MZ", tz = "UTC"),
            round = notes_headline, home = home_display_name, away = away_display_name,
            margin = as.numeric(home_score) - as.numeric(away_score)) %>%
  mutate(winner = ifelse(margin > 0, home, away),
         a = pmin(home, away), b = pmax(home, away)) %>%
  arrange(date) %>%
  group_by(season, a, b) %>%
  mutate(game = row_number()) %>%
  ungroup()

series <- po %>%
  group_by(season, a, b) %>%
  summarise(games = n(), a_wins = sum(winner == a), b_wins = sum(winner == b),
            g1_home = first(home), g1_winner = first(winner), g2_winner = nth(winner, 2),
            round = first(round), .groups = "drop") %>%
  mutate(winner = ifelse(a_wins > b_wins, a, b),
         best_of = 2 * pmax(a_wins, b_wins) - 1)              # winner's wins tell you the length

best_of_5 <- series %>% filter(best_of == 5)                    # finished series only

up_2_0 <- best_of_5 %>%
  filter(g1_winner == g2_winner) %>%
  transmute(season, round, leader = g1_winner, trailer = ifelse(leader == a, b, a),
            leader_won = winner == leader,
            final = paste0(pmax(a_wins, b_wins), "-", pmin(a_wins, b_wins)), games,
            leader_hosted_g1 = g1_home == leader)

cat(nrow(best_of_5), "finished best of 5 series,", nrow(up_2_0), "started 2-0,",
    sum(up_2_0$leader_won), "won by the team up 2-0\n")
print(count(up_2_0, final))

# the napkin math ------------------------------------------------------------------------
# a comeback = the trailing team wins 3 straight, so p^3 if each game is independent with
# win chance p. comebacks in 19 series ~ binomial(19, p^3), and 0 comebacks = (1 - p^3)^19.

# what trailing teams have actually done after going down 0-2: games 3, 4 and 5
after_2_0 <- po %>%
  inner_join(up_2_0 %>% select(season, leader, trailer), by = "season", relationship = "many-to-many") %>%
  filter((home == leader & away == trailer) | (home == trailer & away == leader), game >= 3) %>%
  mutate(trailer_won = winner == trailer, trailer_home = home == trailer)

p_actual <- mean(after_2_0$trailer_won)
cat("\ntrailing teams after 0-2:", sum(after_2_0$trailer_won), "wins in", nrow(after_2_0),
    "games (", round(100 * p_actual), "% )\n")

# comebacks in 19 tries if every game is a coin flip
comebacks <- tibble(k = 0:19, prob = dbinom(0:19, 19, 0.5^3)) %>%
  mutate(k_label = factor(ifelse(k >= 6, "6+", as.character(k)), levels = c(0:5, "6+"))) %>%
  group_by(k_label) %>%
  summarise(prob = sum(prob))
print(comebacks %>% mutate(prob = round(prob, 3)))
cat("0 comebacks in 19 at the trailing teams' actual rate:", round((1 - p_actual^3)^19, 3), "\n")

# how much worse are the teams that go down 0-2? -----------------------------------------

gap <- up_2_0 %>%
  left_join(team_season %>% rename_with(~ paste0("leader_", .x), -c(season, team)),
            by = c("season", leader = "team")) %>%
  left_join(team_season %>% rename_with(~ paste0("trailer_", .x), -c(season, team)),
            by = c("season", trailer = "team")) %>%
  mutate(leader_record = paste0(leader_w, "-", leader_l),
         trailer_record = paste0(trailer_w, "-", trailer_l),
         margin_gap = leader_pt_margin - trailer_pt_margin,
         trailer_worse_record = trailer_win_pct < leader_win_pct,
         trailer_worse_margin = trailer_pt_margin < leader_pt_margin)

print(gap %>% transmute(season, leader, leader_record, trailer, trailer_record,
                        margin_gap = round(margin_gap, 1), leader_hosted_g1, final), n = Inf, width = 200)

cat("\ntrailing team had the worse record:", sum(gap$trailer_worse_record), "of", nrow(gap),
    "(ties:", sum(gap$trailer_win_pct == gap$leader_win_pct), ")\n")
cat("trailing team had the worse point margin:", sum(gap$trailer_worse_margin), "of", nrow(gap), "\n")
cat("trailing team did not have home court (didn't host game 1):", sum(gap$leader_hosted_g1), "of", nrow(gap), "\n")
cat("average regular season point margin gap:", round(mean(gap$margin_gap), 1), "points a game\n")

# compare: in best of 5 series that did NOT start 2-0, how far apart were the teams?
not_2_0 <- best_of_5 %>%
  filter(g1_winner != g2_winner) %>%
  left_join(team_season %>% select(season, team, a_margin = pt_margin), by = c("season", a = "team")) %>%
  left_join(team_season %>% select(season, team, b_margin = pt_margin), by = c("season", b = "team"))
cat("average point margin gap in series that started 1-1:", round(mean(abs(not_2_0$a_margin - not_2_0$b_margin)), 1), "\n")

# do the regular season numbers predict 7 of 26 by themselves? expected trailer wins in the
# games actually played after 0-2, from the point margin gap plus home court, game noise sd 11.5
hca <- 2.5
expected <- after_2_0 %>%
  left_join(gap %>% select(season, leader, trailer, margin_gap), by = c("season", "leader", "trailer")) %>%
  mutate(p_trailer = pnorm((-margin_gap + ifelse(trailer_home, hca, -hca)) / 11.5))
cat("expected trailer wins from regular season strength:", round(sum(expected$p_trailer), 1),
    "of", nrow(expected), "(actual", sum(expected$trailer_won), ")\n")

# the table: every best of 5 that started 2-0 ------------------------------------------

nickname <- function(team) sub("^(Los Angeles|Las Vegas|New York|San Antonio|Golden State|[A-Za-z]+) ", "", team)

two_oh_table <- po %>%
  inner_join(up_2_0 %>% select(season, leader, trailer, final), by = "season", relationship = "many-to-many") %>%
  filter((home == leader & away == trailer) | (home == trailer & away == leader)) %>%
  arrange(season, leader, game) %>%
  group_by(season, leader, trailer, final) %>%
  summarise(round = ifelse(grepl("final", first(round), ignore.case = TRUE) & !grepl("semi", first(round), ignore.case = TRUE),
                           "finals", "semis"),
            games = paste(ifelse(winner == leader, "W", "L"), collapse = " "),
            .groups = "drop") %>%
  left_join(gap %>% select(season, leader, trailer, leader_record, trailer_record), by = c("season", "leader", "trailer")) %>%
  transmute(season, round, up_2_0 = nickname(leader), record = leader_record,
            down_0_2 = nickname(trailer), their_record = trailer_record,
            games, series = final) %>%
  arrange(season)

print(two_oh_table, n = Inf, width = 200)

# the 13 that were sweeps
sweeps <- two_oh_table %>% filter(series == "3-0")
print(sweeps, n = Inf, width = 200)

# every time the aces have been down 0-2 in a best of 5, including this year -------------
# 2026 semis are best of 5 but unfinished, so they're added by hand. the same franchise was the
# san antonio silver stars in 2008 (swept by detroit in the finals); the script says "the aces", so
# that one isn't counted here

aces_down_0_2 <- po %>%
  filter(a == "Las Vegas Aces" | b == "Las Vegas Aces") %>%
  arrange(date) %>%
  group_by(season, a, b) %>%
  summarise(opponent = ifelse(first(a) == "Las Vegas Aces", first(b), first(a)),
            round = ifelse(grepl("semi", first(round), ignore.case = TRUE), "semis", "finals"),
            games = paste(ifelse(winner == "Las Vegas Aces", "W", "L"), collapse = " "),
            aces_wins = sum(winner == "Las Vegas Aces"), opp_wins = sum(winner != "Las Vegas Aces"),
            down_0_2 = n() >= 2 && all(winner[1:2] != "Las Vegas Aces"),
            .groups = "drop") %>%
  left_join(series %>% select(season, a, b, best_of), by = c("season", "a", "b")) %>%
  mutate(best_of = ifelse(season == 2026 & round == "semis", 5, best_of)) %>%
  filter(down_0_2, best_of == 5) %>%
  transmute(season, round, opponent = nickname(opponent), games,
            result = ifelse(season == 2026, paste0("down ", aces_wins, "-", opp_wins, ", still going"),
                            paste0("lost ", aces_wins, "-", opp_wins)))

print(aces_down_0_2)

# charts --------------------------------------------------------------------------------

blue <- "#3b6ea8"
gold <- "#c9a227"

# coin flip odds: if you replayed all 19 series 100 times, how many of those 100 replays end
# with 0 comebacks, 1 comeback, and so on (each bar = the binomial probability x 100)
p_coinflip <- ggplot(comebacks, aes(x = k_label, y = prob, fill = k_label == "0")) +
  geom_col(width = 0.7) +
  geom_text(aes(label = round(100 * prob)), vjust = -0.4, size = 5) +
  scale_fill_manual(values = c(`TRUE` = gold, `FALSE` = blue), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "comebacks from 0-2, out of 19 tries", y = NULL,
       title = "if every game were a coin flip",
       subtitle = "replay all 19 series 100 times. 0 comebacks happens 8 times") +
  theme_minimal(base_size = 14) +
  theme(plot.title.position = "plot", axis.text.y = element_blank(), panel.grid = element_blank())

# the napkin math: down 0-2, a comeback means winning games 3, 4 and 5, each a coin flip
math_tiles <- tibble(x = c(1, 3, 5), game = paste("win game", 3:5))
math_ops <- tibble(x = c(2, 4, 6), op = c("\u00d7", "\u00d7", "="))

p_math <- ggplot() +
  geom_tile(data = math_tiles, aes(x = x, y = 0), width = 1.6, height = 1.6, fill = blue) +
  geom_text(data = math_tiles, aes(x = x, y = 0.35, label = game), color = "white", size = 4.5) +
  geom_text(data = math_tiles, aes(x = x, y = -0.15, label = "1/2"), color = "white", size = 11, fontface = "bold") +
  geom_text(data = math_ops, aes(x = x, y = 0, label = op), size = 10) +
  geom_tile(aes(x = 7.2, y = 0), width = 2, height = 1.6, fill = gold) +
  geom_text(aes(x = 7.2, y = 0.3, label = "1/8"), size = 11, fontface = "bold") +
  geom_text(aes(x = 7.2, y = -0.4, label = "12.5%"), size = 6) +
  coord_equal(xlim = c(0, 8.3), ylim = c(-1, 1.3)) +
  labs(title = "coming back from 0-2 if every game were a coin flip") +
  theme_void(base_size = 14) +
  theme(plot.title.position = "plot", plot.margin = margin(10, 10, 10, 10))

# 19-0: one tile per series that started 2-0
p_record <- two_oh_table %>%
  mutate(i = row_number() - 1, col = i %% 4, row = -(i %/% 4),
         label = paste0(season, "\n", tolower(up_2_0))) %>%
  ggplot(aes(x = col, y = row)) +
  geom_tile(fill = gold, color = "white", linewidth = 2) +
  geom_text(aes(label = label), size = 3.6, lineheight = 0.9) +
  coord_equal() +
  labs(title = "teams up 2-0 in a best of 5: 19-0",
       subtitle = "every one of them won the series") +
  theme_void(base_size = 14) +
  theme(plot.title.position = "plot", plot.margin = margin(10, 10, 10, 10))

# how the 19 series ended
endings <- count(two_oh_table, series) %>%
  mutate(label = c(`3-0` = "swept", `3-1` = "in 4", `3-2` = "in 5")[series])

p_endings <- ggplot(endings, aes(x = label, y = n, fill = series == "3-2")) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n), vjust = -0.4, size = 6) +
  scale_x_discrete(limits = c("swept", "in 4", "in 5")) +
  scale_fill_manual(values = c(`TRUE` = gold, `FALSE` = blue), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = NULL, y = NULL, title = "how the 19 series ended") +
  theme_minimal(base_size = 14) +
  theme(plot.title.position = "plot", axis.text.y = element_blank(), panel.grid = element_blank())

# one series game by game: who won each game and the score
series_strip <- function(yr, team_a, team_b) {
  po %>%
    filter(season == yr, (home == team_a & away == team_b) | (home == team_b & away == team_a)) %>%
    mutate(winner_nick = tolower(nickname(winner)),
           score = paste0(pmax(abs(margin), 0), " pt win"),
           score = paste0(winner_nick, "\nby ", abs(margin))) %>%
    ggplot(aes(x = factor(paste("game", game), levels = paste("game", 1:5)), y = 1, fill = winner == team_a)) +
    geom_tile(color = "white", linewidth = 2, height = 0.8) +
    geom_text(aes(label = score), color = "white", size = 4.5, lineheight = 0.9) +
    scale_fill_manual(values = c(`TRUE` = gold, `FALSE` = blue), guide = "none") +
    scale_x_discrete(drop = FALSE) +                               # keep empty slots up to game 5
    labs(x = NULL, y = NULL, title = paste(yr, tolower(nickname(team_a)), "vs", tolower(nickname(team_b)))) +
    theme_minimal(base_size = 14) +
    theme(plot.title.position = "plot", axis.text.y = element_blank(), panel.grid = element_blank())
}

p_2018 <- series_strip(2018, "Seattle Storm", "Phoenix Mercury")          # the only game 5
p_liberty_2023 <- series_strip(2023, "Las Vegas Aces", "New York Liberty") # liberty's last 0-2
p_dream_2026 <- series_strip(2026, "Atlanta Dream", "New York Liberty")       # this year so far
p_valks_2026 <- series_strip(2026, "Golden State Valkyries", "Las Vegas Aces")

# chart index ---------------------------------------------------------------------------
# p_record         19 tiles, one per series that started 2-0 (the hook)
# p_endings        how the 19 ended: 13 swept, 5 in 4, 1 in 5
# p_2018           storm vs mercury 2018 game by game, the only game 5
# p_liberty_2023   aces vs liberty 2023 finals game by game
# p_dream_2026     dream vs liberty 2026 so far
# p_valks_2026     valkyries vs aces 2026 so far
# p_math           1/2 x 1/2 x 1/2 = 1/8 = 12.5%, the napkin math
# p_coinflip       comebacks in 19 tries if every game is a coin flip, 0 in gold (8%)
# two_oh_table     the 19 series as a tibble (printed above)
# sweeps           the 13 of the 19 that ended 3-0 (printed above)
# aces_down_0_2    every aces 0-2 deficit in a best of 5, including 2026 (printed above)

p_record
p_endings
p_2018
p_liberty_2023
p_dream_2026
p_valks_2026
p_math
p_coinflip
two_oh_table
