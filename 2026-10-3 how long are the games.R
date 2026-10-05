# ============================================================
# How long is a WNBA game, 1997-2026 (tip to final buzzer)
# Chart 1: average game length by season, regular season vs playoffs
# Chart 2: share of regular season games that finished under 2 hours
# Chart 3: 2026 playoff games scheduled 2 hours apart
# Chart 4: every 2026 game, regular season vs playoffs (wehoop, live)
# Chart 5: every 2026 playoff game on a clock, scheduled vs actual, with overlaps
# Chart 6: zoomed in on each 2 hours apart handoff
# Chart 7: fouls vs game length, every regular season game since 1997
# Chart 8: same thing, one dot per season
# Chart 9: fouls per game and per 100 possessions by season
# Chart 10: the worst overlap, fever vs aces then liberty vs lynx, 9/29
# Chart 11: how late 2026 games tipped off, regular season vs playoffs (wehoop, live)
# ============================================================

library(tidyverse)

# ---- 1. DATA: HHS HISTORY (hard-coded) -------------------------
# Hard-coded from Her Hoop Stats, WNBA, pulled 10/3. Game length is the
# official tip to final buzzer time, in whole minutes.
# Checked against the wehoop wallclock method on all ten 2026 playoff games:
# 8 within 1 minute, the two Dallas/Golden State games off by 2 (167/169, 135/137).
# wehoop pbp only has wallclock from 2013 on, so 1997-2012 is HHS only.
#
#   reg_avg   = mean minutes, regular season (OT games included)
#   reg_u2h   = % of regular season games under 120 minutes
#   pst_avg   = mean minutes, playoffs
# 2026 playoffs = all 12 games through semis game 1 (10/4), official game
# lengths. (1311 + 125 + 115) / 12 = 129.25. Includes a 167 min OT game.

history <- tribble(
  ~season, ~reg_avg, ~reg_u2h, ~pst_avg,
  1997, 116.0, 70.6, 118.0,
  1998, 115.7, 78.9, 113.8,
  1999, 115.1, 75.9, 114.5,
  2000, 115.9, 71.4, 115.8,
  2001, 114.8, 79.9, 115.1,
  2002, 116.1, 72.8, 119.9,
  2003, 115.0, 76.4, 121.2,
  2004, 116.8, 67.0, 119.3,
  2005, 116.6, 69.1, 124.9,
  2006, 116.8, 68.2, 122.7,
  2007, 119.1, 67.4, 123.6,
  2008, 120.0, 54.9, 126.1,
  2009, 120.0, 62.4, 126.1,
  2010, 119.5, 56.4, 126.0,
  2011, 116.9, 68.1, 123.3,
  2012, 116.6, 70.6, 122.8,
  2013, 116.2, 70.1, 120.4,
  2014, 116.8, 64.2, 125.0,
  2015, 116.4, 65.7, 122.4,
  2016, 117.9, 60.8, 123.1,
  2017, 116.4, 74.0, 125.7,
  2018, 112.7, 81.7, 119.4,
  2019, 113.9, 77.8, 117.1,
  2020, 117.9, 66.4, 114.0,
  2021, 114.8, 72.1, 119.3,
  2022, 116.7, 67.8, 116.5,
  2023, 121.4, 43.0, 119.8,
  2024, 119.6, 56.1, 120.8,
  2025, 120.7, 47.0, 129.0,
  2026, 125.6, 27.5, 129.3   # 2026 playoffs: 12 games through 10/4
)

# printable checks
history %>% arrange(desc(reg_avg)) %>% head(5)     # 2026 longest regular season
history %>% arrange(reg_u2h) %>% head(5)           # 2026 fewest under-2-hour games
history %>%
  filter(season <= 2025) %>%
  summarize(seasons = n(),
            playoffs_longer = sum(pst_avg > reg_avg),
            avg_gap_min = mean(pst_avg - reg_avg))

# ---- 2. DATA: 2026 PLAYOFF DOUBLEHEADERS (hard-coded) ------------
# wehoop pbp wallclock (first event to last event) + wehoop schedule start
# time, typed in ET (as pulled), shifted to PT below. Only pairs scheduled
# exactly 2 hours apart. Pair labels are already PT.

dh <- tribble(
  ~pair,              ~slot,    ~game,          ~sched,  ~tip,    ~end,
  "Sun 9/27, 11am/1pm", "first",  "MIN vs NY",    "14:00", "14:04", "16:06",
  "Sun 9/27, 11am/1pm", "second", "LV vs IND",    "16:00", "16:12", "18:16",
  "Sun 9/27, 4pm/6pm", "first",  "ATL vs WSH",   "19:00", "19:09", "21:16",
  "Sun 9/27, 4pm/6pm", "second", "GS vs DAL",    "21:00", "21:08", "23:06",
  "Tue 9/29, 3:30/5:30", "first",  "IND vs LV",  "18:30", "18:37", "20:47",
  "Tue 9/29, 3:30/5:30", "second", "NY vs MIN",  "20:30", "20:38", "22:34",
  "Wed 9/30, 4pm/6pm", "first",  "WSH vs ATL",   "19:00", "19:04", "21:15",
  "Wed 9/30, 4pm/6pm", "second", "DAL vs GS",    "21:00", "21:12", "00:01",
  # semis game 1, from the official play-by-play (opening tip to final buzzer)
  "Sun 10/4, 11am/1pm", "first",  "ATL vs NY",    "14:00", "14:04", "16:09",
  "Sun 10/4, 11am/1pm", "second", "GS vs LV",     "16:00", "16:03", "17:59"
) %>%
  mutate(across(c(sched, tip, end), ~ as.numeric(hms::parse_hm(.x)) / 3600),
         end = if_else(end < tip, end + 24, end),   # 9/30 DAL-GS went to OT, past midnight ET
         across(c(sched, tip, end), ~ .x - 3),      # ET -> PT
         pair = fct_inorder(pair))

# overlap check: did game 1 end after game 2 tipped?
dh %>%
  select(pair, slot, tip, end) %>%
  pivot_wider(names_from = slot, values_from = c(tip, end)) %>%
  mutate(overlap_min = round((end_first - tip_second) * 60))

# ---- 3. COLORS & THEME ------------------------------------------
# Same validated CVD-safe pair as the Shepard videos.
type_colors <- c(`regular season` = "#2a78d6", playoffs = "#eb6834")

theme_gl <- theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey40", size = 11),
    plot.caption = element_text(color = "grey55", size = 9),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    axis.title = element_text(color = "grey40", size = 10)
  )

# ---- 4. CHART 1: average length by season -------------------------

hist_long <- history %>%
  pivot_longer(c(reg_avg, pst_avg), names_to = "type", values_to = "minutes") %>%
  mutate(type = if_else(type == "reg_avg", "regular season", "playoffs"))

chart_history <- ggplot(hist_long, aes(season, minutes, color = type)) +
  geom_hline(yintercept = 120, linetype = "dashed", color = "grey60") +
  annotate("text", x = 1997, y = 120.6, label = "2 hours", hjust = 0,
           size = 3.5, color = "grey45") +
  geom_line(linewidth = 1) +
  geom_point(data = filter(hist_long, season == 2026), size = 3) +
  geom_text(data = filter(hist_long, season == 2026),
            aes(label = sprintf("%.1f", minutes)), hjust = -0.3,
            size = 4, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = type_colors) +
  scale_x_continuous(breaks = seq(1997, 2026, 4), limits = c(1997, 2028)) +
  labs(title = "WNBA games have never been longer",
       subtitle = "average minutes from tipoff to final buzzer",
       x = NULL, y = "minutes",
       caption = "data: Her Hoop Stats | 2026 playoffs through 10/4 | @wnbadata") +
  theme_gl

# ---- 5. CHART 2: % of regular season games under 2 hours -----------

chart_under2 <- history %>%
  mutate(highlight = season %in% c(2019, 2026)) %>%
  ggplot(aes(season, reg_u2h, fill = highlight)) +
  geom_col(width = 0.75) +
  geom_text(data = ~ filter(.x, highlight),
            aes(label = paste0(round(reg_u2h), "%")),
            vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = c(`TRUE` = "#2a78d6", `FALSE` = "grey75"), guide = "none") +
  scale_y_continuous(limits = c(0, 100), labels = \(x) paste0(x, "%")) +
  scale_x_continuous(breaks = seq(1997, 2026, 4)) +
  labs(title = "the 2 hour game is disappearing",
       subtitle = "share of regular season games that finished in under 2 hours",
       x = NULL, y = NULL,
       caption = "data: Her Hoop Stats | @wnbadata") +
  theme_gl +
  theme(panel.grid.major.x = element_blank())

# ---- 6. CHART 3: 2026 playoff doubleheaders ------------------------

hour_lab <- \(x) {
  h <- floor(x) %% 24; m <- round((x %% 1) * 60)
  sprintf("%d:%02d", if_else(h > 12, h - 12, if_else(h == 0, 12, h)), m)
}

chart_doubleheaders <- ggplot(dh) +
  geom_segment(aes(x = tip, xend = end, y = slot, yend = slot, color = slot),
               linewidth = 7, lineend = "round") +
  geom_point(aes(x = sched, y = slot), shape = 124, size = 7, color = "grey30") +
  geom_text(aes(x = tip, y = slot, label = game), vjust = -1.4, hjust = 0,
            size = 3.4, color = "grey30") +
  facet_wrap(~pair, ncol = 1, scales = "free_x") +
  scale_color_manual(values = c(first = "#2a78d6", second = "#eb6834"), guide = "none") +
  scale_x_continuous(labels = hour_lab) +
  scale_y_discrete(limits = c("second", "first")) +
  labs(title = "2 hours apart is not enough",
       subtitle = "2026 playoffs, pacific time. bar = tipoff to final buzzer, tick = scheduled start",
       x = NULL, y = NULL,
       caption = "data: wehoop | @wnbadata") +
  theme_gl +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0))

# ---- 7. CHART 4: every 2026 game (wehoop, live) ----------------------
# Your original method: first wallclock to last wallclock. Cleaning drops
# the All-Star game and pbp with broken wallclocks (gap > 40 min between
# plays, under 90 / over 240 minutes, or out-of-order timestamps at the end).
# 39 of 331 regular season games drop out; the HHS average (125.6) and this
# one (125.7) still agree. All 10 playoff games survive.

pbp_26 <- wehoop::load_wnba_pbp(seasons = 2026) %>%
  filter(season_type %in% c(2, 3)) %>%
  mutate(wc = as.POSIXct(wallclock, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))

lengths_26 <- pbp_26 %>%
  filter(!is.na(wc), !str_detect(home_team_name, "^Team "), !str_detect(away_team_name, "^Team ")) %>%
  arrange(game_id, sequence_number) %>%
  group_by(game_id) %>%
  summarize(type = if_else(first(season_type) == 3, "playoffs", "regular season"),
            date = first(game_date), home = first(home_team_name), away = first(away_team_name),
            ot = max(period_number) > 4,
            minutes = as.numeric(difftime(last(wc), first(wc), units = "mins")),
            maxgap = max(diff(as.numeric(wc))) / 60,
            # last play vs latest timestamp in the final period: if these
            # disagree the wallclocks are out of order and the length is junk
            minutes_mx = as.numeric(difftime(max(wc[period_number == max(period_number)]),
                                             first(wc), units = "mins")),
            .groups = "drop") %>%
  filter(minutes >= 90, minutes <= 240, maxgap <= 40, abs(minutes - minutes_mx) <= 3)

lengths_26 %>%
  group_by(type) %>%
  summarize(games = n(), avg = mean(minutes), avg_no_ot = mean(minutes[!ot]),
            under_2h = sum(minutes < 120), pct_under_2h = 100 * mean(minutes < 120))

chart_2026 <- ggplot(lengths_26, aes(minutes, fill = type)) +
  geom_histogram(binwidth = 3, boundary = 120, color = "white") +
  geom_vline(xintercept = 120, linetype = "dashed", color = "grey40") +
  facet_wrap(~ fct_rev(type), ncol = 1, scales = "free_y") +
  scale_fill_manual(values = type_colors, guide = "none") +
  scale_y_continuous(breaks = \(l) seq(0, floor(l[2]), by = max(1, round(diff(l) / 4, -1)))) +
  coord_cartesian(xlim = c(100, 175)) +
  labs(title = "how long every 2026 game took",
       subtitle = "minutes from tipoff to final buzzer. dashed line = 2 hours",
       x = "minutes", y = "games",
       caption = "data: wehoop | @wnbadata") +
  theme_gl +
  theme(strip.text = element_text(face = "bold", hjust = 0))

# ---- 7b. CHART 5: playoff timeline, scheduled vs actual ---------------
# Every 2026 playoff game on one clock, typed in ET as pulled, shown in PT. Hollow dot = scheduled start,
# dotted = wait until the real tip, solid = tipoff to final buzzer.
# Shaded = the next game had tipped while the earlier one was still going.
# Times to the second: wehoop pbp wallclock for 9/27 to 10/2 (first play to
# last play), the official play-by-play for 10/4 (opening tip to final buzzer).

playoff_times <- tribble(
  ~day,               ~game,         ~sched,     ~tip,       ~end,
  "Sun 9/27",         "MIN vs NY",   "14:00:00", "14:04:19", "16:06:27",
  "Sun 9/27",         "LV vs IND",   "16:00:00", "16:12:53", "18:16:40",
  "Sun 9/27",         "ATL vs WSH",  "19:00:00", "19:09:05", "21:16:48",
  "Sun 9/27",         "GS vs DAL",   "21:00:00", "21:08:43", "23:06:07",
  "Tue 9/29",         "IND vs LV",   "18:30:00", "18:37:59", "20:47:37",
  "Tue 9/29",         "NY vs MIN",   "20:30:00", "20:38:56", "22:34:52",
  "Wed 9/30",         "WSH vs ATL",  "19:00:00", "19:04:46", "21:15:08",
  "Wed 9/30",         "DAL vs GS (OT)", "21:00:00", "21:12:59", "00:01:34",
  "Thu 10/1",         "LV vs IND",   "21:00:00", "21:08:10", "23:32:15",
  "Fri 10/2",         "GS vs DAL",   "21:00:00", "21:04:37", "23:21:09",
  "Sun 10/4 (semis)", "ATL vs NY",   "14:00:00", "14:04:22", "16:09:19",
  "Sun 10/4 (semis)", "GS vs LV",    "16:00:00", "16:03:44", "17:59:20"
) %>%
  mutate(across(c(sched, tip, end), ~ as.numeric(hms::as_hms(.x)) / 3600),
         end = if_else(end < tip, end + 24, end),   # OT game ended after midnight ET
         across(c(sched, tip, end), ~ .x - 3),      # ET -> PT
         day = fct_inorder(day)) %>%
  group_by(day) %>%
  arrange(sched, .by_group = TRUE) %>%
  mutate(lane = (row_number() - 1) %% 2,           # alternate lanes so back-to-backs don't collide
         prev_end = lag(end)) %>%
  ungroup()

# each back-to-back pair: positive = overlap, negative = time to spare
fmt_ms <- \(h) sprintf("%d:%02d", floor(abs(h) * 60), round((abs(h) * 3600) %% 60))
overlaps <- playoff_times %>%
  filter(!is.na(prev_end), prev_end > tip - 0.5) %>%   # back-to-backs only, not the 3 hour gaps
  transmute(day, xmin = pmin(tip, prev_end), xmax = pmax(tip, prev_end),
            overlap = prev_end > tip,
            label = if_else(overlap, paste(fmt_ms(prev_end - tip), "overlap"),
                                     paste(fmt_ms(tip - prev_end), "gap")))
overlaps

clock_lab <- \(x) {
  h <- x %% 24
  paste0(if_else(h %% 12 == 0, 12, h %% 12), if_else(h < 12, "am", "pm"))
}

# On-screen version: short title, no subtitle, no matchup labels. One direct
# label ("scheduled start") on the first hollow dot explains the dots; the
# "overlap" / "gap" labels explain the shading.
chart_timeline <- ggplot(playoff_times) +
  geom_rect(data = filter(overlaps, overlap),
            aes(xmin = xmin, xmax = xmax, ymin = -0.6, ymax = 1.6),
            fill = "#eb6834", alpha = 0.35) +
  geom_text(data = overlaps,
            aes(x = (xmin + xmax) / 2, y = 2.1, label = label),
            size = 3.5, fontface = "bold", color = "grey15") +
  geom_segment(aes(x = sched, xend = tip, y = lane, yend = lane),
               linetype = "dotted", linewidth = 0.6, color = "grey45") +
  geom_segment(aes(x = tip, xend = end, y = lane, yend = lane),
               linewidth = 2.6, color = "#2a78d6", lineend = "round") +
  geom_point(aes(x = sched, y = lane), shape = 21, size = 2.4,
             fill = "white", color = "grey30", stroke = 0.8) +
  geom_text(data = ~ slice(.x, 1),
            aes(x = sched, y = lane + 0.55, label = "scheduled start"),
            hjust = 0, size = 3, color = "grey35") +
  facet_wrap(~day, ncol = 1, labeller = as_labeller(\(x) str_remove(x, " \\(semis\\)"))) +
  scale_x_continuous(breaks = 11:21, labels = clock_lab,
                     expand = expansion(add = c(0.15, 0.15))) +
  scale_y_continuous(limits = c(-0.7, 2.4)) +
  labs(title = "2026 playoffs, pacific time", x = NULL, y = NULL,
       caption = "data: wehoop, Her Hoop Stats | @wnbadata") +
  theme_gl +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 18),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey92"),
        strip.text = element_text(face = "bold", hjust = 0, size = 11),
        panel.spacing.y = unit(4, "pt"))

# ---- 7c. CHART 6: the handoffs, zoomed in -----------------------------
# Same data as chart 5, but only the back-to-backs, and the x axis is
# minutes from the second game's scheduled start. 0 = when game 2 was
# supposed to start. Top line = game 1 finishing, bottom = game 2 starting.

clock_hm <- \(x) {
  h <- floor(x) %% 24; m <- round((x %% 1) * 60)
  paste0(if_else(h %% 12 == 0, 12, h %% 12), if_else(m == 0, "", sprintf(":%02d", m)),
         if_else(h < 12, "am", "pm"))
}

handoffs <- playoff_times %>%
  group_by(day) %>%
  mutate(next_game = lead(game), next_sched = lead(sched), next_tip = lead(tip)) %>%
  ungroup() %>%
  filter(!is.na(next_sched), next_sched - sched <= 2.01) %>%   # exactly 2 hours apart
  transmute(pair = paste0(str_remove(day, " \\(semis\\)"), ", ", clock_hm(next_sched)),   # e.g. "Sun 9/27, 1pm"
            end1 = (end - next_sched) * 60,        # minutes vs game 2's scheduled start
            tip2 = (next_tip - next_sched) * 60,
            overlap = end1 > tip2,
            label = if_else(overlap, paste(fmt_ms((end1 - tip2) / 60), "overlap"),
                                     paste(fmt_ms((tip2 - end1) / 60), "gap"))) %>%
  mutate(pair = fct_rev(fct_inorder(pair)))   # chart flips it back so 9/27 is on top
handoffs

# On-screen version: short title, no subtitle. Direct labels on the top
# panel ("game 1" / "game 2") replace the color legend. Panel labels are the
# date + the second game's scheduled start, pacific time.
chart_handoff <- ggplot(handoffs) +
  facet_wrap(~ fct_rev(pair), ncol = 1) +
  geom_rect(data = filter(handoffs, overlap),
            aes(xmin = tip2, xmax = end1, ymin = -0.35, ymax = 1.35),
            fill = "#eb6834", alpha = 0.35) +
  geom_vline(xintercept = 0, color = "grey55", linewidth = 0.4) +
  geom_segment(aes(x = -20, xend = end1, y = 1, yend = 1),
               linewidth = 3, color = "#2a78d6", lineend = "round") +
  geom_segment(aes(x = 0, xend = tip2, y = 0, yend = 0),
               linetype = "dotted", linewidth = 0.7, color = "grey40") +
  geom_segment(aes(x = tip2, xend = 22, y = 0, yend = 0),
               linewidth = 3, color = "#eb6834", lineend = "round") +
  geom_point(aes(x = tip2 * 0, y = 0), shape = 21, size = 2.8,   # one dot per facet at 0
             fill = "white", color = "grey30", stroke = 0.9) +
  # direct labels, top panel only
  geom_text(data = ~ filter(.x, pair == last(levels(pair))),
            aes(x = -20, y = 1.55, label = "game 1"), hjust = 0, size = 3.6, color = "#2a78d6", fontface = "bold") +
  geom_text(data = ~ filter(.x, pair == last(levels(pair))),
            aes(x = 22, y = -0.45, label = "game 2"), hjust = 1, size = 3.6, color = "#eb6834", fontface = "bold") +
  geom_text(aes(x = 22, y = 1.75, label = label),
            hjust = 1, size = 4.2, fontface = "bold", color = "grey15") +
  scale_y_continuous(limits = c(-0.7, 2.1)) +
  scale_x_continuous(breaks = c(-20, -10, 0, 10, 20),
                     labels = c("-20 min", "-10", "game 2\nscheduled", "+10", "+20 min"),
                     limits = c(-21, 23)) +
  labs(title = "2 hours apart", x = NULL, y = NULL,
       caption = "data: wehoop, Her Hoop Stats | @wnbadata") +
  theme_gl +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 18),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0, size = 11))

# ---- 7d. CHARTS 7-8: fouls vs game length ---------------------------
# hhs_game_fouls_length.csv = one row per game from Her Hoop Stats, pulled
# 10/4 (game length, fouls and free throw attempts for both teams). Not
# included in this repo. Regular season, no overtime (OT adds both fouls and
# minutes).
# 1997-2005 were halves, not quarters, but the clock time is still 40 minutes.
# Note: play-by-play foul counts (wehoop) run 1-2 low vs the box score and
# differently by era, which is why this uses the Her Hoop Stats box scores.

fouls <- read_csv("~/Desktop/wehoop/2026-10-3 how long are the games/hhs_game_fouls_length.csv",
                  show_col_types = FALSE) %>%
  filter(season_type == "REG", team_mp <= 205) %>%
  arrange(season)                      # newest drawn last, on top

# printable checks
cor(fouls$fouls, fouls$minutes)                       # 0.55, every game
coef(lm(minutes ~ fouls + factor(season), fouls))[2]  # ~0.7 min per foul, within a season
fouls_by_season <- fouls %>%
  group_by(season) %>%
  summarize(games = n(), fouls = mean(fouls), minutes = mean(minutes),
            r = cor(fouls, minutes), .groups = "drop")
fouls_by_season
cor(fouls_by_season$fouls, fouls_by_season$minutes)   # ~0.05 across seasons!
fouls %>% filter(season == 2026) %>%
  mutate(third = ntile(fouls, 3)) %>%
  group_by(third) %>% summarize(fouls = mean(fouls), minutes = mean(minutes))

chart_fouls <- ggplot(fouls, aes(fouls, minutes, color = season)) +
  geom_hline(yintercept = 120, linetype = "dashed", color = "grey60") +
  geom_point(size = 1.1, alpha = 0.55, position = position_jitter(width = 0.25, height = 0.25, seed = 1)) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "grey20", linewidth = 0.8) +
  annotate("text", x = 64, y = 121.5, label = "2 hours", hjust = 1, size = 3.4, color = "grey45") +
  # a few delayed / suspended games run past 180 minutes; zoom without dropping them from the fit
  coord_cartesian(xlim = c(15, 65), ylim = c(92, 160)) +
  scale_color_gradient(low = "#c9dcf3", high = "#0b3d91", breaks = c(1997, 2005, 2015, 2026),
                       guide = guide_colorbar(barwidth = unit(14, "lines"), barheight = unit(0.6, "lines"))) +
  labs(title = "more fouls, longer games",
       subtitle = "every wnba regular season game since 1997 (no overtime)\nfouls = both teams combined",
       x = "fouls in the game", y = "minutes, tipoff to final buzzer",
       caption = "data: Her Hoop Stats | @wnbadata") +
  theme_gl +
  theme(legend.title = element_blank())

chart_fouls_seasons <- ggplot(fouls_by_season, aes(fouls, minutes, color = season)) +
  annotate("segment", x = 34.66, y = 120.10, xend = 39.0, yend = 124.2,
           arrow = arrow(length = unit(7, "pt"), type = "closed"), color = "grey55", linewidth = 0.5) +
  geom_point(size = 3.2) +
  geom_text(data = filter(fouls_by_season, season %in% c(1998, 2008, 2019, 2023, 2025, 2026)),
            aes(label = season), vjust = -1, size = 3.6, fontface = "bold", show.legend = FALSE) +
  scale_color_gradient(low = "#c9dcf3", high = "#0b3d91", guide = "none") +
  labs(title = "fouls only explain part of it",
       subtitle = "one dot per regular season, 1997-2026: average fouls vs average game length\ndarker = more recent. arrow = 2025 to 2026",
       x = "fouls per game", y = "minutes per game",
       caption = "data: Her Hoop Stats | @wnbadata") +
  theme_gl


# ---- 7e. CHART 9: fouls by season, per game and per 100 possessions ----
# Hard-coded from Her Hoop Stats box scores (fouls, both teams), pulled 10/5.
# Regular season, no overtime. per_100 = all fouls / all possessions (both
# teams) x 100, totals not averaged per game. 1997-2005 had a 30 second shot
# clock (fewer possessions); 24 seconds from 2006, hence the jump in pace.

fouls_season <- tribble(
  ~season, ~per_game, ~per_100,
  1997, 38.3, 25.1,   1998, 40.5, 26.6,   1999, 40.1, 27.7,   2000, 40.2, 28.1,
  2001, 36.9, 26.3,   2002, 37.8, 26.8,   2003, 37.5, 26.4,   2004, 37.8, 26.8,
  2005, 38.9, 27.5,   2006, 39.0, 25.1,   2007, 38.5, 24.5,   2008, 41.0, 26.1,
  2009, 39.1, 24.7,   2010, 38.0, 23.6,   2011, 36.2, 22.9,   2012, 35.4, 22.2,
  2013, 35.3, 22.4,   2014, 36.0, 23.0,   2015, 36.7, 23.5,   2016, 38.6, 24.1,
  2017, 37.3, 23.1,   2018, 36.2, 22.3,   2019, 35.0, 21.5,   2020, 35.6, 21.6,
  2021, 34.7, 21.4,   2022, 34.9, 21.3,   2023, 35.5, 21.6,   2024, 34.2, 20.9,
  2025, 34.7, 21.5,   2026, 39.2, 23.7
)

# printable checks
fouls_season %>% mutate(jump = per_game - lag(per_game)) %>% arrange(desc(jump)) %>% head(3)  # 2026 +4.5 biggest
fouls_season %>% filter(per_game >= 39.2)                                                       # last higher: 2008
fouls_season %>% filter(season > 2010, per_100 >= 23.7)                                         # last higher: 2016

fouls_season_long <- fouls_season %>%
  pivot_longer(c(per_game, per_100), names_to = "measure", values_to = "fouls") %>%
  mutate(measure = factor(measure, levels = c("per_game", "per_100"),
                          labels = c("fouls per game", "fouls per 100 possessions")))

chart_fouls_by_season <- ggplot(fouls_season_long, aes(season, fouls)) +
  geom_vline(xintercept = 2005.5, color = "grey80", linewidth = 0.4) +
  geom_text(data = tibble(measure = factor("fouls per 100 possessions",
                                           levels = levels(fouls_season_long$measure))),
            aes(x = 2005.8, y = Inf, label = "24 second shot clock"),
            hjust = 0, vjust = 1.6, size = 3, color = "grey50", inherit.aes = FALSE) +
  geom_line(color = "grey60", linewidth = 0.8) +
  geom_point(color = "grey60", size = 1.8) +
  geom_point(data = ~ filter(.x, season == 2026), color = "#2a78d6", size = 3.6) +
  geom_text(data = ~ filter(.x, season == 2026), aes(label = sprintf("%.1f", fouls)),
            color = "#2a78d6", fontface = "bold", hjust = -0.35, size = 4) +
  facet_wrap(~measure, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(1997, 2026, 4), limits = c(1997, 2028.5)) +
  labs(title = "refs are blowing the whistle more this year",
       subtitle = "wnba regular season, both teams combined, no overtime games",
       x = NULL, y = NULL,
       caption = "data: Her Hoop Stats | @wnbadata") +
  theme_gl +
  theme(strip.text = element_text(face = "bold", hjust = 0, size = 11),
        panel.grid.major.x = element_blank())


# ---- 7f. CHART 10: the worst handoff, Tue 9/29 -------------------------
# Game 2 of both first round series. Fever vs Aces (in Indiana) then Liberty
# vs Lynx (in New York), scheduled 2 hours apart. Same playoff_times data,
# pacific time. Zoomed to 5:15-6:00 pm so the 8:41 overlap is readable.

worst <- playoff_times %>% filter(day == "Tue 9/29") %>% arrange(sched)
g1 <- worst[1, ]   # IND vs LV: scheduled 3:30, ended 5:47:37
g2 <- worst[2, ]   # NY vs MIN: scheduled 5:30, tipped 5:38:56
x_lo <- 17.25; x_hi <- 18.0

clock_lab_min <- \(x) {   # truncates to the minute, like a clock (5:47:37 -> 5:47)
  tot <- floor(x * 60 + 1e-6); h <- (tot %/% 60) %% 24; m <- tot %% 60
  sprintf("%d:%02d", if_else(h %% 12 == 0, 12, h %% 12), m)
}

chart_worst <- ggplot() +
  annotate("rect", xmin = g2$tip, xmax = g1$end, ymin = -0.4, ymax = 1.4,
           fill = "#eb6834", alpha = 0.35) +
  annotate("text", x = (g2$tip + g1$end) / 2, y = 1.75,
           label = paste(fmt_ms(g1$end - g2$tip), "overlap"),
           size = 5.5, fontface = "bold", color = "grey15") +
  # game 1: fever vs aces, still going
  annotate("segment", x = x_lo, xend = g1$end, y = 1, yend = 1,
           linewidth = 5, color = "#2a78d6", lineend = "round") +
  annotate("text", x = x_lo, y = 1.3, label = "fever vs aces", hjust = 0,
           size = 4.5, fontface = "bold", color = "#2a78d6") +
  annotate("text", x = g1$end, y = 0.7, label = paste("final buzzer", clock_lab_min(g1$end)),
           hjust = 0.5, size = 3.6, color = "grey30") +
  # game 2: liberty vs lynx, scheduled then tipped
  annotate("segment", x = g2$sched, xend = g2$tip, y = 0, yend = 0,
           linetype = "dotted", linewidth = 0.8, color = "grey40") +
  annotate("segment", x = g2$tip, xend = x_hi, y = 0, yend = 0,
           linewidth = 5, color = "#eb6834", lineend = "round") +
  annotate("point", x = g2$sched, y = 0, shape = 21, size = 4,
           fill = "white", color = "grey30", stroke = 1) +
  annotate("text", x = g2$sched, y = -0.32, label = paste("scheduled", clock_lab_min(g2$sched)),
           hjust = 0.5, size = 3.6, color = "grey30") +
  annotate("text", x = g2$tip, y = -0.32, label = paste("tipoff", clock_lab_min(g2$tip)),
           hjust = 0.2, size = 3.6, color = "grey30") +
  annotate("text", x = x_hi, y = -0.6, label = "liberty vs lynx", hjust = 1,
           size = 4.5, fontface = "bold", color = "#eb6834") +
  scale_x_continuous(breaks = seq(17.25, 18, 0.25), labels = \(x) paste0(clock_lab_min(x), "pm"),
                     limits = c(x_lo - 0.01, x_hi + 0.01)) +
  scale_y_continuous(limits = c(-0.8, 2)) +
  labs(title = "tue 9/29, pacific time", x = NULL, y = NULL,
       caption = "data: wehoop, Her Hoop Stats | @wnbadata") +
  theme_gl +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 18),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey92"))


# ---- 7g. CHART 11: how late 2026 games tip off (wehoop, live) ----------
# Scheduled start = wehoop schedule `date` (the listed broadcast time).
# Actual tipoff = wallclock of the opening jump ball in the pbp. Games whose
# pbp doesn't start with the jump ball are dropped (a handful), and so is the
# All-Star game. Reuses pbp_26 from chart 4.

sched_26 <- wehoop::load_wnba_schedule(seasons = 2026) %>%
  transmute(game_id = as.integer(id),
            sched = as.POSIXct(date, format = "%Y-%m-%dT%H:%MZ", tz = "UTC"))

tips_26 <- pbp_26 %>%
  filter(!str_detect(home_team_name, "^Team "), !str_detect(away_team_name, "^Team "),
         game_date != as.Date("2026-06-30")) %>%     # Commissioner's Cup final, not a regular season game
  arrange(game_id, sequence_number) %>%
  group_by(game_id) %>%
  slice(1) %>%
  ungroup() %>%
  filter(type_text %in% c("Jumpball", "Jump Ball"), !is.na(wc)) %>%
  mutate(game_id = as.integer(game_id)) %>%
  inner_join(sched_26, by = "game_id") %>%
  transmute(type = if_else(season_type == 3, "playoffs", "regular season"),
            date = game_date, home = home_team_name, away = away_team_name,
            minutes_late = as.numeric(difftime(wc, sched, units = "mins")))

# printable checks: none early, regular season median ~3.9, playoffs ~8.1
tips_26 %>%
  group_by(type) %>%
  summarize(games = n(), early = sum(minutes_late < 0), median = median(minutes_late),
            avg = mean(minutes_late), max = max(minutes_late))

tip_rows <- c(`regular season` = 2, playoffs = 1)   # regular season on top
tips_plot <- tips_26 %>% mutate(row = tip_rows[type])
tip_medians <- tips_plot %>% group_by(type, row) %>% summarize(median = median(minutes_late), .groups = "drop")

chart_tipoffs <- ggplot(tips_plot, aes(minutes_late, row, color = type)) +
  geom_vline(xintercept = 0, color = "grey40", linewidth = 0.5) +
  annotate("text", x = 0.3, y = 2.5, label = "scheduled start", hjust = 0,
           size = 3.4, color = "grey35") +
  geom_point(position = position_jitter(width = 0, height = 0.2, seed = 2),
             size = 1.8, alpha = 0.6) +
  geom_segment(data = tip_medians, aes(x = median, xend = median, y = row - 0.3, yend = row + 0.3),
               color = "grey15", linewidth = 1) +
  geom_text(data = tip_medians, aes(x = median, y = row + 0.38, label = sprintf("usually %.0f min late", median)),
            hjust = 0, nudge_x = 0.3, size = 3.8, fontface = "bold", color = "grey15") +
  scale_color_manual(values = type_colors, guide = "none") +
  scale_y_continuous(breaks = tip_rows, labels = names(tip_rows), limits = c(0.55, 2.6)) +
  scale_x_continuous(breaks = seq(0, 25, 5), labels = \(x) if_else(x == 0, "0", paste0("+", x, " min")),
                     limits = c(-1, 25)) +
  labs(title = "2026 tipoffs vs scheduled start", x = NULL, y = NULL,
       caption = "data: wehoop | @wnbadata") +
  theme_gl +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 18),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 12, face = "bold", color = "grey20"))

# ---- 8. VIEW ---------------------------------------------------------
# Positron plot pane.

chart_history
chart_under2
chart_doubleheaders
chart_2026
chart_timeline
chart_handoff
chart_fouls
chart_fouls_seasons
chart_fouls_by_season
chart_worst
chart_tipoffs
