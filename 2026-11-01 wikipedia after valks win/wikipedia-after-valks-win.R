#how fast did wikipedia update after 2026 wnba finals game 7?
#wikipedia edits from the wikipedia api, game clock from espn play by play via wehoop, all times GMT

library(tidyverse)
library(lubridate)
library(httr2)
library(wehoop)

DATA_DIR <- "data"
dir.create(DATA_DIR, showWarnings = FALSE)
pbp_file <- file.path(DATA_DIR, "game7_pbp.csv")
revs_file <- file.path(DATA_DIR, "wikipedia_edits.csv")
detroit_file <- file.path(DATA_DIR, "detroitfan7_edits.csv")

before_color <- "#eb6834"
after_color <- "#7a45b0"
source_note <- "Data: Wikipedia edit history, ESPN via wehoop | @wnbadata"

theme_wiki <- theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
        plot.caption = element_text(hjust = 0, color = "grey40"),
        panel.grid.minor = element_blank())

wiki <- function(...) {
  request("https://en.wikipedia.org/w/api.php") |>
    req_url_query(..., format = "json", formatversion = 2) |>
    req_user_agent("wnbadata analysis (github.com/madbro206/wnbadata)") |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
}

revids <- c(valks_crowned = 1383118482, finals_4_3 = 1383118626, season_page = 1383119080,
            finals_3_3 = 1383119082, valks_reverted = 1383119312, wnba_page = 1383119447,
            finals_4_3_again = 1383120389, finals_2026 = 1383120397, valks_infobox = 1383120657,
            mvp_page = 1383123742, dream_fix = 1383139316)


#---- data ----
if (!file.exists(pbp_file)) {
  espn_wnba_pbp(game_id = 401918311) |>
    as_tibble() |>
    select(sequence_number, period_number, clock_display_value, wallclock, home_score, away_score, type_text, text) |>
    write_csv(pbp_file)
}

if (!file.exists(revs_file)) {
  wiki(action = "query", prop = "revisions", revids = paste(revids, collapse = "|"),
       rvprop = "ids|timestamp|user|comment|tags")$query$pages |>
    select(page = title, revisions) |>
    unnest(revisions) |>
    transmute(revid, page, editor = user, comment, timestamp,
              reverted = map_lgl(tags, \(t) "mw-reverted" %in% t)) |>
    write_csv(revs_file)
}

if (!file.exists(detroit_file)) {
  wiki(action = "query", list = "usercontribs", ucuser = "DetroitFan7",
       ucstart = "2026-11-01T02:21:46Z", ucend = "2026-11-01T02:32:00Z", ucdir = "newer",
       uclimit = 100, ucprop = "title|timestamp|comment")$query$usercontribs |>
    as_tibble() |>
    select(title, timestamp, comment) |>
    write_csv(detroit_file)
}

pbp <- read_csv(pbp_file, col_types = cols(wallclock = "c", clock_display_value = "c", .default = col_guess())) |>
  mutate(wall_clock = ymd_hms(wallclock),
         clock_sec = coalesce(as.numeric(str_extract(clock_display_value, "^\\d+(?=:)")), 0) * 60 +
           as.numeric(str_remove(clock_display_value, "^\\d+:")),
         gsv = home_score, atl = away_score) |>
  arrange(as.integer(sequence_number))

buzzer <- pbp |> filter(period_number == 4, type_text == "End Period") |> pull(wall_clock)
q4 <- pbp |> filter(period_number == 4, clock_sec <= 180)

revs <- read_csv(revs_file, col_types = cols(timestamp = "c", .default = col_guess())) |>
  mutate(key = names(revids)[match(revid, revids)],
         timestamp_gmt = ymd_hms(timestamp),
         sec_from_buzzer = as.numeric(difftime(timestamp_gmt, buzzer, units = "secs")))

detroit_edits <- read_csv(detroit_file, col_types = cols(.default = "c")) |>
  mutate(timestamp_gmt = ymd_hms(timestamp))

template_made <- detroit_edits |> filter(str_starts(title, "Template:")) |> pull(timestamp_gmt) |> min()
navbox <- detroit_edits |> filter(comment == "+navtemp") |> distinct(title, .keep_all = TRUE)
player_pages <- setdiff(navbox$title, c("Natalie Nakase", "Kasib Powell", "Sugar Rodgers", "2026 Golden State Valkyries season"))


#---- helpers ----
rev_sec <- function(k) revs$sec_from_buzzer[revs$key == k]
last_play <- function(t) q4 |> filter(wall_clock <= t) |> slice_tail(n = 1)

game_state <- function(t) {
  map_chr(t, \(x) {
    p <- last_play(x)
    sprintf("%d:%02d left, %d-%d", floor(p$clock_sec / 60), floor(p$clock_sec %% 60), p$gsv, p$atl)
  })
}

mmss <- function(sec) paste0(if_else(sec < 0, "-", "+"), abs(sec) %/% 60, ":", sprintf("%02d", abs(sec) %% 60))

say_duration <- function(sec) {
  m <- sec %/% 60
  case_when(sec < 60 ~ paste(sec, "sec"),
            sec < 600 & sec %% 60 != 0 ~ paste(m, "min", sec %% 60, "sec"),
            sec < 3600 ~ paste(m, "min"),
            .default = paste(m %/% 60, "hr", m %% 60, "min"))
}


#---- p_early ----
early <- revs |>
  filter(key %in% c("valks_crowned", "season_page", "valks_reverted")) |>
  arrange(timestamp_gmt) |>
  mutate(kind = if_else(key == "valks_reverted", "Reverted", "Valkyries crowned (by Wikipedia)"),
         what = c("Valkyries page: \"2026 WNBA Champions!\"",
                  "2026 Valkyries season page: defeated the Dream\n(never reverted)",
                  "Someone else reverts the Valkyries page:\n\"game has not ended\""),
         clock_sec = map_dbl(timestamp_gmt, \(x) last_play(x)$clock_sec),
         label = paste0(format(timestamp_gmt, "%H:%M:%S"), " GMT (", game_state(timestamp_gmt), ")\n", what),
         label_y = c(178, 140, 102))

p_early <- ggplot(q4, aes(x = wall_clock, y = clock_sec)) +
  geom_vline(xintercept = buzzer, color = "grey40", linewidth = 0.7) +
  geom_line(color = "grey70", linewidth = 1.1) +
  geom_segment(data = early, aes(x = timestamp_gmt, xend = timestamp_gmt, y = clock_sec, yend = label_y),
               color = "grey80", linewidth = 0.5) +
  geom_point(data = early, aes(x = timestamp_gmt, color = kind, shape = kind), size = 4, stroke = 1.4) +
  geom_label(data = early, aes(x = timestamp_gmt + 10, y = label_y, label = label),
             hjust = 0, size = 2.9, linewidth = 0, fill = "white", lineheight = 0.95) +
  annotate("text", x = buzzer, y = 182, label = paste0("Final buzzer\n", format(buzzer, "%H:%M:%S"), " GMT"),
           hjust = 1.08, vjust = 1, size = 3, color = "grey30") +
  scale_color_manual(values = c("Valkyries crowned (by Wikipedia)" = before_color, "Reverted" = "grey30"), name = NULL) +
  scale_shape_manual(values = c("Valkyries crowned (by Wikipedia)" = 16, "Reverted" = 4), name = NULL) +
  scale_x_datetime(date_breaks = "2 min", date_labels = "%H:%M", name = "Time (GMT), Nov 1 2026") +
  scale_y_continuous(breaks = seq(0, 180, 30), labels = \(s) sprintf("%d:%02d", s %/% 60, s %% 60),
                     limits = c(0, 185), name = "Game clock left in the 4th") +
  theme_wiki +
  theme(legend.position = "bottom") +
  labs(title = "Wikipedia edits before the final buzzer",
       subtitle = "The gray line is the game clock",
       caption = source_note)


#---- p_flip, p_flip_simple ----
flips <- revs |>
  filter(key %in% c("finals_4_3", "finals_3_3", "finals_4_3_again")) |>
  arrange(timestamp_gmt) |>
  mutate(series = c("4–3", "3–3", "4–3"))

flip_steps <- bind_rows(tibble(timestamp_gmt = ymd_hms("2026-11-01 02:06:00"), series = "3–3"),
                        flips |> select(timestamp_gmt, series),
                        tibble(timestamp_gmt = buzzer + minutes(2), series = "4–3")) |>
  mutate(series = factor(series, levels = c("3–3", "4–3")))

flip_labels <- flips |>
  mutate(series = factor(series, levels = c("3–3", "4–3")),
         label = if_else(timestamp_gmt > buzzer,
                         paste0(format(timestamp_gmt, "%H:%M:%S"), ": 4–3 again,\n", rev_sec("finals_4_3_again"), " seconds after the buzzer"),
                         paste0(format(timestamp_gmt, "%H:%M:%S"), ": ", series, "\n", game_state(timestamp_gmt))))

early_window <- tibble(start = flips$timestamp_gmt[1], end = flips$timestamp_gmt[2])

p_flip <- ggplot(flip_steps, aes(x = timestamp_gmt, y = as.integer(series))) +
  geom_rect(data = early_window, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), inherit.aes = FALSE,
            fill = before_color, alpha = 0.12) +
  geom_vline(xintercept = buzzer, color = "grey40", linewidth = 0.7) +
  geom_step(color = after_color, linewidth = 1.3) +
  geom_point(data = flip_labels, aes(y = as.integer(series)), color = after_color, size = 3) +
  geom_label(data = flip_labels, aes(y = as.integer(series), label = label),
             vjust = c(-0.25, 1.25, -0.25), hjust = c(0, 0, 1), nudge_x = c(10, 10, -10),
             size = 2.9, linewidth = 0, fill = "white", lineheight = 0.95) +
  annotate("text", x = buzzer, y = 0.55, label = "Final buzzer", hjust = 1.08, size = 3, color = "grey30") +
  scale_x_datetime(date_breaks = "2 min", date_labels = "%H:%M", name = "Time (GMT), Nov 1 2026") +
  scale_y_continuous(breaks = 1:2, labels = c("3–3\n(series tied)", "4–3\n(Valkyries win)"), limits = c(0.5, 2.5), name = NULL) +
  theme_wiki +
  labs(title = "WNBA Finals page: the 2026 series result",
       subtitle = "All 3 edits by the same account",
       caption = paste0("Shaded: the page said 4–3 while the game was still going. ", source_note))

flip_tiles <- flips |>
  mutate(step = row_number(),
         when = if_else(timestamp_gmt > buzzer, paste0(rev_sec("finals_4_3_again"), " seconds after\nthe buzzer"),
                        str_replace(game_state(timestamp_gmt), " left, ", " left\n")),
         fill = case_when(step == 2 ~ "grey92", timestamp_gmt > buzzer ~ "#e6dcf0", .default = "#fbe1d6"))

p_flip_simple <- ggplot(flip_tiles, aes(x = step, y = 1)) +
  geom_tile(aes(fill = fill), width = 0.92, height = 0.9) +
  geom_text(aes(label = series), vjust = -0.2, size = 16, color = "grey15", fontface = "bold") +
  geom_text(aes(label = when), vjust = 1.4, size = 4, color = "grey30", lineheight = 0.95) +
  geom_text(aes(y = 1.48, label = format(timestamp_gmt, "%H:%M:%S GMT")), size = 3.4, color = "grey40") +
  scale_fill_identity() +
  scale_y_continuous(limits = c(0.5, 1.55)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
        plot.caption = element_text(hjust = 0, color = "grey40"),
        plot.margin = margin(10, 10, 10, 10)) +
  labs(title = "WNBA Finals page: the 2026 series result",
       subtitle = "3 edits by the same account",
       caption = source_note)


#---- p_detroit ----
detroit <- bind_rows(tibble(title = "(the champions box itself)", timestamp_gmt = template_made),
                     navbox |> select(title, timestamp_gmt)) |>
  mutate(sec = as.numeric(difftime(timestamp_gmt, buzzer, units = "secs")),
         label = mmss(sec),
         title = fct_rev(fct_inorder(title)))

p_detroit <- ggplot(detroit, aes(x = sec / 60, y = title)) +
  geom_segment(aes(x = 0, xend = sec / 60, yend = title), color = "grey85", linewidth = 0.8) +
  geom_point(color = after_color, size = 2.8) +
  geom_text(aes(label = label), hjust = -0.3, size = 3, color = "grey30") +
  scale_x_continuous(breaks = 0:10, labels = \(m) paste0("+", m, " min"), limits = c(0, 10.5),
                     expand = expansion(mult = c(0, 0.02)), name = "Time after the final buzzer") +
  scale_y_discrete(name = NULL) +
  theme_wiki +
  theme(panel.grid.major.y = element_blank()) +
  labs(title = "DetroitFan7's champions box edits",
       subtitle = "Every page, in order",
       caption = source_note)


#---- p_howlong ----
howlong <- tibble(
  what = c("Finals pages", "Valkyries page", "Every Valkyries player", "Finals MVP page", "Dream page fixed (by me)"),
  sec = c(rev_sec("finals_2026"),
          rev_sec("valks_infobox"),
          max(as.numeric(difftime(navbox$timestamp_gmt[navbox$title %in% player_pages], buzzer, units = "secs"))),
          rev_sec("mvp_page"),
          rev_sec("dream_fix"))) |>
  mutate(what = fct_rev(fct_inorder(what)), label = say_duration(sec))

p_howlong <- ggplot(howlong, aes(x = sec / 60, y = what)) +
  geom_col(fill = "#B896D4", width = 0.6) +
  geom_text(aes(label = label), hjust = -0.15, size = 3.8, color = "grey20") +
  scale_x_continuous(breaks = seq(0, 150, 30), expand = expansion(mult = c(0, 0.18)), name = "Minutes after the final buzzer") +
  scale_y_discrete(name = NULL) +
  theme_wiki +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 11)) +
  labs(title = "Time until each page updated",
       subtitle = paste0("From the final buzzer, ", format(buzzer, "%H:%M:%S"), " GMT"),
       caption = source_note)


#---- tibbles ----
early_pages <- revs |>
  filter(key %in% c("valks_crowned", "finals_4_3", "season_page", "wnba_page")) |>
  arrange(timestamp_gmt) |>
  transmute(page,
            gmt = format(timestamp_gmt, "%H:%M:%S"),
            before_buzzer = sub("^-", "", mmss(sec_from_buzzer)),
            min_before = round(-sec_from_buzzer / 60, 1),
            game = game_state(timestamp_gmt),
            reverted)

end_of_game <- pbp |>
  filter(period_number == 4, !str_detect(text, "enters the game")) |>
  slice_tail(n = 10) |>
  transmute(gmt = format(wall_clock, "%H:%M:%S"), clock = clock_display_value, gsv, atl, play = text)


p_early
p_flip
p_flip_simple
p_detroit
p_howlong
early_pages
end_of_game
