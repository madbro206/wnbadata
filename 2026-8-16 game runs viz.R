#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html
# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, ggplot2)

#load data
#wnba full play by play
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp(season=c(2026))
})
tictoc::toc()
##############################################################################################################
# NOTE: load_wnba_pbp() reads pre-built files from the sportsdataverse repo and
# is currently stale (last game 2026-08-01), so recent games aren't in wnba_pbp.
# espn_wnba_pbp() hits ESPN live. It returns a data.table, hence as_tibble().
pbp <- wehoop::espn_wnba_pbp(game_id = 401857141) |> tibble::as_tibble()  # LAS @ NYL, 2026-08-13

##############################################################################################################
# NET SCORE GAME FLOW ---------------------------------------------------------
# One line, home - away, like ESPN's game flow but as a single margin.

# ---- 1. clock -> elapsed minutes -------------------------------------------
# ESPN clock counts DOWN inside a period, and switches format under a minute:
# "9:41" (mm:ss) above a minute, "7.2" (seconds) below it.
clock_to_seconds <- function(x) {
  mmss <- grepl(":", x, fixed = TRUE)
  out <- numeric(length(x))
  out[mmss]  <- as.numeric(sub(":.*", "", x[mmss])) * 60 +
                as.numeric(sub(".*:", "", x[mmss]))
  out[!mmss] <- as.numeric(x[!mmss])
  out
}

# WNBA: 10-minute quarters, 5-minute overtimes.
elapsed_minutes <- function(period, clock_chr) {
  len   <- ifelse(period <= 4, 10, 5)
  start <- ifelse(period <= 4, (period - 1) * 10, 40 + (period - 5) * 5)
  start + (len - clock_to_seconds(clock_chr) / 60)
}

# Quarter/OT boundaries actually played, for the shared x axis.
period_axis <- function(pbp) {
  last_p <- max(pbp$period_number)
  bounds <- elapsed_minutes(seq_len(last_p), rep("0:00", last_p))
  list(
    bounds = bounds,
    breaks = c(0, head(bounds, -1)) + diff(c(0, bounds)) / 2,
    labels = ifelse(seq_len(last_p) <= 4, paste0("Q", seq_len(last_p)),
                    paste0("OT", seq_len(last_p) - 4)),
    end    = max(bounds)
  )
}

# ---- 2. pbp -> net score ----------------------------------------------------
# One row per scoring event: the margin as it stood after each made basket.
net_score_events <- function(pbp) {
  pbp |>
    as_tibble() |>
    mutate(
      seq  = as.numeric(sequence_number),
      mins = elapsed_minutes(period_number, clock_display_value),
      net  = home_score - away_score
    ) |>
    filter(!is.na(mins), !is.na(net)) |>
    arrange(period_number, mins, seq) |>
    # Several plays share one clock reading (shot, rebound, foul all at 7:32).
    # Keep the last one, so each instant has a single margin instead of the
    # zigzag you get from ties in event order.
    group_by(mins) |>
    slice_tail(n = 1) |>
    ungroup() |>
    select(mins, net)
}

# Hold each margin until the next event, sampled once per second. A dense grid
# (rather than duplicated points) keeps x strictly increasing, which is what
# geom_ribbon needs to fill correctly.
net_score_frame <- function(pbp, end) {
  e <- net_score_events(pbp)
  grid <- tibble(mins = seq(0, end, by = 1 / 60))
  idx <- findInterval(grid$mins, e$mins)          # 0 before the first event
  grid |>
    mutate(
      net       = ifelse(idx == 0, 0, e$net[pmax(idx, 1)]),
      home_lead = pmax(net, 0),
      away_lead = pmin(net, 0)
    )
}

# ---- 3. colors -------------------------------------------------------------
INK     <- "#0b0b0b"
INK_2   <- "#52514e"
SURFACE <- "#fcfcfb"

# ESPN ships each team's hex in the pbp itself, so no color package needed:
# home_team_color / away_team_color, plus an alternate for each.
hex_of <- function(x) paste0("#", x)

col_distance <- function(a, b) {
  as.numeric(farver::compare_colour(
    farver::decode_colour(a), farver::decode_colour(b),
    from_space = "rgb", method = "cie2000"))
}

# Some matchups are two shades of the same color — Fever navy and Wings navy
# are 1.2 apart, indistinguishable. Fall back to an alternate when that happens.
team_palette <- function(g) {
  home <- hex_of(g$home_team_color)
  away <- hex_of(g$away_team_color)
  if (col_distance(home, away) < 20) away <- hex_of(g$away_team_alternate_color)
  if (col_distance(home, away) < 20) home <- hex_of(g$home_team_alternate_color)
  c(home = home, away = away)
}

# Liberty mint is 1.77:1 against this background: fine as a filled block,
# unreadable as type. Darken a copy for anything set in text.
readable <- function(col, bg = SURFACE, target = 3) {
  lum <- function(h) {
    v <- farver::decode_colour(h) / 255
    v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055) ^ 2.4)
    as.numeric(v %*% c(0.2126, 0.7152, 0.0722))
  }
  ratio <- function(a, b) (max(lum(a), lum(b)) + 0.05) / (min(lum(a), lum(b)) + 0.05)
  lab <- farver::convert_colour(farver::decode_colour(col), "rgb", "lab")
  while (ratio(col, bg) < target && lab[1, "l"] > 5) {
    lab[1, "l"] <- lab[1, "l"] - 3
    rgb <- pmin(pmax(farver::convert_colour(lab, "lab", "rgb"), 0), 255)
    col <- farver::encode_colour(rgb)
  }
  unname(col)
}

# ---- 4. the charts ---------------------------------------------------------
# Diverging encoding: one line, two poles around a neutral zero.

plot_net_score <- function(pbp) {
  g   <- pbp[1, ]
  pal <- team_palette(g)
  ax  <- period_axis(pbp)
  f   <- net_score_frame(pbp, ax$end)

  # biggest lead of the game, for the one annotation
  peak <- net_score_events(pbp) |> slice_max(abs(net), n = 1, with_ties = FALSE)
  peak_team <- if (peak$net > 0) g$home_team_abbrev else g$away_team_abbrev

  ggplot(f, aes(mins)) +
    geom_vline(xintercept = head(ax$bounds, -1), colour = "#e6e5e1", linewidth = 0.4) +
    geom_ribbon(aes(ymin = 0, ymax = home_lead), fill = pal["home"], alpha = 0.3) +
    geom_ribbon(aes(ymin = away_lead, ymax = 0), fill = pal["away"], alpha = 0.3) +
    geom_hline(yintercept = 0, colour = "#8a8880", linewidth = 0.5) +
    geom_line(aes(y = net), colour = INK, linewidth = 0.7) +
    # direct labels instead of a legend: one series, two poles
    annotate("text", x = 0, y = max(f$net) , label = paste(g$home_team_abbrev, "leads"),
             hjust = 0, vjust = -0.4, colour = readable(pal["home"]),
             fontface = "bold", size = 3.6) +
    annotate("text", x = 0, y = min(f$net), label = paste(g$away_team_abbrev, "leads"),
             hjust = 0, vjust = 1.4, colour = readable(pal["away"]),
             fontface = "bold", size = 3.6) +
    annotate("point", x = peak$mins, y = peak$net, colour = INK, size = 2) +
    annotate("text", x = peak$mins, y = peak$net, size = 3.2, colour = INK_2,
             hjust = -0.15, vjust = if (peak$net > 0) -0.6 else 1.6,
             label = paste0(peak_team, " by ", abs(peak$net))) +
    scale_x_continuous(breaks = ax$breaks, labels = ax$labels,
                       limits = c(0, ax$end), expand = expansion(mult = c(0.01, 0.03))) +
    scale_y_continuous(labels = abs, expand = expansion(mult = 0.12)) +
    labs(
      title = paste0(g$home_team_full_name, " ", g$home_team_score, ", ",
                     g$away_team_full_name, " ", g$away_team_score),
      subtitle = paste0(format(as.Date(g$game_date), "%B %d, %Y"),
                        "  ·  score margin through the game"),
      x = NULL, y = "Margin"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.background   = element_rect(fill = SURFACE, colour = NA),
      panel.background  = element_rect(fill = SURFACE, colour = NA),
      panel.grid.minor  = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#eceae6", linewidth = 0.4),
      plot.title    = element_text(face = "bold", colour = INK),
      plot.subtitle = element_text(colour = INK_2, size = 10),
      axis.title.y  = element_text(colour = INK_2, size = 9),
      axis.text     = element_text(colour = INK_2),
      axis.text.x   = element_text(face = "bold", size = 10)
    )
}

##############################################################################################################
# WHO SCORED LAST -------------------------------------------------------------
# A timeline strip: the game clock as a number line, colored by whichever team
# scored most recently. A long block of one color IS a run.

# Every made basket, from the score changing rather than from play type — that
# catches free throws and and-1s without parsing text.
scoring_events <- function(pbp) {
  pbp |>
    as_tibble() |>
    mutate(
      seq  = as.numeric(sequence_number),
      mins = elapsed_minutes(period_number, clock_display_value)
    ) |>
    filter(!is.na(mins)) |>
    arrange(period_number, mins, seq) |>
    mutate(
      d_home = home_score - lag(home_score, default = 0),
      d_away = away_score - lag(away_score, default = 0)
    ) |>
    filter(d_home > 0 | d_away > 0) |>
    transmute(mins, scorer = if_else(d_home > 0, "home", "away"),
              pts = pmax(d_home, d_away))
}

# Consecutive scores by one team = one run. rle() never looks at
# period_number, so a streak carrying across a quarter break stays one run.
# A run holds the floor from its first basket until the other team answers.
runs_frame <- function(pbp, end) {
  e <- scoring_events(pbp)
  r <- rle(e$scorer)
  e |>
    mutate(grp = rep(seq_along(r$lengths), r$lengths)) |>
    group_by(grp, scorer) |>
    summarise(pts = sum(pts), xmin = min(mins), .groups = "drop") |>
    arrange(xmin) |>
    mutate(xmax = lead(xmin, default = end))
}

biggest_run <- function(pbp, end) {
  runs_frame(pbp, end) |> slice_max(pts, n = 1, with_ties = FALSE)
}

plot_scoring_runs <- function(pbp) {
  g   <- pbp[1, ]
  ax  <- period_axis(pbp)
  f   <- runs_frame(pbp, ax$end)
  run <- f |> slice_max(pts, n = 1, with_ties = FALSE)

  team_cols <- team_palette(g)
  # labels line up with breaks below, so they stay unnamed and in that order
  team_labs <- c(g$home_team_full_name, g$away_team_full_name)
  top <- max(f$pts)

  ggplot(f) +
    # the number line itself, so the empty stretch before the first basket
    # still reads as part of the game
    annotate("segment", x = 0, xend = ax$end, y = 0, yend = 0,
             colour = "#c9c7c1", linewidth = 0.4) +
    # thin surface-colored edge so neighboring runs read as separate bars
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = pts, fill = scorer),
              colour = SURFACE, linewidth = 0.3) +
    # Quarter marks sit under the bars rather than notching them: a streak that
    # carries from one quarter into the next is one run, so the color shouldn't
    # break at the boundary.
    annotate("segment", x = head(ax$bounds, -1), xend = head(ax$bounds, -1),
             y = -top * 0.07, yend = -top * 0.02, colour = "#b8b6b0", linewidth = 0.5) +
    # the one run worth naming
    annotate("text", x = (run$xmin + run$xmax) / 2, y = run$pts, vjust = -0.9,
             size = 3.4, fontface = "bold", colour = INK_2,
             label = paste0(if (run$scorer == "home") g$home_team_abbrev
                            else g$away_team_abbrev, " ", run$pts, "-0")) +
    scale_fill_manual(values = team_cols, labels = team_labs,
                      breaks = c("home", "away"), name = NULL) +
    scale_x_continuous(breaks = ax$breaks, labels = ax$labels,
                       limits = c(0, ax$end), expand = expansion(mult = 0.005)) +
    # headroom at the top so the label on the tallest bar isn't clipped when
    # the plot pane is short. breaks stop at the tallest bar so the extra
    # space doesn't grow a gridline above the data.
    scale_y_continuous(breaks = seq(0, top, by = 5),
                       expand = expansion(mult = c(0, 0.28))) +
    labs(
      title = paste0("Scoring runs: ", g$away_team_full_name, " at ",
                     g$home_team_full_name),
      subtitle = paste0(format(as.Date(g$game_date), "%B %d, %Y"),
                        "  ·  bar height = points scored before the other team answered"),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.background  = element_rect(fill = SURFACE, colour = NA),
      panel.background = element_rect(fill = SURFACE, colour = NA),
      panel.grid         = element_blank(),
      panel.grid.major.y = element_line(colour = "#eceae6", linewidth = 0.4),
      aspect.ratio     = 0.13,   # keep it flat whatever shape the plot pane is
      axis.text.y      = element_text(colour = INK_2, size = 9),
      axis.text.x      = element_text(face = "bold", size = 10, colour = INK_2),
      legend.position  = "top",
      legend.justification = "left",
      legend.text      = element_text(colour = INK_2, size = 10),
      legend.key.size  = unit(10, "pt"),
      plot.title       = element_text(face = "bold", colour = INK),
      plot.subtitle    = element_text(colour = INK_2, size = 10)
    )
}

##############################################################################################################
# THE SAME THING AS A CLOCK ---------------------------------------------------
# Runs wrapped around a circle: one full turn = one full game, tip-off at 12
# o'clock running clockwise. Angle is time, radius is points in the run.

plot_runs_clock <- function(pbp) {
  g   <- pbp[1, ]
  ax  <- period_axis(pbp)
  f   <- runs_frame(pbp, ax$end)
  run <- f |> slice_max(pts, n = 1, with_ties = FALSE)

  team_cols <- team_palette(g)
  team_labs <- c(g$home_team_full_name, g$away_team_full_name)
  top   <- max(f$pts)
  inner <- top * 0.75   # size of the hole; bars grow outward from this ring
  rings <- seq(5, top, by = 5)   # radial scale, same 5-point steps as the bars

  ggplot(f) +
    # under polar coords a horizontal line is a circle, so these are the
    # gridlines. Drawn before the bars so they sit underneath.
    geom_hline(yintercept = rings, colour = "#dedcd7", linewidth = 0.3) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = pts, fill = scorer),
              colour = SURFACE, linewidth = 0.3) +
    # quarter marks as spokes cut into the hole
    annotate("segment", x = ax$bounds, xend = ax$bounds,
             y = -inner * 0.22, yend = 0, colour = "#b8b6b0", linewidth = 0.5) +
    # final score in the middle
    annotate("text", x = 0, y = -inner, vjust = 0.1, size = 5, fontface = "bold",
             colour = INK, label = paste0(g$away_team_abbrev, " ", g$away_team_score,
                                          "  ", g$home_team_abbrev, " ", g$home_team_score)) +
    # quarter labels placed just outside the ring — the polar x axis parks them
    # out at the panel corners, too far from the circle to read as labels
    annotate("text", x = ax$breaks, y = top * 1.12, label = ax$labels,
             size = 3.8, fontface = "bold", colour = INK_2) +
    # scale labels up the 12 o'clock line, on chips so they stay readable
    # wherever a bar happens to cross them
    annotate("label", x = 0, y = rings, size = 2.9, colour = INK_2,
             fill = SURFACE, linewidth = 0, label.padding = unit(1.5, "pt"),
             label = c(head(rings, -1), paste(tail(rings, 1), "pts"))) +
    scale_fill_manual(values = team_cols, labels = team_labs,
                      breaks = c("home", "away"), name = NULL) +
    scale_x_continuous(limits = c(0, ax$end), expand = expansion(0)) +
    scale_y_continuous(limits = c(-inner, top * 1.2)) +
    coord_polar(theta = "x") +   # tip-off at 12 o'clock, running clockwise
    labs(
      title = paste0("Scoring runs: ", g$away_team_full_name, " at ",
                     g$home_team_full_name),
      subtitle = paste0(format(as.Date(g$game_date), "%B %d, %Y"),
                        "  ·  one turn = one game, bar length = points in the run"),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.background  = element_rect(fill = SURFACE, colour = NA),
      panel.background = element_rect(fill = SURFACE, colour = NA),
      panel.grid       = element_blank(),
      axis.text        = element_blank(),
      legend.position  = "top",
      legend.justification = "left",
      legend.text      = element_text(colour = INK_2, size = 10),
      legend.key.size  = unit(10, "pt"),
      plot.title       = element_text(face = "bold", colour = INK),
      plot.subtitle    = element_text(colour = INK_2, size = 10)
    )
}



plot_net_score(pbp)
plot_scoring_runs(pbp)
plot_runs_clock(pbp)
