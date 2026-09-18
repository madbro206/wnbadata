# charts for the las vegas aces "weird 4/15" video
#
# everything computes live from wehoop (espn) and was cross checked against her hoop
# stats on 9/6. margin sd 18.2, phoenix 15.0 second, 17 blowouts, 8 close games at 5-3,
# average absolute margin 14.62, all five named games match to the point.
#
# two mandatory wehoop filters or the season is wrong:
#   - the commissioner's cup final sits inside season_type == 2 but does not count
#   - the all star rosters appear as their own "teams"
#
# axis rule: anything with a length starts at zero. the game log chart uses margin,
# which is naturally centred on zero, so its baseline is the real zero line.

library(wehoop)
library(dplyr)
library(tidyr)
library(ggplot2)

ALLSTAR   <- c("Team Spoon", "Team Coop")
CUP_FINAL <- as.Date("2026-06-30")
LV        <- "Las Vegas"

HI     <- "#C8102E"   # aces red. their other option is black, try "#101010"
LOSS   <- "#B4B2A9"   # losses on the game log
GRAY   <- "#8A8A86"   # everyone else
INK    <- "#15171D"
CREDIT <- "2026 season through the fiba break  ·  data: her hoop stats and wehoop  |  chart: @wnbadata"

team_box <- load_wnba_team_box(seasons = 2026) |>
  filter(season_type == 2, !team_location %in% ALLSTAR, game_date != CUP_FINAL) |>
  mutate(margin = team_score - opponent_team_score)

base_theme <- function() {
  theme_minimal(base_size = 15) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_text(face = "bold", colour = INK, size = 19),
      plot.subtitle = element_text(colour = GRAY, size = 13, margin = margin(b = 14)),
      plot.caption  = element_text(colour = GRAY, size = 11, hjust = 0, margin = margin(t = 14)),
      axis.text     = element_text(colour = INK, size = 13),
      axis.title    = element_text(colour = GRAY, size = 12),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#E3E3E0", linewidth = 0.3),
      plot.margin   = margin(18, 18, 14, 10)
    )
}

lollipop <- function(df, value, label_fmt, title, subtitle, ascending = FALSE) {
  d <- df |> mutate(v = {{ value }}, is_lv = team == LV, lab = sprintf(label_fmt, v))
  # constant gap in data units so single and double digit labels clear the dot equally
  PAD <- 0.035 * max(d$v, na.rm = TRUE)
  ggplot(d, aes(x = v, y = reorder(team, if (ascending) -v else v))) +
    geom_segment(aes(x = 0, xend = v, yend = team, colour = is_lv),
                 linewidth = 1.6, alpha = 0.55, show.legend = FALSE) +
    geom_point(aes(colour = is_lv, size = is_lv)) +
    geom_text(aes(label = lab, colour = is_lv), hjust = 0, nudge_x = PAD, size = 4.2,
              show.legend = FALSE) +
    scale_colour_manual(values = c(`FALSE` = GRAY, `TRUE` = HI), guide = "none") +
    scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 5.4), guide = "none") +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.14))) +
    labs(title = title, subtitle = subtitle, caption = CREDIT, x = NULL, y = NULL) +
    base_theme()
}

# 1. THE GAME LOG. every aces result in order. this is the chart that shows the whole
# premise without any explanation, and margin has a genuine zero so the bars are honest.
lv_games <- team_box |>
  filter(team_location == LV) |>
  arrange(game_date) |>
  mutate(g = row_number(), won = margin > 0)

hilite <- lv_games |> filter(game_date %in% as.Date(c("2026-07-11", "2026-07-12")))

p_gamelog <- ggplot(lv_games, aes(x = g, y = margin, fill = won)) +
  geom_col(width = 0.72) +
  geom_hline(yintercept = 0, colour = INK, linewidth = 0.4) +
  geom_hline(yintercept = c(-15, 15), colour = "#C9C8C3", linewidth = 0.4, linetype = "22") +
  annotate("segment", x = hilite$g[1] - 0.6, xend = hilite$g[2] + 0.6,
           y = 56, yend = 56, colour = INK, linewidth = 0.4) +
  annotate("text", x = mean(hilite$g), y = 60,
           label = "back to back nights", colour = INK, size = 4, fontface = "bold") +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = LOSS), guide = "none") +
  scale_y_continuous(breaks = seq(-40, 50, 15)) +
  labs(title = "las vegas final margin by game, 2026",
       subtitle = "final margin per game, dashed lines mark a 15 point blowout either way",
       caption = CREDIT, x = "game", y = "margin") +
  base_theme() +
  theme(panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3),
        panel.grid.major.x = element_blank())

# 1b. BASKETBALL REFERENCE STYLE GAME RESULTS. same data as p_gamelog but styled like
# the bbref strip: green wins above the line, red losses below, grouped by month with
# separators, no y axis. only games actually played, no placeholder bars for the rest.
#
# green/red is normally a colourblind problem, but here it is REDUNDANT encoding: a bar
# above the line is a win and below is a loss by definition, so direction carries the
# meaning and colour only reinforces it.

WIN_G  <- "#2E8B45"
LOSS_R <- "#D0342C"

lv_months <- lv_games |>
  mutate(month = format(game_date, "%B"),
         month = factor(month, levels = unique(month[order(game_date)])))

month_bounds <- lv_months |>
  group_by(month) |>
  summarise(lo = min(g), hi = max(g), mid = mean(range(g)), .groups = "drop")

p_results <- ggplot(lv_months, aes(x = g, y = margin, fill = margin > 0)) +
  geom_col(width = 0.68) +
  geom_hline(yintercept = 0, colour = INK, linewidth = 0.5, linetype = "22") +
  geom_segment(data = month_bounds, inherit.aes = FALSE,
               aes(x = lo - 0.4, xend = hi + 0.4, y = -58, yend = -58),
               colour = "#C9C8C3", linewidth = 0.6) +
  geom_text(data = month_bounds, inherit.aes = FALSE,
            aes(x = mid, y = -64, label = tolower(month)),
            colour = GRAY, size = 4) +
  scale_fill_manual(values = c(`TRUE` = WIN_G, `FALSE` = LOSS_R), guide = "none") +
  # breaks stop at +/- 40 on purpose. the axis runs to -68 to make room for the month
  # rule and labels underneath, and a break down there would put a gridline through them.
  scale_y_continuous(limits = c(-68, 55), breaks = seq(-40, 40, 20),
                     labels = function(x) if_else(x == 0, "0", sprintf("%+d", as.integer(x)))) +
  labs(title = "las vegas final margin by game, colored by result",
       subtitle = "wins above the line and losses below",
       caption = CREDIT, x = NULL, y = "final margin") +
  base_theme() +
  theme(axis.text.x  = element_blank(),
        axis.text.y  = element_text(colour = GRAY, size = 12),
        axis.title.y = element_text(colour = GRAY, size = 12, margin = margin(r = 8)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#EDECE9", linewidth = 0.3),
        panel.grid.minor   = element_blank())

p_results

# 2. THE MEASURED VERSION. margin standard deviation, the lead stat
sd_margin <- team_box |>
  group_by(team = team_location) |>
  summarise(sd_margin = sd(margin), .groups = "drop")

p_sd <- lollipop(sd_margin, sd_margin, "%.1f",
                 "las vegas swings more game to game than any team in the wnba",
                 "standard deviation of final margin, ie how much the result moves night to night")

# 3. how often the game is a blowout at all, either direction
# FACT CHECK for the "no other team with a winning record has more than four" line.
# re-verified 9/5 against hhs, which returns the identical eight rows. hhs also lists
# franchise aliases (utah and san antonio for las vegas, detroit and tulsa for dallas),
# so the raw hhs version returns 12 rows for these 8 teams. that is a naming artifact,
# not extra teams. see the memory note on the hhs table map.
blowout_check <- team_box |>
  group_by(team = team_location) |>
  summarise(w = sum(margin > 0), l = sum(margin < 0),
            losses_by_15 = sum(margin <= -15), .groups = "drop") |>
  transmute(team, record = sprintf("%d-%d", w, l), losses_by_15) |>
  arrange(desc(losses_by_15), desc(record))

blowout_check

blowouts <- team_box |>
  group_by(team = team_location) |>
  summarise(won_by_15 = sum(margin >= 15), lost_by_15 = sum(margin <= -15), .groups = "drop") |>
  tidyr::pivot_longer(-team, names_to = "kind", values_to = "n") |>
  mutate(kind = factor(if_else(kind == "won_by_15", "won by 15+", "lost by 15+"),
                       levels = c("lost by 15+", "won by 15+")))

p_blowouts <- ggplot(blowouts, aes(x = n, y = reorder(team, n), fill = kind)) +
  geom_col(width = 0.66) +
  scale_fill_manual(values = c(`won by 15+` = HI, `lost by 15+` = LOSS), name = NULL) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
  labs(title = "games decided by 15 or more, by team",
       subtitle = NULL,
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top")

# 4. the flip side. how rarely their games are close
close_games <- team_box |>
  group_by(team = team_location) |>
  summarise(close = sum(abs(margin) <= 5), .groups = "drop")

p_close <- lollipop(close_games, close, "%.0f",
                    "las vegas plays almost no close games",
                    "games decided by 5 points or fewer", ascending = TRUE)

# 5. the two consecutive night swings, side by side
swings <- lv_games |>
  filter(game_date %in% as.Date(c("2026-05-09", "2026-05-10", "2026-07-11", "2026-07-12"))) |>
  mutate(pair = if_else(game_date < as.Date("2026-06-01"),
                        "may 9 and 10, a 60 point swing",
                        "july 11 and 12, an 82 point swing"),
         lab  = paste0(format(game_date, "%b %e"), " vs ", opponent_team_location))

p_swings <- ggplot(swings, aes(x = reorder(lab, game_date), y = margin, fill = margin > 0)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, colour = INK, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+d", margin),
                vjust = if_else(margin > 0, -0.5, 1.4)), colour = INK, size = 4.6) +
  facet_wrap(~pair, scales = "free_x") +
  scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = LOSS), guide = "none") +
  scale_y_continuous(limits = c(-45, 60)) +
  labs(title = "las vegas final margin, four consecutive nights",
       subtitle = NULL,
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(colour = INK, size = 12))

# render one at a time in positron. for 9:16 size the plot pane tall and narrow.
p_gamelog
p_sd
p_blowouts
p_close
p_swings

# ---------------------------------------------------------------------------
# SEASON BY SEASON. the championship comparison.
#
# 2020-2023 are hardcoded from her hoop stats because wehoop's wnba team box only
# reaches back to 2024. the 2024-2026 rows were independently confirmed in wehoop
# and match exactly (once the commissioner's cup final is filtered out, which is
# a las vegas loss and otherwise shows them as 27-14 in 41 games).
#
# titles confirmed from the hhs playoff label table: 2022, 2023, 2025.
#
# season lengths differ (22, 32, 36, 40, 40, 44, 40) so blowout losses are shown
# PER 40 GAMES, not as raw counts. margin and sd are rates already and compare fine.
# for the 2024 vs 2026 pair in the video both seasons are 40 games, so there the
# rate and the raw count are the same number.

aces_seasons <- tibble::tribble(
  ~season, ~games, ~w, ~l, ~margin, ~sd_margin, ~blowout_l, ~title,
  2020,     22,    18,  4,   8.59,   9.26,      0,          FALSE,
  2021,     32,    24,  8,   9.06,  13.85,      0,          FALSE,
  2022,     36,    26, 10,   6.36,  13.51,      2,          TRUE,
  2023,     40,    34,  6,  12.55,  14.76,      3,          TRUE,
  2024,     40,    27, 13,   5.48,  10.87,      0,          FALSE,
  2025,     44,    30, 14,   2.91,  17.39,      5,          TRUE,
  2026,     40,    27, 13,   4.08,  18.20,      6,          FALSE
) |>
  # average winning margin and average losing margin, her hoop stats, regular season only.
  # 2024 and 2025 match wehoop exactly. 2026's win_by matches wehoop too; lose_by differs
  # only because wehoop leaves the commissioner's cup loss in, which is the expected filter gap.
  mutate(win_by  = c(11.50, 15.00, 12.96, 17.35, 12.15, 12.23, 13.85),
         lose_by = c( 4.50,  8.75, 10.80, 14.67,  8.38, 17.07, 16.23)) |>
  mutate(blowout_l_per40 = blowout_l / games * 40,
         lab = if_else(title, "title", ""))

season_bars <- function(yvar, label_fmt, title, subtitle) {
  ggplot(aces_seasons, aes(x = factor(season), y = {{ yvar }}, fill = title)) +
    geom_col(width = 0.66) +
    geom_text(aes(label = sprintf(label_fmt, {{ yvar }})),
              vjust = -0.55, colour = INK, size = 4.4) +
    geom_text(aes(y = 0, label = lab), vjust = 1.6, colour = HI, size = 3.6, fontface = "bold") +
    scale_fill_manual(values = c(`TRUE` = HI, `FALSE` = GRAY), guide = "none") +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.08, 0.16))) +
    labs(title = title, subtitle = subtitle, caption = CREDIT, x = NULL, y = NULL) +
    base_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))
}

# 6. the margin collapse. they keep winning, by less and less
p_season_margin <- season_bars(margin, "%.1f",
  "las vegas point differential per game, by season",
  "red seasons ended in a championship")

# 7. volatility was already there when they won last year
p_season_sd <- season_bars(sd_margin, "%.1f",
  "las vegas margin standard deviation, by season",
  "standard deviation of the final margin, red seasons ended in a championship")

# 8. the killer one. same record in 2024 and 2026, opposite shape
p_season_blowouts <- season_bars(blowout_l_per40, "%.1f",
  "las vegas losses by 15 or more per 40 games, by season",
  "rate adjusted because season length changed, red seasons ended in a championship")

# average margin in the games they actually won. this is the flat one, which is the point:
# vegas has beaten people by 12 to 15 a night for seven straight years, title or not.
p_season_win_margin <- season_bars(win_by, "%.1f",
  "las vegas average margin in wins, by season",
  "wins only, red seasons ended in a championship")

# THE ONE TO USE. same chart split by result. wins are flat, losses fall off a cliff.
# it is the whole "same record, completely different team" line in a single frame.
win_loss_long <- aces_seasons |>
  select(season, win_by, lose_by) |>
  pivot_longer(-season, names_to = "side", values_to = "pts") |>
  mutate(side = factor(if_else(side == "win_by", "when they win", "when they lose"),
                       levels = c("when they win", "when they lose")))

p_season_win_vs_loss <- ggplot(win_loss_long, aes(x = factor(season), y = pts,
                                                  colour = side, group = side)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 4) +
  geom_text(aes(label = sprintf("%.1f", pts)), vjust = -1.1, size = 4, show.legend = FALSE) +
  scale_colour_manual(values = c(`when they win` = GRAY, `when they lose` = HI), name = NULL) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.06, 0.18))) +
  labs(title = "las vegas average margin in wins and in losses, by season",
       subtitle = "the win line is flat, the loss line is not",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E3E3E0", linewidth = 0.3))

p_season_margin
p_season_win_margin
p_season_win_vs_loss
p_season_sd
p_season_blowouts

# ---------------------------------------------------------------------------
# 2025 VS 2026. the "they won the title doing this exact thing" beat.
#
# 2025 needs no commissioner's cup filter: wehoop returns 44 games and 30-14 for
# las vegas, which matches her hoop stats exactly, so vegas was not in that cup final.
# filtering to one team also sidesteps the all star roster problem entirely.

lv_two <- load_wnba_team_box(seasons = 2025:2026) |>
  filter(season_type == 2, team_location == LV, game_date != CUP_FINAL) |>
  mutate(margin = team_score - opponent_team_score) |>
  arrange(season, game_date) |>
  group_by(season) |>
  mutate(g = row_number()) |>
  ungroup()

season_labs <- lv_two |>
  group_by(season) |>
  summarise(w = sum(margin > 0), l = sum(margin < 0),
            sd_m = sd(margin), bl = sum(margin <= -15), .groups = "drop") |>
  mutate(lab = sprintf("%d  ·  %d-%d  ·  margin sd %.1f  ·  blown out %d times%s",
                       season, w, l, sd_m, bl,
                       if_else(season == 2025, "  ·  WON THE TITLE", "")))

lv_two <- lv_two |> left_join(season_labs |> select(season, lab), by = "season")

p_2025_vs_2026 <- ggplot(lv_two, aes(x = g, y = margin, fill = margin > 0)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, colour = INK, linewidth = 0.5, linetype = "22") +
  facet_wrap(~lab, ncol = 1, scales = "free_x") +
  scale_fill_manual(values = c(`TRUE` = WIN_G, `FALSE` = LOSS_R), guide = "none") +
  labs(title = "las vegas final margin by game, 2025 and 2026",
       subtitle = "final margin of every game, wins above the line and losses below",
       caption = CREDIT, x = NULL, y = NULL) +
  base_theme() +
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        strip.text = element_text(colour = INK, size = 12, face = "bold", hjust = 0),
        panel.spacing.y = unit(18, "pt"))

# each season on its own, as single cards. same scale on both so they can be cut
# back to back in the edit and the shapes are directly comparable.
Y_BOTH <- range(lv_two$margin) + c(-2, 2)

season_card <- function(yr) {
  d <- filter(lv_two, season == yr)
  r <- season_labs[season_labs$season == yr, ]
  ggplot(d, aes(x = g, y = margin, fill = margin > 0)) +
    geom_col(width = 0.7) +
    geom_hline(yintercept = 0, colour = INK, linewidth = 0.5, linetype = "22") +
    scale_fill_manual(values = c(`TRUE` = WIN_G, `FALSE` = LOSS_R), guide = "none") +
    scale_y_continuous(limits = Y_BOTH) +
    labs(title = sprintf("las vegas final margin by game, %d", yr),
         subtitle = sprintf("%d-%d  \u00b7  margin sd %.1f  \u00b7  %d losses by 15 or more%s",
                            r$w, r$l, r$sd_m, r$bl,
                            if (yr == 2025) "  \u00b7  won the title" else ""),
         caption = CREDIT, x = NULL, y = NULL) +
    base_theme() +
    theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank())
}

p_2025 <- season_card(2025)
p_2026 <- season_card(2026)

p_2025_vs_2026
p_2025
p_2026
