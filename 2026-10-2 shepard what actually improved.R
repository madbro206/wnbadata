# ============================================================
# Jessica Shepard 2026 - what actually improved
# Chart 1: give last year's Shepard this year's minutes
# Chart 2: the two rates she really did improve at
# ============================================================

library(tidyverse)

# ---- 1. DATA -------------------------------------------------
# Hard-coded from Her Hoop Stats (stats_v02), WNBA regular season only.
# Small enough that re-pulling on record day isn't worth the risk.
# Provenance:
#
#   Shepard 2025: 40 g, 20.87 mpg, 8.00 / 7.33 / 2.60
#   Shepard 2026: 42 g, 31.75 mpg, 14.48 / 11.12 / 5.31
#   Projection   = 2025 per-minute rate x 31.75 minutes
#   AST% / TOV%  = computed from season totals, not averaged per-game
#                  rates (per-game averaging gives badly wrong answers)

# All three are per-game counts, so they share one honest axis.
projected <- tribble(
  ~stat,       ~projected, ~threshold, ~actual,
  "assists",         3.95,          5,    5.31,
  "rebounds",       11.14,         10,   11.12,
  "points",         12.17,         10,   14.48
) %>%
  mutate(
    stat    = fct_inorder(stat),
    cleared = if_else(projected >= threshold, "cleared", "missed")
  )

# The two rates that survive adjusting for opportunity. Note they move
# in opposite directions and both are improvements.
rates <- tribble(
  ~rate,            ~y2025, ~y2026, ~note,
  "turnover rate",    18.7,   14.1,  "fewer turnovers",
  "assist rate",      19.8,   24.2,  "more assists"
) %>%
  mutate(rate = fct_inorder(rate))

# ---- 2. COLORS ------------------------------------------------
# Same validated CVD-safe pair as the July Shepard video, so the two
# videos read as a set. Checked: protan dE 24.7, normal dE 33.6, all pass.

year_colors    <- c(`2025` = "#eb6834", `2026` = "#2a78d6")
cleared_colors <- c(cleared = "#2a78d6", missed = "#eb6834")

# ---- 3. SHARED THEME ------------------------------------------

theme_hooks <- theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    axis.title = element_text(color = "grey40", size = 10),
    axis.text.y = element_text(size = 10)
  )

# ---- 4. CHART 1: DID SHE ALREADY CLEAR IT ----------------------
# One panel, one axis, three bars. Each bar is last year's Shepard at
# this year's minutes. The black tick on each row is the number she
# needed. Two cross it, one doesn't. That is the whole chart.

chart_thresholds <- ggplot(projected, aes(x = projected, y = stat, fill = cleared)) +
  geom_col(width = 0.6) +
  geom_errorbarh(aes(xmin = threshold, xmax = threshold),
                 height = 0.78, color = "grey15", linewidth = 1.1) +
  geom_text(aes(label = sprintf("%.1f", projected)),
            hjust = -0.25, fontface = "bold", size = 5, color = "grey20") +
  geom_text(aes(x = threshold, y = as.numeric(stat) + 0.42,
                label = paste0("", threshold)),
            vjust = 0, size = 3.4, color = "grey25") +
  scale_fill_manual(values = cleared_colors) +
  scale_y_discrete(expand = expansion(add = c(0.55, 0.85))) +
  scale_x_continuous(limits = c(0, 14), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "Last year's Jessica Shepard with this year's minutes",
    subtitle = "2025 per-minute rates scaled to 31.7 mpg",
    x = NULL, y = NULL,
    caption = "data: Her Hoop Stats | @wnbadata"
  ) +
  theme_hooks +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 13, face = "bold", color = "grey20"),
    plot.title = element_text(face = "bold", size = 17),
    plot.caption = element_text(color = "grey55", size = 9)
  )

# ---- 5. CHART 2: WHAT SHE ACTUALLY IMPROVED AT -------------------
# Both are percentages so they share one axis. The arrows point opposite
# ways on purpose: more assists is good, fewer turnovers is good. The
# grey dot is 2025, the blue dot is 2026.

# Laid out as a table rather than a plot. Four numbers don't need a
# geometry, they just need to be readable. Built in ggplot so it shows
# up in the same plot pane as chart 1.

tbl_rows <- rates %>%
  mutate(row_y = c(1, 2))   # turnover on the bottom, assist on top

chart_rates <- ggplot(tbl_rows) +
  # header rule and row separator
  annotate("segment", x = 0, xend = 4.3, y = 2.62, yend = 2.62,
           color = "grey25", linewidth = 0.8) +
  annotate("segment", x = 0, xend = 4.3, y = 1.5, yend = 1.5,
           color = "grey88", linewidth = 0.5) +
  # header
  annotate("text", x = 1.75, y = 2.8, label = "2025",
           size = 5, fontface = "bold", color = year_colors[["2025"]]) +
  annotate("text", x = 2.75, y = 2.8, label = "2026",
           size = 5, fontface = "bold", color = year_colors[["2026"]]) +
  # row labels
  geom_text(aes(x = 0, y = row_y, label = rate),
            hjust = 0, size = 5.4, fontface = "bold", color = "grey20") +
  # values
  geom_text(aes(x = 1.75, y = row_y, label = sprintf("%.1f%%", y2025)),
            size = 5.4, color = "grey45") +
  geom_text(aes(x = 2.75, y = row_y, label = sprintf("%.1f%%", y2026)),
            size = 5.4, fontface = "bold", color = year_colors[["2026"]]) +
  # plain-english direction, so "turnover rate went down" reads as good
  geom_text(aes(x = 3.45, y = row_y, label = note),
            hjust = 0, size = 4.2, color = "grey45") +
  scale_x_continuous(limits = c(-0.1, 4.6)) +
  scale_y_continuous(limits = c(0.4, 3.1)) +
  labs(
    title = "Jessica Shepard is a passer :)",
    subtitle = "assist rate = share of teammates' baskets she assisted",
    caption = "data: Her Hoop Stats | @wnbadata"
  ) +
  theme_void(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 17, hjust = 0),
    plot.subtitle = element_text(color = "grey40", size = 11, hjust = 0,
                                 margin = margin(b = 14)),
    plot.caption = element_text(color = "grey55", size = 9, hjust = 1),
    plot.margin = margin(16, 16, 10, 16)
  )

# ---- 6. VIEW ------------------------------------------------------
# Positron plot pane. Widen the pane to roughly 3:4 before screenshotting
# for the vertical cut.

chart_thresholds
chart_rates
