#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, stringr, ggplot2)

#load data

#wnba full play by play
tictoc::tic()
progressr::with_progress({
  wbb_pbp <- wehoop::load_wbb_pbp(season=c(2020:2024))
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box(season=c(2020:2024))
})
tictoc::toc()

cb <- wbb_pbp %>% 
  arrange(game_date, game_play_number) %>%  #make sure the log is in order
  filter(str_detect(text, "Cameron Brink") 
    & str_detect(text, regex("Free Throw", ignore_case = TRUE))) %>%
  mutate(result = case_when(str_detect(text, "made") ~ "Make",
        str_detect(text, "missed") ~ "Miss"),
        attempt = row_number())

ggplot(cb, aes(x = attempt, y = 1, color = result)) +
  geom_point(size = 1, alpha = 0.2) +
  scale_color_manual(values = c("Make" = "#0052b0", "Miss" = "#F44336")) +
  labs(
    title = "Cameron Brink's College Career Free Throw Makes and Misses",
    x = "Attempt Number (Chronological Order)",
    y = "",
    color = "Result"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(1, 1))

cb <- cb %>%
  mutate(
    y_pos = ifelse(result == "Make", 1.2, 1)
  )


ggplot(cb, aes(x = attempt, y = y_pos, color = result)) +
  geom_point(size = 1, alpha = 0.8, shape = 15) +
  scale_color_manual(values = c("Make" = "#0052b0", "Miss" = "#F44336")) +
  scale_y_continuous(
    breaks = c(1, 2),
    labels = c("Miss", "Make"),
    limits = c(0.5, 2.5),
    expand = c(0, 0)
  ) +
  labs(
    title = "Cameron Brink's Free Throw Makes & Misses",
    x = "Attempt Number",
    y = "",
    color = "Result"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.y = element_text(size = 14),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )
  









############## where did the streak start and end

# Get the run-length encoding of the result column
r <- rle(cb$result)

# Find the longest streak of "Make"
make_streaks <- which(r$values == "Make")
longest_streak_idx <- make_streaks[which.max(r$lengths[make_streaks])]
streak_length <- r$lengths[longest_streak_idx]

# Calculate the start and end indices of the streak
end_idx <- sum(r$lengths[1:longest_streak_idx])
start_idx <- end_idx - streak_length + 1

start_row <- cb[start_idx, ]
end_row <- cb[end_idx, ]

# View the results
start_row[, c("game_date", "attempt")]
end_row[, c("game_date", "attempt")]



# Run-length encoding to find streaks
r <- rle(cb$result)
make_streaks <- which(r$values == "Make")
longest_streak_idx <- make_streaks[which.max(r$lengths[make_streaks])]
streak_length <- r$lengths[longest_streak_idx]

# Indices for the streak
streak_end <- sum(r$lengths[1:longest_streak_idx])
streak_start <- streak_end - streak_length + 1

# Optional: get attempt numbers for annotation
start_attempt <- cb$attempt[streak_start]
end_attempt <- cb$attempt[streak_end]

# Plot
ggplot(cb, aes(y = attempt, x = 1, color = result)) +
  # Highlight the streak
  geom_rect(aes(xmin = 0.8, xmax = 1.2, ymin = streak_start - 0.5, ymax = streak_end + 0.5),
            fill = "#e0f7fa", alpha = 0.4, inherit.aes = FALSE) +
  # Points for each attempt
  geom_point(size = .5, alpha = 0.8, shape = 15) +
  # Annotate start and end
  annotate("text", x = 1.25, y = streak_start, label = "Streak Start", hjust = 0, vjust = 0.5, size = 4, fontface = "bold", color = "#0052b0") +
  annotate("text", x = 1.25, y = streak_end, label = "Streak End", hjust = 0, vjust = 0.5, size = 4, fontface = "bold", color = "#0052b0") +
  scale_color_manual(values = c("Make" = "#ffffff", "Miss" = "#F44336")) +
  labs(
    title = "Cameron Brink's Free Throw Makes & Misses",
    y = "Attempt Number",
    x = "",
    color = "Result"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  scale_y_continuous(expand = c(0, 0))

cb_miss <- cb %>% filter(result == "Miss")

# Find the maximum attempt number
max_attempt <- max(cb_miss$attempt, na.rm = TRUE)


ggplot(cb_miss, aes(y = attempt, x = 0)) +
  geom_point(size = 0.1, alpha = 1, color = "#F44336") +
  labs(
    title = "Cameron Brink's Free Throw Misses",
    y = "FT Attempt Number",
    x = ""
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank()
  ) +
  scale_x_continuous(
    limits = c(-0.05, 0.5),  # Tighten the x-axis so dots are right next to the y-axis
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, max_attempt, by = 50)
  )
