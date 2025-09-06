#Use wehoop package to download WNBA data

#this loads all the packages I need at once, i could instead just do "library(wehoop)" for each package
pacman::p_load(wehoop, dplyr, glue, tictoc, progressr)

#load data
#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box()
})
tictoc::toc()

#find W/L sequence for each team
wnba_2025 <- wnba_team_box %>% filter(season==2025) %>% arrange(desc(game_date))


# 1) Ensure chronological per team and binary outcome
wl_seq <- wnba_2025 %>%
  filter(game_date < as.Date("2025-08-31")) %>%
  arrange(team_name, game_date) %>%
  mutate(win = as.integer(team_winner)) %>%         # 1 = win, 0 = loss
  group_by(team_name) %>%
  mutate(run_id = consecutive_id(win)) %>%          # run id for W/L streaks
  ungroup()

team_streaks <- wl_seq %>%
  group_by(team_name, run_id, win) %>%
  summarise(run_length = n(),
            start_date = first(game_date),
            end_date   = last(game_date),
            .groups = "drop") %>%
  mutate(run_type = ifelse(win == 1L, "W", "L"))

runs_z <- wl_seq %>%
  group_by(team_name) %>%
  summarise(
    W = sum(win, na.rm = TRUE),
    L = sum(1L - win, na.rm = TRUE),
    N = n(),
    Runs = n_distinct(run_id),
    .groups = "drop"
  ) %>%
  mutate(
    mu     = (2 * W * L) / N + 1,
    sigma2 = ((mu - 1) * (mu - 2)) / (N - 1),
    sigma  = sqrt(pmax(sigma2, 0)),
    # continuity correction optional when N < ~50 as in references
    z      = dplyr::case_when(
               N < 50 & Runs < mu  ~ (Runs - mu + 0.5) / sigma,
               N < 50 & Runs >= mu ~ (Runs - mu - 0.5) / sigma,
               TRUE             ~ (Runs - mu) / sigma
             )
  )

runs_z %>% filter(team_name != "TEAM CLARK" & team_name != "TEAM COLLIER") %>% arrange(desc(z)) %>% select(team_name, W, L, Runs, z)
