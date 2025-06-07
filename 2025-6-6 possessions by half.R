#Use wehoop package to download WNBA or NCAAW data
pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, tidyr, stringr)

#load data
tictoc::tic()
progressr::with_progress({
  wbb_pbp <- wehoop::load_wbb_pbp()
})
tictoc::toc()


calculate_possessions <- function(wbb_pbp) {
  #split data into first half and second half (including OT)
  first_half <- wbb_pbp %>% filter(period %in% 1:2)
  second_half <- wbb_pbp %>% filter(period %in% 3:6)
  
  #Extract stats for possession calculation
  extract_stats <- function(df) {
    df %>%
      mutate(
        FGA = str_count(type_text, regex("JumpShot|LayUpShot|TipShot", ignore_case = TRUE)),
        OffReb = str_count(type_text, fixed("Offensive Rebound", ignore_case = TRUE)),
        TO = str_count(type_text, fixed("Lost Ball Turnover", ignore_case = TRUE)),
        FTA = str_count(type_text, regex("MadeFreeThrow|MissedFreeThrow", ignore_case = TRUE))
      )
  }
  
  #Calculate possessions
  process_half <- function(half_data) {
    half_data %>%
      extract_stats() %>%
      group_by(game_id, team_id, home_team_name, game_date) %>%
      summarise(
        FGA = sum(FGA),
        OffReb = sum(OffReb),
        TO = sum(TO),
        FTA = sum(FTA),
        .groups = "drop"
      ) %>%
      mutate(Possessions = (FGA - OffReb) + TO + 0.44 * FTA)
  }
  
  # Return nested results
  list(
    first_half = process_half(first_half),
    second_half = process_half(second_half)
  )
}

possessions_results <- calculate_possessions(wbb_pbp)

subset(possessions_all, game_id==401700473 & team_id==2229)


mean(possessions_results$first_half$Possessions)
mean(possessions_results$second_half$Possessions)



possessions_all <- bind_rows(
  possessions_results$first %>% mutate(half = "first"),
  possessions_results$second %>% mutate(half = "second")
)

possessions_all %>%
  group_by(half) %>%
  summarise(avg_possessions = mean(Possessions, na.rm = TRUE))

possessions_wide <- possessions_all %>%
  select(game_id, team_id, home_team_name, game_date, half, Possessions) %>%
  pivot_wider(names_from = half, values_from = Possessions) %>%
    mutate(diff = first-second)

possessions_wide <- subset(possessions_wide, first>0 & second >0)

mean(possessions_wide$first, na.rm=TRUE)
mean(possessions_wide$second, na.rm=TRUE)
mean(possessions_wide$diff, na.rm=TRUE)
##############################################################################################
#compare fga

# Modified function to compare FGA between halves
compare_fga <- function(possessions_results) {
  bind_rows(
    possessions_results$first_half %>% mutate(half = "first"),
    possessions_results$second_half %>% mutate(half = "second")
  ) %>%
  select(game_id, team_id, home_team_name, game_date, half, FGA) %>%
  pivot_wider(
    names_from = half,
    values_from = FGA,
    names_prefix = "FGA_"
  ) %>%
  mutate(
    FGA_diff = FGA_second - FGA_first,
    second_half_more = FGA_diff > 0
  )
}

# Usage
fga_comparison <- compare_fga(possessions_results)
fga_comparison <- subset(fga_comparison, FGA_first>0 & FGA_second >0)

var(fga_comparison$FGA_first)
var(fga_comparison$FGA_second)

## 2. Overall averages
fga_comparison %>%
  summarise(
    avg_first = mean(FGA_first, na.rm = TRUE),
    avg_second = mean(FGA_second, na.rm = TRUE),
    pct_second_higher = mean(second_half_more, na.rm = TRUE)
  )

## 3. Distribution visualization
boxplot(fga_comparison[,c("FGA_first", "FGA_second")], 
        main = "FGA Distribution by Half")
