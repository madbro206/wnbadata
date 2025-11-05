#Use wehoop package to download WNBA data

#this loads all the packages I need at once, i could instead just do "library(wehoop)" for each package
pacman::p_load(wehoop, dplyr, tidyr, lubridate, glue, tictoc, progressr, lubridate, ggplot2)

#load data
#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season=c(2004:2025))
})
tictoc::toc()

#convert date and arrange
wnba_team_box <- wnba_team_box %>%
  mutate(game_date = as.Date(game_date)) %>%
  arrange(team_name, game_date)

# update wnba_team_box with days since last game
wnba_team_box <- wnba_team_box %>%
  arrange(team_name, game_date) %>%
  group_by(team_name, season) %>%
  mutate(
    prev_game_date = lag(game_date),
    days_since_last_game = as.integer(game_date - prev_game_date),
    back_to_back = if_else(is.na(prev_game_date), FALSE, days_since_last_game == 1)
  ) %>%
  ungroup()


#list of second game of back-to-backs
b2b_games <- wnba_team_box %>%
  filter(back_to_back == TRUE)

b2b_games


#count back to backs and season length for each season
season_analysis <- wnba_team_box %>%
  filter(season_type == 2) %>%  # Regular season only
  group_by(season) %>%
  summarise(
    # Season length metrics
    days_with_games = n_distinct(game_date),
    first_game = min(game_date),
    last_game = max(game_date),
    season_span_days = as.integer(last_game - first_game) + 1,
    
    # Back-to-back count
    total_b2b_games = sum(back_to_back == TRUE, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  mutate(
    # Calculate derived metrics
    b2b_percentage = round((total_b2b_games / days_with_games) * 100, 2),
    avg_days_between = round(season_span_days / days_with_games, 2)
  ) %>%
  arrange(season)

print(season_analysis, n = 21)

season_analysis %>%select(season,season_span_days, total_b2b_games, b2b_percentage) %>% arrange(desc(season))

season25<-wnba_team_box%>%filter(season==2025 & season_type==2)
length(unique(season25$game_date))

####################################################################################################
#charts

#reshape

season_analysis_long <- season_analysis %>%
  select(season, total_b2b_games, season_span_days, days_with_games) %>%
  pivot_longer(
    cols = c(total_b2b_games, season_span_days, days_with_games),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = case_when(
      metric == "total_b2b_games" ~ "Back-to-Back Games",
      metric == "season_span_days" ~ "Season Span (days)",
      metric == "days_with_games" ~ "Days with Games"
    )
  )

#line chart
ggplot(season_analysis_long, aes(x = season, y = value, color = metric, group = metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  labs(
    title = "WNBA Season Length and Back-to-Back Games Over Time",
    subtitle = "2004-2025 Regular Seasons",
    x = "Season",
    y = "Count (days or games)",
    color = "Metric"
  ) +
  scale_x_continuous(breaks = seq(2004, 2025, 1)) +  # Changed from 2 to 1
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate labels so they don't overlap
  )



ggplot(season_analysis %>% filter(season != 2020), aes(x = days_with_games, y = total_b2b_games)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray", linetype = "dashed") +
  geom_point(size = 2.5, alpha = 0.8, color="darkblue") +
  geom_text(aes(label = season), vjust = -0.5, size = 4) +
  labs(
    title = "WNBA Schedule Density: Back-to-Backs vs Season Length",
    subtitle = "2004-2025, Excludes 2005 and 2020 season",
    x = "Number of Days with Regular Season Games",
    y = "Total Back-to-Back Games",
    color = "Season"
  ) +
  theme_minimal()
  
  

####################################################################################################
#summarize win/loss by home/away
b2b_summary <- b2b_games %>%
  mutate(result = if_else(team_winner, "Win", "Loss")) %>%
  group_by(team_home_away, result) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = result, values_from = n, values_fill = 0) %>%
  mutate(total = Win + Loss, win_rate = Win / total)

print(b2b_summary)

print(b2b_games %>% select("game_date", "team_name", "team_home_away", "team_winner"), n=30)

####################################################################################################
#chatgpt to help me scrape all season data (because wehoop doesn't have every old season)
library(rvest)
library(dplyr)
library(tidyr)
library(lubridate)

# Function to scrape one season's schedule
scrape_wnba_season <- function(year) {
  
  cat(paste0("Scraping ", year, " season...\n"))
  
  # Add delay BEFORE the request
  wait_time <- runif(1, 5, 8)
  cat(paste0("  Waiting ", round(wait_time, 1), " seconds before request...\n"))
  Sys.sleep(wait_time)
  
  url <- paste0("https://www.basketball-reference.com/wnba/years/", year, "_games.html")
  
  # Read the page with retry logic
  page <- tryCatch({
    read_html(url)
  }, error = function(e) {
    cat(paste0("  First attempt failed, waiting 10 seconds and retrying...\n"))
    Sys.sleep(10)
    read_html(url)
  })
  
  # Basketball Reference stores tables in HTML comments
  # Get all comment nodes
  comments <- page %>% 
    html_nodes(xpath = '//comment()') %>%
    html_text()
  
  # Find the comment containing the games table
  games_comment <- comments[grepl('id=\"games\"', comments)]
  
  # Try to get the table from comments first, then regular
  schedule_table <- NULL
  
  if(length(games_comment) > 0) {
    # Parse from comment
    schedule_table <- tryCatch({
      read_html(games_comment[1]) %>%
        html_node("table") %>%
        html_table(fill = TRUE)
    }, error = function(e) NULL)
  }
  
  # If comment didn't work, try regular way
  if(is.null(schedule_table)) {
    schedule_table <- tryCatch({
      page %>%
        html_node("#games") %>%
        html_table(fill = TRUE)
    }, error = function(e) {
      # Last resort: try finding any table with class "sortable stats_table"
      page %>%
        html_node("table.sortable.stats_table") %>%
        html_table(fill = TRUE)
    })
  }
  
  # If we still don't have a table, throw error
  if(is.null(schedule_table) || nrow(schedule_table) == 0) {
    stop("Could not find schedule table")
  }
  
  # Fix column names - handle empty, NA, and duplicate names
  col_names <- names(schedule_table)
  
  # First pass: replace empty/NA names
  for(i in 1:length(col_names)) {
    if(is.na(col_names[i]) || col_names[i] == "") {
      col_names[i] <- paste0("Col", i)
    }
  }
  
  # Second pass: make unique (handles duplicates)
  col_names <- make.unique(col_names, sep = "_")
  names(schedule_table) <- col_names
  
  # Find the Date column (might have different names)
  date_col <- NULL
  if("Date" %in% names(schedule_table)) {
    date_col <- "Date"
  } else {
    # Look for first column that might be a date
    for(col in names(schedule_table)) {
      if(grepl("date|day", col, ignore.case = TRUE)) {
        date_col <- col
        break
      }
    }
    # If still not found, assume first column
    if(is.null(date_col)) date_col <- names(schedule_table)[1]
  }
  
  # Clean up the data - more flexible filtering
  schedule_clean <- schedule_table %>%
    # Rename date column to standard name
    rename(Date = !!sym(date_col))
  
  # Find the row where "Playoffs" appears and cut off there
  playoff_row <- which(schedule_clean$Date == "Playoffs")
  if(length(playoff_row) > 0) {
    # Keep only rows before the first "Playoffs" marker
    schedule_clean <- schedule_clean[1:(playoff_row[1] - 1), ]
  }
  
  schedule_clean <- schedule_clean %>%
    # Remove header rows and empty rows
    filter(
      !is.na(Date) & 
      Date != "" & 
      !Date %in% c("Date", "Day")
    ) %>%
    # Try multiple date parsing strategies
    mutate(
      # Try with year appended
      game_date = case_when(
        # Try parsing as-is with year
        !is.na(mdy(paste(Date, year))) ~ mdy(paste(Date, year)),
        # Try with comma (e.g., "Jun 21, 1997")
        grepl(",", Date) ~ mdy(Date),
        # Try parsing without year if already included
        !is.na(mdy(Date)) ~ mdy(Date),
        # Try ymd format
        !is.na(ymd(Date)) ~ ymd(Date),
        TRUE ~ as.Date(NA)
      ),
      # Use the column names we have (they might vary by year)
      visitor_team = if("Visitor/Neutral" %in% names(.)) {
        `Visitor/Neutral`
      } else if("Visitor" %in% names(.)) {
        Visitor
      } else {
        .[[3]]
      },
      home_team = if("Home/Neutral" %in% names(.)) {
        `Home/Neutral`
      } else if("Home" %in% names(.)) {
        Home
      } else {
        .[[5]]
      },
      season = year
    ) %>%
    select(season, game_date, visitor_team, home_team) %>%
    filter(!is.na(game_date))
  
  cat(paste0("  Parsed ", nrow(schedule_clean), " regular season games\n"))
  
  return(schedule_clean)
}


# Function to calculate back-to-backs for a season
calculate_season_b2b <- function(schedule_df) {
  
  # If no games, return empty summary
  if(nrow(schedule_df) == 0) {
    return(list(
      summary = tibble(
        season = NA_integer_,
        days_with_games = 0L,
        first_game = as.Date(NA),
        last_game = as.Date(NA),
        season_span_days = NA_integer_,
        total_b2b_games = 0L,
        b2b_percentage = NA_real_,
        avg_days_between = NA_real_
      ),
      team_schedule = tibble()
    ))
  }
  
  # Create a row for each team's games
  visitor_games <- schedule_df %>%
    select(season, game_date, team = visitor_team) %>%
    mutate(home_away = "away")
  
  home_games <- schedule_df %>%
    select(season, game_date, team = home_team) %>%
    mutate(home_away = "home")
  
  # Combine all games
  all_games <- bind_rows(visitor_games, home_games) %>%
    arrange(team, game_date)
  
  # Calculate back-to-backs
  team_schedule <- all_games %>%
    group_by(team, season) %>%
    arrange(game_date) %>%
    mutate(
      prev_game_date = lag(game_date),
      days_since_last = as.integer(game_date - prev_game_date),
      is_back_to_back = if_else(is.na(prev_game_date), FALSE, days_since_last == 1)
    ) %>%
    ungroup()
  
  # Summarize for the season
  season_summary <- schedule_df %>%
    summarise(
      season = first(season),
      days_with_games = n_distinct(game_date),
      first_game = min(game_date),
      last_game = max(game_date),
      season_span_days = as.integer(max(game_date) - min(game_date)) + 1,
      total_b2b_games = sum(team_schedule$is_back_to_back)
    ) %>%
    mutate(
      b2b_percentage = round((total_b2b_games / days_with_games) * 100, 2),
      avg_days_between = round(season_span_days / days_with_games, 2)
    )
  
  return(list(
    summary = season_summary,
    team_schedule = team_schedule
  ))
}


# Main execution: Scrape all seasons from 1997-2025
years <- 1997:2025

all_schedules <- list()
all_summaries <- list()

cat("Starting scraping process. This will take about 3-5 minutes...\n\n")

for(year in years) {
  tryCatch({
    # Scrape the season
    schedule <- scrape_wnba_season(year)
    
    # Calculate metrics
    results <- calculate_season_b2b(schedule)
    
    # Store results
    all_schedules[[as.character(year)]] <- results$team_schedule
    all_summaries[[as.character(year)]] <- results$summary
    
    cat(paste0("  ✓ Completed ", year, " (", nrow(schedule), " games)\n\n"))
    
  }, error = function(e) {
    cat(paste0("  ✗ Error scraping ", year, ": ", e$message, "\n\n"))
  })
}

# Combine all summaries and remove empty rows
season_analysis2 <- bind_rows(all_summaries) %>%
  filter(!is.na(season))

print(season_analysis2, n = Inf)

season_analysis2%>%select(season,season_span_days, total_b2b_games)%>%arrange(desc(total_b2b_games))

#write.csv(season_analysis2, "wnba_b2b_analysis_full.csv", row.names = FALSE)
#write.csv(bind_rows(all_schedules), "wnba_team_schedules_full.csv", row.names = FALSE)


#################################################################
#visualizations (all seasons)
ggplot(season_analysis2, aes(x = season, y = b2b_percentage)) +
  geom_line(linewidth = 1.2, color = "#1f77b4") +
  geom_point(size = 3, color = "#1f77b4") +
  geom_smooth(method = "loess", se = TRUE, color = "gray", linetype = "dashed", alpha = 0.2) +
  annotate("rect", xmin = 2013, xmax = 2025, ymin = 0, ymax = 100, alpha = 0.1, fill = "green", label = "Policy changes") +
  annotate("text", x = 2019, y = 90, label = "Scheduling\nreform era", size = 3, color = "darkgreen") +
  labs(
    title = "WNBA Back-to-Back Games: Dramatic Reduction Since 2000",
    subtitle = "Percentage of team games that were 2nd game of back-to-back",
    x = "Season",
    y = "Back-to-Back Percentage (%)",
    caption = "Data from Basketball-Reference.com"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.major.y = element_line(color = "gray90")
  )

  ggplot(season_analysis2, aes(x = season)) +
    # Left axis: Season span
    geom_line(aes(y = season_span_days, color = "Season Span (days)"), linewidth = 1.2) +
    geom_point(aes(y = season_span_days, color = "Season Span (days)"), size = 2.5) +
    # Right axis: B2B games (scaled)
    geom_line(aes(y = total_b2b_games * 2, color = "Back-to-Backs (scaled)"), linewidth = 1.2, linetype = "dashed") +
    geom_point(aes(y = total_b2b_games * 2, color = "Back-to-Backs (scaled)"), size = 2.5) +
    scale_y_continuous(
      name = "Season Span (days)",
      sec.axis = sec_axis(~./2, name = "Total Back-to-Back Games")
    ) +
    scale_color_manual(values = c("Season Span (days)" = "#2ca02c", "Back-to-Backs (scaled)" = "#d62728")) +
    labs(
      title = "Schedule Compression vs Back-to-Back Frequency",
      subtitle = "More compressed seasons = more back-to-backs",
      x = "Season",
      color = "Metric"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(color = "#2ca02c"),
      axis.title.y.right = element_text(color = "#d62728"),
      legend.position = "bottom"
    )


###########################################################
# Create era groupings if not already there
# Scatterplot: Season Span vs Back-to-Backs
ggplot(season_analysis2, aes(x = season_span_days, y = total_b2b_games, size = days_with_games)) +
  geom_point(alpha = 0.7, size =1.5) +
  geom_text(aes(label = season), vjust = -.2, size = 5, show.legend = FALSE, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "gray30", linetype = "dashed", linewidth = 0.8) +

  scale_size_continuous(name = "Game Days", range = c(2, 8)) +
  labs(
    title = "WNBA Scheduling Evolution: 1997 to 2025",
    x = "Season Span (calendar days)",
    y = "Total Back-to-Back Games",
    color = "Era",
    size = "Game Days"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "gray90")
  )

cor(season_analysis2$season, season_analysis2$season_span_days)
