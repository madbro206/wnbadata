library(readr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(gt)
library(scales)
library(wehoop)
library(progressr)
library(tictoc)
library(gganimate)

#2025 wnba player basic and advanced stats from
#https://www.basketball-reference.com/wnba/years/2025_advanced.html
data <- read.csv("~/Desktop/2025_wnba.csv")

#wehoop pbp data
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp(season=2025)
})
tictoc::toc()

#ast % chart
filtered_data <- data %>% dplyr::filter(`AST.` > 30)

ggplot(filtered_data, aes(x = Player, y = `AST.`)) +  # Use AST. not AST_PCT
  geom_bar(stat = "identity") +
  labs(
    title = "title",
    subtitle = "subtitle",
    caption = "data: Basketball Reference | chart: @wnbadata") +
  theme_minimal()

#alyssa thomas assists

assists_data <- wnba_pbp %>% 
  filter(season_type==2) %>% #regular season
  filter(home_team_name !="Team Clark") %>% #not all star
  dplyr::filter(grepl("(Alyssa Thomas assists)", text, fixed = TRUE))

assists_data$text


court <- geom_basketball(league = "wnba", display_range = "offense", rotation=270)

# Add your assist data on top
court + 
  geom_point(data = assists_data, 
             aes(x = -coordinate_x_raw+25, y = coordinate_y_raw-43),
             size = 2, alpha = 0.6, color = "#01696f") +
  labs(
    title = "Where Alyssa Thomas Creates Shots",
    subtitle = "Location of all 357 assisted field goals, 2025 season",
    caption = "data: ESPN play-by-play | chart: @wnbadata"
  )

#assists_data %>% arrange(desc(coordinate_x_raw))%>% select(id,text, game_date, period_number, clock_display_value, coordinate_x_raw, coordinate_y_raw)


#animated court
assists_data <- assists_data %>%
  mutate(assist_number = row_number())

p_old <- court + 
  geom_point(data = assists_data, 
             aes(x = -coordinate_x_raw+25, y = coordinate_y_raw-43),
             size = 2, alpha = 0.6, color = "#01696f") +
  transition_states(assist_number, transition_length = 1, state_length = 0) +
  shadow_mark(past = TRUE, future = FALSE, alpha = 0.6) +  # Keep previous dots
  labs(
    title = "Where Alyssa Thomas Creates Shots",
    subtitle = "Assist {closest_state} of 357, 2025 season",
    caption = "data: ESPN play-by-play | chart: @wnbadata"
  )

p <- court + 
  geom_point(data = assists_data, 
              aes(x = -coordinate_x_raw+25, y = coordinate_y_raw-43),
              size = 2, alpha = 0.6, color = "#01696f") +
  transition_states(assist_number, transition_length = 1, state_length = 0) +
  shadow_mark(past = TRUE, future = FALSE, alpha = 0.6) +
  labs(
    title = "Where Alyssa Thomas Creates Shots",
    subtitle = "Assist {closest_state} of 357, 2025 season",
    caption = "data: ESPN play-by-play | chart: @wnbadata"
  ) +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    plot.caption = element_text(size = 12),
    plot.title.position = "plot",  # Extends title into plot area
    plot.margin = margin(20, 10, 10, 10)  # Add top margin for text
  )

anim <- animate(p, nframes = 200, fps = 20, width = 800, height = 800, res = 100)
anim_save("alyssa_thomas_assists.gif", animation = anim)
