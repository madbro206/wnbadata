#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, tidyr, glue, tictoc, progressr, BasketballAnalyzeR, ggplot2, gt)

#load data
#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box()
})
tictoc::toc()

##############################################################################################################
#group by team, then calculate minutes by players when starter==TRUE and when starter==FALSE (bench)

data <- wnba_player_box %>%
	filter(team_name !='Team USA' & team_name !='Team WNBA') %>%
	group_by(team_name, starter) %>%
	summarize(min=sum(minutes, na.rm=TRUE))

data_summary <- data %>%
  pivot_wider(names_from = starter, values_from = min, names_prefix = "min_") %>%
  rename(starter_minutes = min_TRUE, bench_minutes = min_FALSE)

data_summary$total <- data_summary$bench_minutes + data_summary$starter_minutes

data_summary <- data.frame(data_summary)

#data table
gt(data_summary)

data_summary %>%
  gt() %>%
    tab_header(title = md("Which WNBA team uses their bench the most/least?"),
               subtitle = md("2024 season through Aug 20")) %>%
  tab_footnote(
    footnote = "Source: stats.wnba.com") %>%
  cols_label( team_name = "Team", bench_minutes = "Bench Minutes", starter_minutes = "Starter Minutes" ) 

##############################################################################################################
#make chart

# Prepare the data
data_long <- data_summary %>%
  pivot_longer(
    cols = c(bench_minutes, starter_minutes),
    names_to = "minute_type",
    values_to = "minutes"
  ) %>%
  group_by(team_name) %>%
  mutate(total_minutes = sum(minutes),
         percentage = (minutes / total_minutes) * 100) %>%
  ungroup()

# Calculate the maximum percentage of starter minutes for each team
data_summary_reordered <- data_long %>%
  filter(minute_type == "starter_minutes") %>%
  arrange(percentage) %>%
  mutate(team_name = factor(team_name, levels = unique(team_name)))

# Join the reordered factor levels back to the full dataset
data_long <- data_long %>%
  mutate(team_name = factor(team_name, levels = levels(data_summary_reordered$team_name)))
  
custom_colors <- c("starter_minutes" = "#f88158", "bench_minutes" = "#fceecb")  
  
  
# Plot with horizontal bars and percentage labels
ggplot(data_long, aes(x = team_name, y = percentage, fill = minute_type)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(percentage / 100, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 4) +
  coord_flip() +  # Flip the coordinates for horizontal bars
  labs(
    title = "Which WNBA team uses their bench the most?",
    subtitle = "2024 season through Aug 20",
    x = NULL,
    y = "Percentage of Total Team Playing Time",
    caption = "Source: stats.wnba.com | Graphic: @wnbadata",
    fill = "Type"
  ) +
  scale_fill_manual(values = custom_colors) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0),
    plot.caption= element_text(hjust=1.5, vjust=3),
    axis.text.x = element_text(angle = 0, hjust = 0),
    axis.text.y = element_text(size = 12)  # Adjust the size of the team labels here
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))
##############################################################################################################
#calculations for 2023 stats

#load data
#wnba player full box score
tictoc::tic()
progressr::with_progress({
  wnba_player_box2 <- wehoop::load_wnba_player_box(season=2023)
})
tictoc::toc()

#group by team, then calculate minutes by players when starter==TRUE and when starter==FALSE (bench)

data2 <- wnba_player_box2 %>%
	filter(team_name !='Team Wilson' & team_name !='Team Stewart' & season_type==2) %>%
	group_by(team_name, starter) %>%
	summarize(min=sum(minutes, na.rm=TRUE))

data_summary2 <- data2 %>%
  pivot_wider(names_from = starter, values_from = min, names_prefix = "min_") %>%
  rename(starter_minutes = min_TRUE, bench_minutes = min_FALSE)

#data_summary2 $total <- data_summary2 $bench_minutes + data_summary2 $starter_minutes

data_summary2$pct_start <- data_summary2$starter_minutes/(data_summary2 $bench_minutes + data_summary2 $starter_minutes) *100

data_summary2 <- data.frame(data_summary2)

data_summary2 <- data_summary2 %>%
	arrange(desc(pct_start))

#data table
data_summary2 %>%
  gt() %>%
    tab_header(title = md("Which WNBA team uses their bench the most/least?"),
               subtitle = md("2023 season")) %>%
  tab_footnote(
    footnote = "Source: stats.wnba.com") %>%
  cols_label( team_name = "Team", bench_minutes = "Bench Minutes", starter_minutes = "Starter Minutes", pct_start="Starter % of Total" ) 
