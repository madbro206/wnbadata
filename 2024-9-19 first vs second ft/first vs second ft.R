#Use wehoop package to download WNBA data

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, tidyverse, gt)

#wnba full play by play
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp(c(2024))
})
tictoc::toc()

#filter to only free throws
ft <- wnba_pbp %>%
  filter(grepl("Free Throw", type_text) & type_text != "Free Throw")

##############################################################################################################
#column with made/missed info
ft$make <- NA

#make=1
ft$make[grepl("makes", ft$text, ignore.case = TRUE)] <- 1

#miss=0
ft$make[grepl("misses", ft$text, ignore.case = TRUE)] <- 0


#make column with first, second, third, etc info
ft$ft_category <- NA

# Categorize free throws
ft$ft_category <- case_when(
  grepl("1 of 2", ft$type_text) ~ "First of 2",
  grepl("2 of 2", ft$type_text) ~ "Second of 2",
  grepl("1 of 3", ft$type_text) ~ "First of 3",
  grepl("2 of 3", ft$type_text) ~ "Second of 3",
  grepl("3 of 3", ft$type_text) ~ "Third of 3",
  grepl("1 of 1", ft$type_text) ~ "First of 1",
  grepl("Technical", ft$type_text) ~ "First of 1",
  TRUE ~ "Single"  # Default case for any unspecified types
)

#calculate percentages for each category
ft_percentages <- ft %>%
  group_by(ft_category) %>%
  summarize(
    total_attempts = n(),
    total_makes = sum(make),
    percentage_made = mean(make) * 100
  ) %>%
  arrange(desc(percentage_made))

# View the results
print(ft_percentages)
##############################################################################################################
#table
ft_percentages %>%
  gt() %>%
  tab_header(
    title = "Free Throw Percentages by Category",
    subtitle = "Made vs Missed, WNBA 2024"
  ) %>%
  fmt_number(
    columns = c(total_attempts, total_makes),
    decimals = 0
  ) %>%
  fmt_percent(
    columns = c(percentage_made),
    decimals = 1,
    scale_values = FALSE
  ) %>%
  cols_label(
    ft_category = "Category",
    total_attempts = "Total Attempts",
    total_makes = "Total Makes",
    percentage_made = "Percentage Made"
  )


#visualization
# Add a column for percentage missed
ft_percentages <- ft_percentages %>%
  mutate(percentage_missed = 100 - percentage_made)

# Reshape the data for plotting
ft_long <- ft_percentages %>%
  select(ft_category, percentage_made, percentage_missed) %>%
  pivot_longer(cols = c(percentage_made, percentage_missed), 
               names_to = "type", 
               values_to = "percentage")

#stacked bar chart with labels
ggplot(ft_long, aes(x = ft_category, y = percentage, fill = type)) +
  geom_bar(stat = "identity") +
  geom_text(data = ft_long %>% filter(type == "percentage_made"),
            aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.94),
            color = "white",
            size = 3.5) +
  geom_hline(yintercept = 78.6, linetype = "dotted", color = "red", linewidth = 0.8) +
  annotate("text", x = Inf, y = 78.6, label = "League Average: 78.6%", vjust = -0.5, hjust = 1.1, color = "red") +
  scale_fill_manual(values = c("lightgray", "steelblue"), 
                    labels = c("Missed", "Made")) +
  labs(title = "Free Throw Percentages by Type",
       subtitle= "2024 WNBA Season Through 39 Games",
       x = "Free Throw Attempt Type",
       y = "Percentage",
       caption="data: stats.wnba.com | graphic: @wnbadata") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##############################################################################################################
#chi square test for 2024 data only (perplexity.ai)
# Recreate the data
ft_percentages <- data.frame(
  ft_category = c("Third of 3", "Second of 3", "Second of 2", "Single", "First of 2", "First of 3"),
  total_attempts = c(103, 103, 3550, 1136, 3550, 103),
  total_makes = c(92, 84, 2888, 875, 2705, 78)
)

# Create a contingency table
contingency_table <- cbind(
  ft_percentages$total_makes,
  ft_percentages$total_attempts - ft_percentages$total_makes
)

# Perform chi-square test
chi_square_result <- chisq.test(contingency_table)

# Print the results
print(chi_square_result)