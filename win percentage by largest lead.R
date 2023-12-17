#win percentage by largest lead

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr)


#wnba team full box score
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(seasons=c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023))
})

tictoc::toc()


# install.packages("dplyr")
library(dplyr)

# Convert largest_lead to integer and calculate the empirical win percentage
result <- wnba_team_box %>%
  mutate(largest_lead = as.integer(largest_lead)) %>%  # Convert to integer
  group_by(largest_lead) %>%
  summarize(empirical_win_percentage = mean(team_winner) * 100) %>%
  arrange(largest_lead)  # Arrange in ascending order by largest_lead

# Print the result
print(result, n=48)


# Install and load the ggplot2 package if not already installed
# install.packages("ggplot2")
library(ggplot2)

# Create a scatter plot with labels for every 5th x-axis number
ggplot(result, aes(x = largest_lead, y = empirical_win_percentage)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, max(result$largest_lead, na.rm = TRUE), by = 5)) +  # Set breaks for every 5th x-axis number
  labs(title = "Win Percentage vs Largest Lead in WNBA 2003-2023",
       x = "Largest Lead",
       y = "Win Percentage")




