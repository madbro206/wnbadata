library(ggplot2)

# Set parameters
p <- 1/12
k_values <- 0:30

# Calculate PMF
pmf_values <- dgeom(k_values, prob = p)

# Create data frame for plotting
pmf_data <- data.frame(k = k_values, pmf = pmf_values)

# Plot the connected dot plot
ggplot(pmf_data, aes(x = k, y = pmf)) +
  geom_line(color = "#000000", linewidth = 1) +
  geom_point(color = "#6ECEB2", size = 3) +
  labs(title = "Probability of X seasons before Liberty's first title",
       subtitle = "Geometric distribution with p=1/12 (assuming 12 teams/season)",
       x = "Number of Non-Champ Seasons Before First Title",
       y = "Probability") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)
  
#chance of 27 or more failures before success
prob <- (1-p)^27
print(prob)

#chance of 28 failures before success
(1-p)^28

#actual probability considering number of teams, 27 yrs
(7/8)*(9/10)*(11/12)*(15/16)^3*(13/14)*(12/13)^2*(13/14)*(12/13)*(13/14)*(12/13)*(11/12)^14

#actual probability considering number of teams, 28 yrs
(7/8)*(9/10)*(11/12)*(15/16)^3*(13/14)*(12/13)^2*(13/14)*(12/13)*(13/14)*(12/13)*(11/12)^15