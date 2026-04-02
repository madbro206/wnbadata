library(ggplot2)

p <- 0.387      # UConn 3P%
n <- 20         # attempts

# P(X = k) for k = 0,...,20
k <- 0:n
pmf <- dbinom(k, size = n, prob = p)

# P(X <= 4)
p_le_4 <- pbinom(4, size = n, prob = p)
p_le_4



df <- data.frame(
  makes = k,
  prob  = pmf,
  cold  = k <= 4
)

ggplot(df, aes(x = makes, y = prob, fill = cold)) +
  geom_col(color = "#000E2F") +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "#7C878E")) +
    labs(
      title = "UConn 3P Outcomes Under Binomial Model",
      subtitle = "X ~ Binom(20, 0.387)",
      x = "Made threes (k)",
      y = "Probability P(X = k)"
    ) +
  theme_minimal() +
  theme(legend.position = "none")