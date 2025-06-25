min_denom <- function(target, n_max = 5000) {
  # target is a decimal like 0.334, already rounded to 3 dp
  eps   <- 0.0005                      # half of one thousandth
  low   <- target - eps
  high  <- target + eps
  
  for (n in 1:n_max) {                 # grow the denominator
    k_low  <- ceiling(low  * n)        # smallest k that could work
    k_high <- floor ((high - 1e-12) * n)  # largest k that still works
    if (k_low <= k_high)               # some k fits the band
      return(n)
  }
  NA                                    # none found up to n_max
}

# build the table for every 0.001 increment from 0.000 to 1.000
targets <- seq(0, 1, by = 0.001)
thousandths_table <- data.frame(
  pct            = sprintf("%.3f", targets),        # character column 0.000 … 1.000
  min_denominator = vapply(targets, min_denom, integer(1))
)

head(thousandths_table, 10)   # view first few rows

library(ggplot2)

# thousandths_table already holds:
#   pct             – character column like "0.334"
#   min_denominator – integer column with the smallest n that can round to pct

ggplot(thousandths_table,
       aes(x = as.numeric(pct),          # x-axis: printed %
           y = min_denominator)) +       # y-axis: minimal denominator
  geom_point(colour = "steelblue", size=.5) +      # draw the curve
  #scale_y_log10() +                      # log scale makes tall “spikes” visible
  labs(x = "3-point % (thousandths)",
       y = "Smallest denominator",
       title = "Minimum attempts needed for any given 3-point percentage") +
  theme_minimal()

min_denom(.249)
