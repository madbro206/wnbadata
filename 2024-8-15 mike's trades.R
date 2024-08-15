#load packages
pacman::p_load(wehoop, dplyr, tictoc, ggplot2, camcorder, patchwork, ggmap, tidyr)

#initialize camcorder
gg_record(
  dir = file.path(tempdir(), "recording100"), # where to save the recording
  device = "png", # device to use to save images
  width = 6,      # width of saved image
  height = 4,     # height of saved image
  units = "in",   # units for width and height
  dpi = 300       # dpi to use when saving image
)


#load data
trades <- read.csv("~/Desktop/wehoop/2024-8-15 mike's trades/transactions.csv", header = TRUE)

trades_post_2013 <- subset(trades, year_id >= 2013)

#filter to WAS and to years mike was GM only
mike <- subset(trades, year_id >= 2013 & (from_team=="WAS" | to_team=="WAS"))


############################################################################################################
#count mike trades per year
library(dplyr)

# Step 1: Create a sequence of all years and count unique trades per year
trades_per_year <- mike %>%
  distinct(transaction_id, .keep_all = TRUE) %>%
  count(year_id, name = "num_trades") %>%
  complete(year_id = seq(min(mike$year_id), max(mike$year_id)), fill = list(num_trades = 0))


ggplot(trades_per_year, aes(x = factor(year_id), y = num_trades)) +
  geom_bar(stat = "identity", fill = "#F88158") +
  theme_minimal() +
  labs(title = "Mike Thibault Trades by Year", x = "Year", y = "Number of Trades")
  
#count total trades per year
total_trades_per_year <- trades_post_2013 %>%
  distinct(transaction_id, .keep_all = TRUE) %>%
  count(year_id, name = "num_trades") %>%
  complete(year_id = seq(min(trades_post_2013 $year_id), max(trades_post_2013 $year_id)), fill = list(num_trades = 0))

ggplot(total_trades_per_year, aes(x = factor(year_id), y = num_trades)) +
  geom_bar(stat = "identity", fill = "#74618f") +
  theme_minimal() +
  labs(title = "Total Trades by Year Since 2013", x = "Year", y = "Number of Trades")



##############################################################################################################

# Define the target directory
target_dir <- file.path(tempdir(), "recording")

# Create the directory if it does not exist
if (!dir.exists(target_dir)) {
  dir.create(target_dir, recursive = TRUE)
}

# Proceed with gg_playback
gg_playback(
  name = file.path(target_dir, "chart.gif"),
  first_image_duration = 5,
  last_image_duration = 15,
  frame_duration = .4,
  image_resize = 800
)
