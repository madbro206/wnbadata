#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package

#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR)

#load data

#wnba full play by play
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp()
})
tictoc::toc()
#######################################################################################################
c <- subset(wnba_pbp, type_text=="Coach's Challenge (Stands)"| type_text=="Coach's Challenge (Supported)"| type_text=="Coach's Challenge (Overturned)" | type_text=="Coach's Challenge (replaycenter)")



##############################################################################################################
# Load necessary libraries
library(ggplot2)
library(dplyr)

#fake
c <- c %>% filter(type_text != "Coach's Challenge (replaycenter)")

# Combine "Supported" and "Stands"
c$type_text <- gsub("Coach's Challenge \\(Stands\\)", "Stands", c$type_text)
c$type_text <- gsub("Coach's Challenge \\(Supported\\)", "Stands", c$type_text)
c$type_text <- gsub("Coach's Challenge \\(Overturned\\)", "Overturned", c$type_text)

# Extract team names using a regular expression
c$team <- sub(".*\\[(.*?)\\].*", "\\1", c$text)

# Summarize the data by team and challenge result
challenge_summary <- c %>%
  group_by(team, type_text) %>%
  summarize(count = n(), .groups = 'drop')


##############################################################################################################
# Load necessary libraries
library(tidyr)
library(dplyr)

# Assume challenge_summary data frame is already defined

# Pivot the data to have separate columns for "Overturned" and "Stands"
challenge_summary <- challenge_summary %>%
  pivot_wider(names_from = type_text, values_from = count, values_fill = 0)

# Rename the columns
colnames(challenge_summary) <- c("team", "Overturned", "Stands")

# Reorder columns
challenge_summary <- challenge_summary[, c("team", "Overturned", "Stands")]

#calculate success rate
challenge_summary$Success_Rate <- challenge_summary$Overturned/(challenge_summary$Overturned+challenge_summary$Stands)*100

#arrange by success rate
challenge_summary <- challenge_summary%>%
	arrange(desc(Success_Rate))

# Print the modified summary
print(challenge_summary)
