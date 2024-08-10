#Use wehoop package to download WNBA or NCAAW data, and translate into format to use with BasketballAnalyzeR package
#https://wehoop.sportsdataverse.org/articles/getting-started-wehoop.html

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, BasketballAnalyzeR, ggplot2)


#######################################################################################################
#load data from herhoopstats
#https://herhoopstats.com/stats/wnba/league/

# Use textConnection to read directly from text
data_text <- "season	G	MIN	PTS	FGM	FGA	FGp	2PM	2PA	2Pp	3PM	3PA	3Pp	FTM	FTA	FTp	FT Trip	ORB	DRB	TRB	AST	TOV	STL	BLK	PF
2024	148	201.2	81.7	29.8	68.2	43.7%	22.2	45.5	48.7%	7.6	22.7	33.6%	14.5	18.3	78.8%	8.1	8.2	26.1	34.4	20.6	14.5	7.6	4.2	17.6
2023	240	201.0	82.7	30.1	68.4	44.1%	22.5	46.3	48.6%	7.7	22.1	34.7%	14.8	18.5	80.0%	8.1	8.0	26.3	34.4	19.8	14.2	7.1	3.9	17.9
2022	216	201.4	82.3	30.1	68.0	44.2%	22.3	45.6	48.9%	7.7	22.4	34.6%	14.4	18.2	79.4%	8.0	8.2	26.1	34.3	20.2	14.6	7.5	3.7	17.7
2021	192	201.7	80.7	29.7	68.4	43.5%	22.5	47.2	47.5%	7.3	21.2	34.3%	14.0	17.3	80.8%	7.6	8.1	26.7	34.8	18.9	13.9	7.0	4.1	17.4
2020	132	200.8	83.1	30.4	68.2	44.6%	23.1	47.0	49.2%	7.3	21.2	34.5%	14.9	18.4	80.7%	8.1	8.3	25.8	34.1	19.1	14.6	7.8	3.5	17.9
2019	204	200.6	78.7	29.1	68.6	42.4%	22.3	48.5	46.0%	6.8	20.0	33.8%	13.7	17.3	79.6%	7.6	9.0	25.8	34.8	18.8	14.5	7.4	4.2	17.5
2018	203	200.9	82.8	30.6	68.8	44.5%	23.9	49.3	48.5%	6.7	19.5	34.5%	14.8	18.6	79.3%	8.2	8.9	25.6	34.5	19.2	13.5	6.9	3.7	18.2
2017	204	201.9	81.5	29.9	68.0	44.1%	24.0	50.5	47.6%	5.9	17.5	33.8%	15.6	19.6	79.7%	8.6	8.8	25.0	33.9	17.9	13.9	7.0	3.9	18.9
2016	204	203.1	81.9	29.8	67.5	44.1%	24.4	51.4	47.4%	5.4	16.1	33.6%	16.9	21.2	79.8%	9.3	9.1	24.7	33.8	17.6	13.8	7.3	4.0	19.6
2015	204	201.7	75.1	27.8	65.4	42.5%	23.0	50.6	45.4%	4.8	14.8	32.5%	14.8	18.6	79.5%	8.2	8.8	24.8	33.6	16.3	13.7	7.2	4.3	18.5
2014	204	203.1	77.1	29.0	66.3	43.7%	24.4	52.3	46.6%	4.6	14.1	32.9%	14.5	18.6	77.9%	8.2	9.3	24.4	33.7	17.1	14.2	7.6	3.7	18.3
2013	204	201.6	75.6	28.1	66.4	42.3%	23.4	52.0	45.0%	4.7	14.4	32.6%	14.7	18.8	78.2%	8.3	9.8	24.7	34.5	16.0	14.4	7.7	4.1	17.8
2012	204	201.8	77.5	28.6	66.6	42.9%	22.3	48.9	45.7%	6.2	17.7	35.3%	14.1	18.5	76.3%	8.1	10.2	23.8	34.0	16.9	15.7	8.4	3.9	17.8
2011	204	201.6	77.3	28.6	65.7	43.5%	23.0	49.9	46.2%	5.6	15.8	35.3%	14.5	18.8	77.0%	8.3	9.5	23.8	33.3	16.9	15.1	7.9	3.9	18.3
2010	204	202.1	80.3	29.3	66.4	44.1%	23.4	49.6	47.2%	5.9	16.8	34.9%	15.9	20.6	77.5%	9.1	9.9	23.9	33.7	17.5	15.6	8.3	3.7	19.2
2009	221	203.5	78.3	28.3	66.2	42.8%	22.8	50.0	45.5%	5.6	16.2	34.4%	16.1	20.7	77.5%	9.1	10.0	23.9	33.9	16.2	15.0	8.2	3.8	19.9
2008	238	202.1	76.3	27.4	65.1	42.0%	22.1	49.6	44.6%	5.3	15.6	33.8%	16.3	21.6	75.1%	9.5	10.4	23.7	34.1	16.1	15.2	8.1	3.9	20.8
2007	221	200.5	75.9	27.4	65.3	42.0%	21.9	49.5	44.3%	5.5	15.8	34.9%	15.6	20.1	77.6%	8.8	10.3	23.5	33.8	16.3	15.7	8.1	4.3	21.2
2006	221	202.0	76.7	27.6	65.7	42.1%	22.0	49.3	44.6%	5.6	16.5	34.1%	15.9	20.4	77.8%	9.1	10.0	23.4	33.4	16.3	15.0	8.2	3.9	20.4
2005	221	201.5	74.2	26.5	63.5	41.8%	21.8	48.8	44.7%	4.7	14.6	32.0%	16.5	21.2	77.9%	9.3	9.7	22.4	32.0	16.2	15.0	7.9	3.9	20.1
2004	221	202.0	67.0	24.5	58.3	42.0%	20.2	46.1	43.9%	4.3	12.2	34.9%	13.8	18.6	74.1%	8.1	9.6	20.9	30.5	15.3	14.1	7.8	3.7	19.0
2003	238	201.6	68.0	24.9	59.7	41.7%	20.4	46.1	44.1%	4.5	13.5	33.6%	13.7	18.4	74.4%	8.1	10.0	21.4	31.4	15.4	13.9	7.5	3.7	18.9
2002	256	201.7	67.4	24.5	58.4	42.0%	20.1	45.3	44.3%	4.5	13.1	34.0%	13.9	18.7	74.3%	8.3	9.8	20.7	30.4	15.1	14.4	7.8	3.6	19.2
2001	256	202.5	65.6	24.0	58.4	41.1%	19.7	45.5	43.3%	4.3	12.9	33.6%	13.3	17.9	74.0%	7.9	9.9	21.0	30.9	15.1	14.5	8.0	3.8	18.8
2000	256	200.0	68.4	24.9	57.7	43.2%	20.8	45.3	46.0%	4.1	12.5	33.1%	14.4	19.5	74.0%	8.6	9.6	20.0	29.6	15.5	15.1	8.1	3.4	20.3
1999	191	199.4	68.5	24.8	59.0	42.1%	20.6	45.9	45.0%	4.2	13.2	32.1%	14.6	20.0	73.2%	8.8	10.0	20.9	30.8	15.8	15.0	7.7	3.4	20.3
1998	149	201.2	70.3	25.8	60.9	42.3%	22.1	49.1	45.0%	3.7	11.8	31.4%	15.0	20.9	71.7%	9.2	10.3	21.7	32.1	16.0	16.6	8.8	3.5	20.3
1997	112	201.7	69.1	25.4	61.1	41.5%	21.5	49.1	43.9%	3.8	12.0	31.9%	14.6	20.5	71.1%	9.0	11.0	21.5	32.5	15.8	18.0	9.7	3.4	19.4
"

# Read data
df <- read.table(textConnection(data_text), header = TRUE, sep = "\t")

# View the data frame
print(df)


#############################################################################################################
#3pa vs 3pm by season

df_long <- df %>%
  pivot_longer(cols = c("X3PM", "X3PA"), names_to = "Metric", values_to = "Count")


library(ggplot2)

ggplot(df_long, aes(x = season, y = Count, color = Metric, group = Metric)) +
  geom_point() +                      # Add points for X3PM and X3PA
  geom_line() +                       # Connect the points with lines
  geom_text(aes(label = Count), vjust = -1, size = 2, check_overlap = TRUE) + # Add labels to the points with overlap check
  labs(
    title = "WNBA 3PM and 3PA per Game by Season",
    subtitle = "data via herhoopstats.com/stats/wnba/league/",
    x = "Season",
    y = "Count per game",
    color = "Type"
  ) +
  scale_x_continuous(breaks = df$season, labels = scales::label_wrap(10)) + # Rotate x-axis labels if needed
  expand_limits(y = 0) +              # Ensure y-axis starts at 0
  scale_color_manual(
    values = c("X3PM" = "blue", "X3PA" = "red"),     # Custom colors (optional)
    labels = c("3s made", "3s attempted") # Custom legend labels
  ) +
  theme_minimal() +                   # Apply a clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability
    legend.position = "bottom"          # Move legend to the bottom for better spacing
  )


#############################################################################################################
#3pa vs 2pa by season


df_long <- df %>%
  pivot_longer(cols = c("X2PA", "X3PA"), names_to = "Metric", values_to = "Count")


library(ggplot2)

ggplot(df_long, aes(x = season, y = Count, color = Metric, group = Metric)) +
  geom_point() +                      # Add points for X2PA and X3PA
  geom_line() +                       # Connect the points with lines
  geom_text(aes(label = Count), vjust = -1, size = 2, check_overlap = TRUE) + # Add labels to the points with overlap check
  labs(
    title = "WNBA 2PA and 3PA per Game by Season",
    subtitle = "data via herhoopstats.com/stats/wnba/league/",
    x = "Season",
    y = "Count per game",
    color = "Type"
  ) +
  scale_x_continuous(breaks = df$season, labels = scales::label_wrap(10)) + # Rotate x-axis labels if needed
  expand_limits(y = 0) +              # Ensure y-axis starts at 0
  scale_color_manual(
    values = c("X2PA" = "blue", "X3PA" = "red"),     # Custom colors (optional)
    labels = c("2s attempted", "3s attempted") # Custom legend labels
  ) +
  theme_minimal() +                   # Apply a clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability
    legend.position = "bottom"          # Move legend to the bottom for better spacing
  )




