## Load Libraries and Packages ----
#| echo: false
library(tidyverse)
library(knitr)

## Load NFL_data ----
#| echo: false
nfl_data <- read.csv("data/team_stats_2003_2023.csv")

### Distribution of Different Variables in the Dataset ----
# creating a function to make histograms for univariate analysis
create_histogram <- function(data, x, binwidth = NULL, fill = "pink", color = "black") {
  ggplot(data, aes(x = {{x}})) + 
    geom_histogram(binwidth = binwidth, fill = fill, color = color) +
    theme_minimal() +
    labs(
      title = paste("Histogram of", deparse(substitute(x))),
      x = deparse(substitute(x)),
      y = "Frequency"
    )
}



##### Distribution of Wins and Losses from 2003-2023 ----
create_histogram(nfl_data_clean, wins, 1)
# peak in center around 7-8 wins

create_histogram(nfl_data_clean, losses, 1)
# peak in the center around 8-9 losses

##### Distribution of Points Scored, Allowed, and Differential ----
create_histogram(nfl_data_clean, points_allowed, 10)
# peak in the center at around 350 points

create_histogram(nfl_data_clean, points_scored, 10)
# slight right skew with a peak at around 380 points

create_histogram(nfl_data_clean, points_diff, 10)
# less unified than the others (has many peaks) - highs at -80, -25, 25, 80, 110



##### Distribution of Offensive Variables ----
create_histogram(nfl_data_clean, total_yards, binwidth = 100)
# peak in the center at 5500 yards - high of over 7000 and low of less than 1000

create_histogram(nfl_data_clean, plays_offense, binwidth = 10)
# peak in center at around 1020 plays and 1080 plays

create_histogram(nfl_data_clean, yds_per_play_offense, binwidth = 0.1)
# peak in center around 5.2-5.4 yards per play - another peak at 5.9 yards

create_histogram(nfl_data_clean, turnovers, binwidth = 1.5)
# right skew - peaks at around 20, 24, 26, 28 - most teams have between 18-30 turnovers

create_histogram(nfl_data_clean, fumbles, binwidth = 1)
# right skew - peak at 8-12 fumbles per season - high of over 25 low of around 1-2

create_histogram(nfl_data_clean, first_down, binwidth = 10)
# slight left skew - peak between 300-350 first downs

##### Distribution of Passing Variables ----
create_histogram(nfl_data_clean, pass_completion, binwidth = 10)
# peak in the middle  at around 350 pass completions

create_histogram(nfl_data_clean, pass_att, binwidth = 15)
# peak in the middle at around 550 pass attempts with a slight left skew in the data

create_histogram(nfl_data_clean, pass_yds, binwidth = 120)
# unimodal distribution with a peak from 3200-4200 yards - large drop off outside this range

create_histogram(nfl_data_clean, pass_td, binwidth = 2)
# right skew - peak at 20 passing touchdowns - high of over 50 and low of 5

create_histogram(nfl_data_clean, pass_net_yds_per_att, binwidth = 0.2)
# slight left skew - peak at 5.8-6.0 yards per attempt

create_histogram(nfl_data_clean, pass_first_down, binwidth = 7)
# unimodal distribution - peak at 180 first down passes

##### Distribution of Rushing Variables ----
create_histogram(nfl_data_clean, rush_att, binwidth = 15)
# unimodal distribution with a peak at 400 rush attempts - most rushing attempts are  between 400-450

create_histogram(nfl_data_clean, rush_yds, binwidth = 100)
# right skew with a peak at 1600 rushing yards 

create_histogram(nfl_data_clean, rush_td, binwidth = 1)
# right skew with a peak at around 14 rushing touchdowns

create_histogram(nfl_data_clean, rush_yds_per_att, binwidth = 0.1)
# unimodal with a peak at 4.0 and 4.3 yards per rushing attempt

create_histogram(nfl_data_clean, rush_fd, binwidth = 7)
# right skew with a peak at around 80 rushing first downs - all teams have above 50 and only a few obs above 150

##### Distribution of Penalty Variables ----
create_histogram(nfl_data_clean, penalties, binwidth = 7)
# unimodal with peak between 90-110 penalties 

create_histogram(nfl_data_clean, penalties_yds, binwidth = 40)
# unimodal with peaks at around 880 yards and 650 yards - high of over 1400

create_histogram(nfl_data_clean, pen_fd, binwidth = 2)
# slight right skew with a peak at 24 penalty first downs - high of 50 and low of 10
##
##### Distribution of Score and Turnover Percentage ----
create_histogram(nfl_data_clean, score_pct, binwidth = 2)
#unimodal with a peak around 33% - low of 10% and high of 55% - most teams score percentage is relatively low

create_histogram(nfl_data_clean, turnover_pct, binwidth = 1)
# slight right skew with a peak at 12% turnover rate, which is actually surprisingly high
                 