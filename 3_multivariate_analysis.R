## Load Libraries and Packages ----
#| echo: false
library(tidyverse)
library(knitr)

## Load NFL_data ----
#| echo: false
nfl_data <- read.csv("data/team_stats_2003_2023.csv")

### General Trends in the NFL ----
##### How have average points scored per season changed over time? ----
#| echo: false
#| fig-cap: "**Fig 1.** Average number of points scored in the NFL from 2003 to 2023. The highest average number of points scored was in 2020 with around 396 points per team, and the lowest number of points was 2005 with only around 330 points per team. There is year to year variation, but overall the data shows a positive trend with an increase in points scored each year."
#how have points changed over time for the entire league
nfl_data_clean |>
  summarise(average_points = mean(points_scored),
            .by = year) |>
  ggplot(aes(x = year, y = average_points)) +
  geom_point() +
  geom_line() + 
  labs(title = "Average Points Scored in the NFL over Time",
       subtitle = "Points scored by NFL teams have generally increased",
       x = "Year",
       y = "Average Points Scored")


##### How have average points scored per season changed over time by conference? ----
#| echo: false
#| fig-cap: "**Fig 2.** Average number of points scored in the AFC from 2003 to 2023. The highest average number of points scored was in 2020 with around 396 points per team, and the lowest number of points was 2006 with only around 330 points per team. There is severe fluctuation within the data year to year that makes it hard to discern a trend."
#how have points changed over time for the AFC
nfl_data_clean |>
  filter(conference == "AFC") |>
  summarise(average_points = mean(points_scored),
            .by = year) |>
  ggplot(aes(x = year, y = average_points)) +
  geom_point() +
  geom_line() + 
  labs(title = "Average Points Scored in the AFC over Time",
       subtitle = "Points scored by AFC teams has significant year to year fluctuations with no clear trend",
       x = "Year",
       y = "Average Points Scored")

#| echo: false
#| fig-cap: "**Fig 3.** Average number of points scored in the NFC from 2003 to 2023. The highest average number of points scored was in 2020 with around 400 points per team, and the lowest number of points was 2004 with only around 322 points per team. There appears to be an increase in the average points scored per team in the NFC over time."
#how have points changed over time for the NFC
nfl_data_clean |>
  filter(conference == "NFC") |>
  summarise(average_points = mean(points_scored),
            .by = year) |>
  ggplot(aes(x = year, y = average_points)) +
  geom_point() +
  geom_line() + 
  labs(title = "Average Points Scored in the NFC over Time",
       subtitle = "Points scored by NFC teams have generally increased",
       x = "Year",
       y = "Average Points Scored")

##### How have average points scored per season changed over time by division? ----
#| echo: false
#| fig-cap: "**Fig 4.** Average number of points scored in the AFC divisions from 2003 to 2023. There is significant variation from year to year within each division, and they appear to have different trends from each other. Overall, there does not seem to be a significant increase in points from 2003-2023 in any division in the AFC."
nfl_data_clean |>
  filter(conference == "AFC") |>
  group_by(year, division) |>
  summarise(average_points = mean(points_scored, na.rm = TRUE)) |>
  ggplot(aes(x = year, y = average_points, color = division)) +
  geom_line() + 
  geom_point() +
  scale_color_manual(
    values = c(
      "East" = "blue",    
      "North" = "red",    
      "South" = "green",  
      "West" = "purple"     
    )
  ) +
  labs(
    title = "Average Points Scored in each Division (AFC Conference)",
    subtitle = "Each division has significant fluctuation by season and appear to have different trends",
    x = "Year",
    y = "Average Points Scored",
    color = "Division"
  ) +
  theme_minimal()


#| echo: false
#| fig-cap: "**Fig 5.** Average number of points scored in the NFC divisions from 2003 to 2023. There is significant variation from year to year within each division, but they appear to have similar trends. Overall, there seems to be an increase in points from 2003-2023 in every division in the NFC."
nfl_data_clean |>
  filter(conference == "NFC") |>
  group_by(year, division) |>
  summarise(average_points = mean(points_scored, na.rm = TRUE)) |>
  ggplot(aes(x = year, y = average_points, color = division)) +
  geom_line() + 
  geom_point() +
  scale_color_manual(
    values = c(
      "East" = "blue",    
      "North" = "red",    
      "South" = "green",  
      "West" = "purple"     
    )
  ) +
  labs(
    title = "Average Points Scored in each Division (NFC Conference)",
    subtitle = "Although there is fluctuation between divisions, the NFC appears to increase in average points",
    x = "Year",
    y = "Average Points Scored",
    color = "Division"
  ) +
  theme_minimal()



### Performance of Specific Teams ----
##### Which team has the most wins in the dataset? ----

#| echo: false
#| fig-cap: "**Table 1.** Total wins for each team in the NFL over 20 years. The teams with the most wins are the New England Patriots, the Pittsburgh Steelers, and the Green Bay Packers."
nfl_data_clean |>
  group_by(team) |>
  summarize(total_wins = sum(wins)) |>
  arrange(desc(total_wins)) |>
  kable(col.names = c("Team", "Total Wins"))

##### Which team has the best win-to-loss ratio over the years? ----
#| echo: false
#| fig-cap: "**Table 2.** Win/loss ratio for each team in the NFL over 20 years. The teams with the best win/loss ratio are the New England Patriots, the Pittsburgh Steelers, and the Green Bay Packers."
nfl_data_clean |>
  group_by(team) |>
  summarise(
    total_wins = sum(wins, na.rm = TRUE),
    total_losses = sum(losses, na.rm = TRUE),
    win_loss_ratio = total_wins / total_losses
  ) |>
  arrange(desc(win_loss_ratio)) |>
  kable(col.names = c("Team", "Total Wins", "Total Losses", "Win/Loss Ratio"))




### Season-Level Trends ----
##### What were the highest-scoring seasons for individual teams? ----
#| echo: false
#| fig-cap: "**Table 3.** The best season for each team in the NFL in the last 20 years. The team with the most wins in their best season is the New England Patriots with 16 wins in 2007."
nfl_data_clean |>
  group_by(team, year) |>
  summarize(season_wins = sum(wins, na.rm = TRUE), .groups = "drop") |>
  group_by(team) |>
  slice_max(season_wins, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(desc(season_wins)) |>
  kable(col.names = c("Team", "Year", "Season Wins"))

##### Are certain years dominated by a few teams, or is performance evenly distributed? ----

# summarize total wins per year per team
nfl_data_clean |>
  group_by(year, team) |>
  summarise(total_wins = sum(wins), .groups = "drop") |>
  group_by(year) |>
  summarise(
    max_wins = max(total_wins),  
    avg_wins = mean(total_wins),  
    sd_wins = sd(total_wins)
  ) |>
  
  
  # visualize standard deviation of wins by year
  ggplot(win_distribution, aes(x = year, y = sd_wins)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Yearly Standard Deviation of Wins in the NFL",
    subtitle = "Higher standard deviations indicate greater dominance by a few teams",
    x = "Year",
    y = "Standard Deviation of Wins"
  ) +
  theme_minimal()

# most dominant team in each year
nfl_data_clean |>
  group_by(year, team) |>
  summarise(total_wins = sum(wins), .groups = "drop") |>
  group_by(year) |>
  slice_max(total_wins, n = 1) |>
  arrange(desc(year))


##


### Determining the Correlation Between Variables in the Dataset ----

#create a function for correlation graphs (generative AI was used to help me construct this function)
create_correlation_graph <- function(data, target_var) {
  # select numeric columns in the dataset
  numeric_data <- data |>
    select(where(is.numeric))
  
  # calculate correlations with the target variable
  correlation_df <- numeric_data |>
    summarise(across(everything(), ~ cor(.x, numeric_data[[target_var]], use = "complete.obs"))) |>
    pivot_longer(cols = everything(), names_to = "variable", values_to = "correlation") |>
    filter(variable != target_var)  # exclude the target variable itself
  
  # create the ggplot object
  plot <- ggplot(correlation_df, aes(x = reorder(variable, correlation), y = correlation, fill = correlation)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    theme_minimal() +
    labs(
      title = paste("Correlation with", target_var),
      x = "Variables",
      y = "Correlation Coefficient"
    )
  
  # return the plot object
  return(plot)
}

create_correlation_graph(nfl_data_clean, "wins")

# the variables that have the best indicator of if a team will win are win_loss_perc,
# mov, and points diff - this all makes sense bc it is stats on winning
# points, score_pct, and pass_net_yds_per_att are the next most
# surprisingly, rush_yds, pass_yds, and plays offense have very little to do with winning
# variables that have the worst indicator are losses and points_opp
# fumbles, pass_int, penalties, and turnovers are also indicators that a team will lose 


#create correlation graphs for touchdowns to see if a variable influences touchdowns the most
create_correlation_graph(nfl_data_clean, "pass_td")

# pass_yds, total_yds, pass_first_down, pass_net_yds_per_att, yds_per_play_offense
# are the biggest indicator of pass_td - these all make sense 
#rush_yds interestingly are inversely correlated to passing touchdowns, which I 
# guess makes sense, but I would think that if you rush more yards, you are 
# closer to the end zone and can pass more accurately? Rush_td are also weakly
# correlated - so why are rush_td correlated, but rush_yds are not?

# the following code was made from generative AI - I thought it was a cool way to explore the data
# Select only numeric columns from the dataframe
numerical <- nfl_data_clean |> select(where(is.numeric))

# Create a correlation matrix for the numeric columns
correlation_matrix <- cor(numerical, use = "complete.obs")

# Plot the heatmap
library(ggplot2)
library(reshape2)

# Melt the correlation matrix for ggplot2
melted_corr <- melt(correlation_matrix)

# Plot the heatmap
ggplot(data = melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0, name = "Correlation") +
  labs(
    title = "Heatmap of Numeric Column Correlations",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Research Questions for Final Project ----
##### How does points allowed/points scored correlate with wins? ----

#one way of seeing data - blue and red lines graphed on the same plot
ggplot(nfl_data_clean, aes(x = wins)) +
  geom_jitter(aes(y = points_scored), color = "blue", alpha = 0.7) +
  geom_jitter(aes(y = points_allowed), color = "red", alpha = 0.7) +
  labs(title = "Wins vs Points Scored/Allowed", x = "Wins", y = "Points") +
  theme_minimal()

#separate plots - this one for points_scored
ggplot(nfl_data_clean, aes(x = wins)) +
  geom_jitter(aes(y = points_scored), 
              width = 0.4, alpha = 0.7, size = 2, color = "black") +
  geom_smooth(aes(y = points_scored), method = "lm", color = "blue", se = FALSE) +
  labs(
    title = "Wins vs Points Scored",
    x = "Wins",
    y = "Points Scored"
  ) +
  theme_minimal()

#separate plots - this one for points_allowed
ggplot(nfl_data_clean, aes(x = wins)) +
  geom_jitter(aes(y = points_allowed), 
              width = 0.4, alpha = 0.7, size = 2, color = "black") +
  geom_smooth(aes(y = points_allowed), method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Wins vs Points Scored",
    x = "Wins",
    y = "Points Scored"
  ) +
  theme_minimal()


# shows one set of points, but with win percentage marked by the color of the points
ggplot(nfl_data_clean, aes(x = points_scored, y = points_allowed)) +
  geom_point(aes(color = wins / games_played), alpha = 0.8) +
  scale_color_gradient(low = "red", high = "green", name = "Win %") +
  labs(
    title = "Win % by Points Scored and Allowed",
    x = "Points Scored",
    y = "Points Allowed"
  ) +
  theme_minimal()



##### Are rushing touchdowns more or less important than passing touchdowns? ----
# calculate correlations and plot bar chart
nfl_data_clean |> 
  summarise(
    rushing_touchdowns = cor(wins, rush_td, use = "complete.obs"),
    passing_touchdowns = cor(wins, pass_td, use = "complete.obs")
  ) |> 
  pivot_longer(cols = everything(), names_to = "Type of Touchdown", values_to = "Correlation") |> 
  ggplot(aes(x = `Type of Touchdown`, y = Correlation, fill = `Type of Touchdown`)) + 
  geom_bar(stat = "identity", width = 0.8) + 
  scale_fill_manual(values = c("rushing_touchdowns" = "blue", "passing_touchdowns" = "red")) + 
  labs(
    title = "Correlation of Touchdowns with Wins",
    subtitle = "Passing Touchdowns are a slightly higher indicator of total season wins\nbut both are important metrics to consider",
    x = "Type of Touchdown",
    y = "Correlation"
  ) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# they have around the same correlation - not a noticeable difference

##### Is offense or defense more important to winning? ----

#this is essentially the same question about points scored/allowed, but shown differently
nfl_data_clean |> 
  summarise(
    offensive_cor = mean(c(
      cor(wins, points_scored, use = "complete.obs")
    ), na.rm = TRUE),
    
    defensive_cor = mean(c(
      cor(wins, points_allowed, use = "complete.obs")
    ), na.rm = TRUE)
  ) |> 
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Correlation") |> 
  ggplot(aes(x = Category, y = Correlation, fill = Category)) + 
  geom_bar(stat = "identity", width = 0.8) + 
  scale_fill_manual(values = c("offensive_cor" = "blue", "defensive_cor" = "red")) + 
  labs(
    title = "Comparison of Offense vs Defense Correlation with Wins",
    x = "Category",
    y = "Correlation"
  ) + 
  theme_minimal()

# teams that have the best offense
nfl_data_clean |>
  summarize(total_points_scored = sum(points_scored),
            .by = team) |>
  arrange(desc(total_points_scored))|>
  head(3) |>
  kable(col.names = c("Team", "Total Points Scored"),
        caption = "Teams With the Best Offense in the NFL")

# teams that have the best defense
nfl_data_clean |>
  summarize(total_points_allowed = sum(points_allowed),
            .by = team) |>
  arrange(total_points_allowed) |>
  head(3) |>
  kable(col.names = c("Team", "Total Points Allowed"),
              caption = "Teams With the Best Defense in the NFL")

# best teams in the NFL
nfl_data_clean |>
  summarize(total_wins = sum(wins),
            .by = team) |>
  arrange(desc(total_wins)) |>
  head(3) |>
  kable(col.names = c("Team", "Total Wins"),
        caption = "Best Football Teams in the NFL")
  

# View the top teams with the most points scored
head(teams_most_points)  # This will display the top teams with the most points scored





##### Are there any teams that consistently underperform or overperform? ----
# example using points scored and points allowed as predictors for expected wins (generative AI used to help)
model <- lm(wins ~ points_scored + points_allowed, data = nfl_data_clean)
nfl_data_clean$expected_wins <- predict(model, newdata = nfl_data_clean)
nfl_data_clean$win_difference <- nfl_data_clean$wins - nfl_data_clean$expected_wins

nfl_data_clean |> 
  ggplot(aes(x = team, y = win_difference, fill = win_difference)) + 
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_gradient2(low = "red", high = "green", midpoint = 0) +
  labs(title = "Teams Overperforming or Underperforming",
       x = "Team",
       y = "Difference in Wins (Actual - Expected)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# visualize team outliers using a boxplot
model <- lm(wins ~ points_scored + points_allowed, data = nfl_data_clean)
nfl_data_clean$expected_wins <- predict(model, newdata = nfl_data_clean)
nfl_data_clean$win_difference <- nfl_data_clean$wins - nfl_data_clean$expected_wins

# calculate IQR for win_difference
Q1 <- quantile(nfl_data_clean$win_difference, 0.25)
Q3 <- quantile(nfl_data_clean$win_difference, 0.75)
IQR <- Q3 - Q1

# define outliers (values outside of Q1 - 1.3*IQR and Q3 + 1.3*IQR)
outliers <- nfl_data_clean |> 
  filter(win_difference < (Q1 - 1.3 * IQR) | win_difference > (Q3 + 1.3 * IQR))

# create boxplot
ggplot(nfl_data_clean, aes(x = "", y = win_difference)) + 
  geom_boxplot(fill = "pink", color = "black") + 
  geom_point(data = outliers, aes(x = "", y = win_difference, color = team), size = 3) +
  labs(title = "Boxplot of Win Differences with Outliers Highlighted", 
       y = "Win Difference",
       color = "Team") +
  theme_minimal()


model <- lm(wins ~ points_scored + points_allowed, data = nfl_data_clean)
nfl_data_clean$expected_wins <- predict(model, newdata = nfl_data_clean)
nfl_data_clean$win_difference <- nfl_data_clean$wins - nfl_data_clean$expected_wins

ggplot(nfl_data_clean, aes(x = team, y = 1, fill = win_difference)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "green", midpoint = 0) +
  labs(title = "Teams Overperforming or Underperforming",
       x = "Team", y = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),  
    axis.text.y = element_blank() 
  )









##### Are there any teams who receive more penalties? Does that correlate with wins? ----
# create barplot that shows teams with most penalties
library(scales) 
# Generating a pastel rainbow palette using AI
pastel_rainbow <- hue_pal(h = c(0, 360), c = 70, l = 80)(length(unique(nfl_data_clean$team)))

ggplot(nfl_data_clean, aes(x = team, y = penalties, fill = team)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = pastel_rainbow) +
  labs(
    title = "Frequency of Penalties per Team",
    x = "Team",
    y = "Penalties"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none" # Removes the legend
  )

nfl_data_clean |> 
  filter(team %in% c("Las Vegas Raiders", "Arizona Cardinals", "Baltimore Ravens", 
                     "Indianapolis Colts", "Atlanta Falcons", "New England Patriots")) |> 
  ggplot(aes(x = penalties, y = wins, color = team)) + 
  geom_point(alpha = 0.9) + 
  facet_wrap(~team) +  # Create a separate plot for each team
  labs(title = "Penalties vs Wins for Selected NFL Teams",
       x = "Penalties",
       y = "Wins") + 
  theme_minimal() + 
  theme(legend.position = "none")

nfl_data_clean |>
  filter(team == c("Las Vegas Raiders", "Arizona Cardinals", "Baltimore Ravens",
                   "Indianapolis Colts", "Atlanta Falcons", "New England Patriots")) |>
  summarize(average_penalties = mean(penalties),
            average_wins = mean(wins),
            .by = team) |>
  kable()


# Calculate IQR for penalties

Q1 <- quantile(nfl_data_clean$penalties, 0.25, na.rm = TRUE)
Q3 <- quantile(nfl_data_clean$penalties, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# define outliers (values outside of Q1 - 1.7*IQR and Q3 + 1.7*IQR)
outliers_penalties <- nfl_data_clean |> 
  filter(penalties < (Q1 - 1.7 * IQR) | penalties > (Q3 + 1.7 * IQR))

# create the boxplot with outliers highlighted for penalties
Q1_penalties <- quantile(nfl_data_clean$penalties, 0.25)
Q3_penalties <- quantile(nfl_data_clean$penalties, 0.75, na.rm = TRUE)
IQR_penalties <- Q3_penalties - Q1_penalties

# Define outliers (values outside of Q1 - 1.6*IQR and Q3 + 1.6*IQR)
outliers_penalties <- nfl_data_clean |> 
  filter(penalties < (Q1_penalties - 1.6 * IQR_penalties) | penalties > (Q3_penalties + 1.6 * IQR_penalties))

# create the boxplot with outliers highlighted
ggplot(nfl_data_clean, aes(x = "", y = penalties)) + 
  geom_boxplot(fill = "pink", color = "black", outlier.shape = NA) +  # Suppress default outliers
  geom_point(data = outliers_penalties, aes(x = "", y = penalties, color = team), size = 3) +
  labs(
    title = "Boxplot of Penalties with Outliers Highlighted",
    subtitle = "Outliers show teams with exceptionally high or low penalties compared to others",
    y = "Penalties",
    x = " ",
    color = "Team"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )




#barplot of penalties by each team
#make rainbow palette
pastel_rainbow <- hue_pal(h = c(0, 360), c = 70, l = 80)(length(unique(nfl_data_clean$team)))

#plot barchart of penalties
ggplot(nfl_data_clean, aes(x = team, y = penalties, fill = team)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = pastel_rainbow) +
  labs(
    title = "Frequency of Penalties per Team",
    x = "Team",
    y = "Penalties"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none" # Removes the legend
  )

#scatterplot of penalties for highest and lowest penalty teams
nfl_data_clean |> 
  filter(team == c("Las Vegas Raiders", "Arizona Cardinals", "Baltimore Ravens", 
                     "Indianapolis Colts", "Atlanta Falcons", "New England Patriots")) |> 
  ggplot(aes(x = penalties, y = wins, color = team)) + 
  geom_point(alpha = 0.9) + 
  facet_wrap(~team) +  # Create a separate plot for each team
  labs(title = "Penalties vs Wins for Selected NFL Teams",
       x = "Penalties",
       y = "Wins") + 
  theme_minimal() + 
  theme(legend.position = "none")

# summary table of penalties 
nfl_data_clean |>
  filter(team == c("Las Vegas Raiders", "Arizona Cardinals", "Baltimore Ravens",
                   "Indianapolis Colts", "Atlanta Falcons", "New England Patriots")) |>
  summarize(average_penalties = mean(penalties),
            average_wins = mean(wins),
            .by = team) |>
  kable()


##### Which divisions have the highest average wins per season? ----

nfl_data_clean |> 
  group_by(conference, division) |> 
  summarize(avg_wins = mean(wins)) |> 
  ggplot(aes(x = division, y = avg_wins)) + 
  geom_col(fill = "pink", color = "black") +
  labs(
    title = "Average Wins per Division",
    x = "Division",
    y = "Average Wins"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.title = element_text(hjust = 0.5)
  ) + 
  facet_wrap(~conference)  

##### Is it harder to make the playoffs in certain divisions? ----
# summarize average wins for playoff teams by division

nfl_data_clean |> 
  filter(playoff == "Yes") |>  
  group_by(conference, division) |> 
  summarize(avg_wins_playoff = mean(wins, na.rm = TRUE)) |>
  kable()

# make a bar chart
nfl_data_clean |> 
  filter(playoff == "Yes") |>  
  group_by(conference, division) |> 
  summarize(avg_wins_playoff = mean(wins, na.rm = TRUE)) |>  
  ggplot(aes(x = division, y = avg_wins_playoff)) + 
  geom_col(fill = "pink", color = "black") +  
  labs(title = "Average Wins for Playoff Teams by Division",
       x = "Division", y = "Average Wins for Playoff Teams") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ conference)

# boxplot of median wins per division
nfl_data_clean |> 
  group_by(conference, division) |> 
  ggplot(aes(x = division, y = wins)) + 
  geom_boxplot(fill = "pink", color = "black") +
  labs(title = "Wins Distribution by Division",
       x = "Division", y = "Wins") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~conference)
##### How is superbowl performance correlated to division wins? ----

#table of superbowl appearances by division
nfl_data_clean |> 
  filter(superbowl == "Yes") |>  
  group_by(conference, division) |> 
  summarize(count = n()) |>
  kable()

# bar chart of superbowl appearances by division
nfl_data_clean |> 
  filter(superbowl == "Yes") |> 
  group_by(conference, division) |> 
  summarize(count = n()) |> 
  ggplot(aes(x = division, y = count, fill = division)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Super Bowl Appearances by Division",
       subtitle = "The AFC East has the highest number of superbowl appearances\nwith 6 teams in the past 20 years",
       x = "Division", 
       y = "Number of Teams in Super Bowl",
       fill = "Division") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) + 
  facet_wrap(~conference)

# boxplot to show distribution of wins in each division for superbowl winning teams
nfl_data_clean |> 
  filter(superbowl == "Yes") |> 
  ggplot(aes(x = division, y = wins, fill = division)) + 
  geom_boxplot() + 
  labs(title = "Distribution of Wins for Super Bowl Teams by Division", 
       subtitle = "The NFC North has the highest number of wins for a\nSuperbowl winning team at 15 wins in a season",
       x = "Division", 
       y = "Wins") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) + 
  facet_wrap(~conference)




