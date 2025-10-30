
## Figures for Research Question One (Analysis of the `Wins` Variable)----
### How does points allowed/points scored correlate with wins? ----

# correlation of points_scored to wins
ggplot(nfl_data_clean, aes(x = wins)) +
  geom_jitter(aes(y = points_scored), 
              width = 0.4, alpha = 0.7, size = 2, color = "black") +
  geom_smooth(aes(y = points_scored), method = "lm", color = "blue", se = FALSE) +
  labs(
    title = "Correlation of Average Points Scored with Wins per Season",
    subtitle = "There is a positive correlation between points scored and total wins",
    x = "Wins",
    y = "Points Scored"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),       
    plot.subtitle = element_text(hjust = 0.5)   
  )

# correlation of points_allowed to wins
ggplot(nfl_data_clean, aes(x = wins)) +
  geom_jitter(aes(y = points_allowed), 
              width = 0.4, alpha = 0.7, size = 2, color = "black") +
  geom_smooth(aes(y = points_allowed), method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Correlation of Average Points Allowed with Wins per Season",
    subtitle = "There is a negative correlation between points allowed and total wins",
    x = "Wins",
    y = "Points Allowed"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),       
    plot.subtitle = element_text(hjust = 0.5)   
  )

#points allowed and points scored plotted on the same scatterplot for direct comparison
ggplot(nfl_data_clean, aes(x = wins)) + 
  geom_jitter(aes(y = points_scored, color = "Points Scored"), alpha = 0.7) + 
  geom_jitter(aes(y = points_allowed, color = "Points Allowed"), alpha = 0.7) + 
  scale_color_manual(values = c("Points Scored" = "blue", "Points Allowed" = "red")) + 
  geom_smooth(aes(y = points_scored, color = "Points Scored"), method = "lm", se = FALSE, linetype = "solid") + 
  geom_smooth(aes(y = points_allowed, color = "Points Allowed"), method = "lm", se = FALSE, linetype = "solid") + 
  labs(
    title = "Correlation of Points Allowed and Points Scored with Wins", 
    subtitle = "Points scored has a positive relationship with total wins,\nwhile points allowed has a negative relationship with total wins",
    x = "Wins", 
    y = "Points Scored or Allowed",
    color = "Metric"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5),        
    plot.subtitle = element_text(hjust = 0.5)
  )


### Are rushing touchdowns more or less important than passing touchdowns? ----
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
    subtitle = "Passing touchdowns are a slightly higher indicator of total wins",
    x = "Type of Touchdown",
    y = "Correlation"
  ) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),       
    plot.subtitle = element_text(hjust = 0.5)   
  )

nfl_data_clean |>
  summarize(total_passing_touchdowns = sum(pass_td),
            .by = team) |>
  arrange(desc(total_passing_touchdowns))|>
  head(3) |>
  kable(col.names = c("Team", "Total Passing Touchdowns"),
        caption = "Teams With the Most Passing Touchdowns in the NFL")

### Is offense or defense more important to winning? ----
nfl_data_clean |> 
  summarise(
    offensive_correlation = mean(c(
      cor(wins, points_scored, use = "complete.obs")
    ), na.rm = TRUE),
    
    defensive_correlation = mean(c(
      cor(wins, points_allowed, use = "complete.obs")
    ), na.rm = TRUE)
  ) |> 
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Correlation") |> 
  ggplot(aes(x = Category, y = Correlation, fill = Category)) + 
  geom_bar(stat = "identity", width = 0.8) + 
  scale_fill_manual(values = c("offensive_correlation" = "blue", "defensive_correlation" = "red")) + 
  labs(
    title = "Comparison of Offense vs Defense Correlation with Wins",
    subtitle = "Offense has a higher correlation with total wins per season",
    x = "Category",
    y = "Correlation"
  ) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),       
    plot.subtitle = element_text(hjust = 0.5)   
  )

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



## Figures for Research Question Two (Analysis of Outliers in the Dataset) ----
### Are there any teams that consistently underperform or overperform? ----
# example using points scored and points allowed as predictors for expected wins (generative AI used to help)
model <- lm(wins ~ points_scored + points_allowed, data = nfl_data_clean)
nfl_data_clean$expected_wins <- predict(model, newdata = nfl_data_clean)
nfl_data_clean$win_difference <- nfl_data_clean$wins - nfl_data_clean$expected_wins

nfl_data_clean |> 
  ggplot(aes(x = team, y = win_difference, fill = win_difference)) + 
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_gradient2(low = "red", high = "green", midpoint = 0) +
  labs(title = "Teams Overperforming or Underperforming",
       subtitle = "The Los Angeles Chargers seem to underperform the most on average\nwhile the Indianapolis Colts Overperform the most",
       x = "Team",
       y = "Win Difference",
       fill = "Win Difference") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  theme(
    plot.title = element_text(hjust = 0.5),       
    plot.subtitle = element_text(hjust = 0.5)   
  )

# same plot as above displayed in a heatmap instead - better readability
ggplot(nfl_data_clean, aes(x = team, y = 1, fill = win_difference)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "green", midpoint = 0) +
  labs(title = "Teams Overperforming or Underperforming",
       subtitle = "The Los Angeles Chargers seem to underperform the most on average\nwhile the Colts and Steelers overperform the most",
       x = "Team",
       y = " ",
       fill = "Win Difference") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),  
    axis.text.y = element_blank()) +
  theme(
    plot.title = element_text(hjust = 0.5),       
    plot.subtitle = element_text(hjust = 0.5)   
  )


# creating a boxplot that explicity shows outliers in relation to average win difference
model <- lm(wins ~ points_scored + points_allowed, data = nfl_data_clean)
nfl_data_clean$expected_wins <- predict(model, newdata = nfl_data_clean)
nfl_data_clean$win_difference <- nfl_data_clean$wins - nfl_data_clean$expected_wins

# calculate IQR for win_difference
Q1_win_difference <- quantile(nfl_data_clean$win_difference, 0.25)
Q3_win_difference <- quantile(nfl_data_clean$win_difference, 0.75)
IQR_win_difference <- Q3_win_difference - Q1_win_difference

# define outliers (values outside of Q1 - 1.3*IQR and Q3 + 1.3*IQR)
outliers <- nfl_data_clean |> 
  filter(win_difference < (Q1_win_difference - 1.3 * IQR_win_difference) | win_difference > (Q3_win_difference + 1.3 * IQR_win_difference))

# create boxplot
ggplot(nfl_data_clean, aes(x = "", y = win_difference)) + 
  geom_boxplot(fill = "pink", color = "black") + 
  geom_point(data = outliers, aes(x = "", y = win_difference, color = team), size = 3) +
  labs(title = "Win Differences with Outliers Highlighted", 
       subtitle = "The Minnesota Vikings overperform the most along with the Raiders and the Steelers\nwhile the Cowboys and the Falcons underperform the most each season",
       y = "Win Difference",
       x = " ",
       color = "Team") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),       
    plot.subtitle = element_text(hjust = 0.5)   
  )
  
### Are there any teams who receive more penalties? Does that correlate with wins? ----

# create barplot that shows teams with most penalties
# Generating a pastel rainbow palette using AI
pastel_rainbow <- hue_pal(h = c(0, 360), c = 70, l = 80)(length(unique(nfl_data_clean$team)))

ggplot(nfl_data_clean, aes(x = team, y = penalties, fill = team)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = pastel_rainbow) +
  labs(
    title = "Frequency of Penalties per Team",
    subtitle = "The Las Vegas Raiders have the most penalties in the NFL,\nand the Indianapolis Colts have the least penalties against them in the NFL",
    x = "Team",
    y = "Penalties"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),       
    plot.subtitle = element_text(hjust = 0.5) 
  )

# make scatterplot of the top three penalty and bottom three penalty teams
nfl_data_clean |> 
  filter(team %in% c("Las Vegas Raiders", "Arizona Cardinals", "Baltimore Ravens", 
                     "Indianapolis Colts", "Atlanta Falcons", "New England Patriots")) |> 
  ggplot(aes(x = penalties, y = wins, color = team)) + 
  geom_point(alpha = 0.9) + 
  facet_wrap(~team) + 
  labs(title = "Penalties vs Wins for Selected NFL Teams",
       subtitle = "Less penalties correlate with a higher number of wins in the NFL",
       x = "Penalties",
       y = "Wins") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),       
        plot.subtitle = element_text(hjust = 0.5))

nfl_data_clean |>
  filter(team == c("Las Vegas Raiders", "Arizona Cardinals", "Baltimore Ravens",
                   "Indianapolis Colts", "Atlanta Falcons", "New England Patriots")) |>
  summarize(average_penalties = mean(penalties),
            average_wins = mean(wins),
            .by = team) |>
  arrange(average_penalties) |>
  kable(col.names = c("Team", "Average Number of Penalties", "Average Number of Wins"),
        caption = "Number of Wins for the Teams with the Highest and Lowest Penalty Averages")


# make a boxplot to show the outlier seasons - which teams had a lot of penalties for different seasons
# calculate IQR for penalties
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

# define outliers (values outside of Q1 - 1.6*IQR and Q3 + 1.6*IQR)
outliers_penalties <- nfl_data_clean |> 
  filter(penalties < (Q1_penalties - 1.6 * IQR_penalties) | penalties > (Q3_penalties + 1.6 * IQR_penalties))

# create the boxplot with outliers highlighted
ggplot(nfl_data_clean, aes(x = "", y = penalties)) + 
  geom_boxplot(fill = "pink", color = "black", outlier.shape = NA) +  # Suppress default outliers
  geom_point(data = outliers_penalties, aes(x = "", y = penalties, color = team), size = 3) +
  labs(
    title = "Boxplot of Penalties with Outliers Highlighted",
    subtitle = "The Las Vegas Raiders have two seasons with an exceptionally high number of penalties\nwhile the Atlanta Falcons have a season with the lowest number of penalties",
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

----

#boxplot of specific games that have high penalties and teams that have high penalties
Q1_penalties <- quantile(nfl_data_clean$penalties, 0.25)
Q3_penalties <- quantile(nfl_data_clean$penalties, 0.75, na.rm = TRUE)
IQR_penalties <- Q3_penalties - Q1_penalties

# Define outliers (values outside of Q1 - 1.6*IQR and Q3 + 1.6*IQR)
outliers_penalties <- nfl_data_clean |> 
  filter(penalties < (Q1_penalties - 1.6 * IQR_penalties) | penalties > (Q3_penalties + 1.6 * IQR_penalties))

# create the boxplot with outliers highlighted for penalties
ggplot(nfl_data_clean, aes(x = "", y = penalties)) + 
  geom_boxplot(fill = "pink", color = "black", outlier.shape = NA) +  # Suppress default outliers
  geom_point(data = outliers_penalties, aes(x = "", y = penalties, color = team), size = 3) +
  labs(
    title = "Penalties with Outliers Highlighted",
    subtitle = "The Raiders and Seahawks have seasons with very high penalties\nwhile the Patriots and the Falcons have seasons with very low penalties",
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

ggplot(nfl_data_clean, aes(x = team, y = penalties, fill = team)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = pastel_rainbow) +
  labs(
    title = "Frequency of Penalties per Team",
    subtitle = "The Las Vegas Raiders have the highest average penalty rate in the last two decades",
    x = "Team",
    y = "Penalties"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
    )
  

#scatterplot of penalties for highest and lowest penalty teams
nfl_data_clean |> 
  filter(team %in% c("Las Vegas Raiders", "Arizona Cardinals", "Baltimore Ravens", 
                     "Indianapolis Colts", "Atlanta Falcons", "New England Patriots")) |> 
  ggplot(aes(x = penalties, y = wins, color = team)) + 
  geom_point(alpha = 0.9) + 
  facet_wrap(~team) +  # Create a separate plot for each team
  labs(
    title = "Penalties vs Wins for Selected NFL Teams",
    subtitle = "Teams with less penalties tend to have more wins per season",
    x = "Penalties",
    y = "Wins"
  ) + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )


# summary table of penalties 
nfl_data_clean |> 
  filter(team %in% c("Las Vegas Raiders", "Arizona Cardinals", "Baltimore Ravens",
                     "Indianapolis Colts", "Atlanta Falcons", "New England Patriots")) |> 
  summarize(
    average_penalties = mean(penalties, na.rm = TRUE),
    average_wins = mean(wins, na.rm = TRUE),
    .by = team
  ) |> 
  kable(
    col.names = c("Team", "Average Penalties", "Average Wins"),
    caption = "Average Wins for the Teams with the Least and Most Penalties"
  )


## Figures for Research Question Three (Analysis of Divisions and Conferences in the NFL) ----
### Which divisions have the highest average wins per season? ----
# barplot of divisions with the highest average wins faceted by conference
nfl_data_clean |> 
  group_by(conference, division) |> 
  summarize(avg_wins = mean(wins)) |> 
  ggplot(aes(x = division, y = avg_wins)) + 
  geom_col(fill = "pink", color = "black") +
  labs(
    title = "Average Wins per Division",
    subtitle = "The AFC east has the highest average wins over all eight divisions,\nand the NFC east has the most wins in the NFC conference",
    x = "Division",
    y = "Average Wins"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) + 
  facet_wrap(~conference)  

### Is it harder to make the playoffs in certain divisions? ----
# summarize average wins for playoff teams by division in a table
nfl_data_clean |> 
  filter(playoff == "Yes") |>  
  group_by(conference, division) |> 
  summarize(avg_wins_playoff = mean(wins, na.rm = TRUE)) |>
  kable(col.names = c("Conference", "Division", "Average Wins"),
        caption = "Average Wins for Playoff Teams by Conference and Division")

# make a bar chart with the same data from the table
nfl_data_clean |> 
  filter(playoff == "Yes") |>  
  group_by(conference, division) |> 
  summarize(avg_wins_playoff = mean(wins, na.rm = TRUE)) |>  
  ggplot(aes(x = division, y = avg_wins_playoff)) + 
  geom_col(fill = "pink", color = "black") +  
  labs(title = "Average Wins for Playoff Teams by Division",
       subtitle = "The AFC East has the highest average wins for playoff teams in the AFC,\nand the NFC South has the highest average wins for playoff teams in the NFC",
       x = "Division", 
       y = "Average Wins for Playoff Teams") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) + 
  facet_wrap(~ conference)


### How is Superbowl performance correlated to division wins? ----

#table of superbowl appearances by division
nfl_data_clean |> 
  filter(superbowl == "Yes") |>  
  group_by(conference, division) |> 
  summarize(count = n()) |>
  kable(col.names = c("Conference", "Division", "Number of Teams"),
        caption = "Number of Teams from Each Conference and Division to Have Made the Super Bowl")

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






## Missingness Graphs ----

#missingness for the raw dataset
miss_var_summary(nfl_data) |>
  kable(col.names = c("Variable", "Number of Missing Observations", "Percent Missing"),
        caption = "Summary of Missing Variables in the Raw NFL Dataset")

vis_miss(nfl_data)
gg_miss_upset(nfl_data)
gg_miss_var(nfl_data)

#missingness for the clean dataset
miss_var_summary(nfl_data_clean) |>
  kable(col.names = c("Variable", "Number of Missing Observations", "Percent Missing"),
        caption = "Summary of Missing Variables in the Clean NFL Dataset")

vis_miss(nfl_data_clean)
gg_miss_var(nfl_data_clean)

