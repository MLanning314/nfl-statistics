
## Load Libraries ----
#| echo: false
library(tidyverse)
library(knitr)

##Load Data ----
#| echo: false
nfl_data <- read.csv("data/team_stats_2003_2023.csv")

## Load Codebook
# Define the metadata for the codebook
nfl_data_clean_codebook <- data.frame(
  Variable = c("year", "team", "wins", "losses", "win_loss_perc", "points_scored", 
                    "points_allowed", "points_diff", "margin_of_victory", "games_played", "total_yards", 
                    "plays_offense", "yds_per_play_offense", "turnovers", "fumbles", "first_down", 
                    "pass_completion", "pass_att", "pass_yds", "pass_td", "pass_int", "pass_net_yds_per_att",
                    "pass_first_down", "rush_att", "rush_yds", "rush_td", "rush_yds_per_att", "rush_fd",
                    "penalties", "penalties_yds", "pen_fd", "score_pct", "turnover_pct", "expected_points_offense",
                    "conference", "division", "playoff", "superbowl"),
  Description = c(
    "Season year",
    "Team name",
    "Number of games won",
    "Number of games lost",
    "Win/Loss Percentage",
    "Total points scored by the team",
    "Total points allowed by the team",
    "Points differential",
    "Average margin of victory",
    "Number of games played",
    "Offensive yards gained",
    "Offensive plays ran",
    "Yards per play offense",
    "Team turnovers lost",
    "Team fumbles lost",
    "First downs gained",
    "Passes completed",
    "Pass attempts",
    "Passing yards",
    "Passing touchdowns",
    "Interceptions thrown",
    "Net yards gained per pass attempt",
    "Passing first downs gained",
    "Rushing attempts",
    "Rushing yards",
    "Rushing touchdowns",
    "Rushing yards per attempt",
    "Rushing first downs",
    "Penalties committed",
    "Penalty yards committed",
    "First downs by penalty",
    "Percentage of drives ending in score",
    "Percentage of drives ending in turnover",
    "Expected points contributed by offense",
    "NFL conference",
    "NFL division",
    "Playoff team",
    "Superbowl winner"
  ))




## Renaming Variables ----
#| echo: false

#rename confusing variables
nfl_data_clean <- nfl_data |>
  rename(games_played = g,
         margin_of_victory = mov,
         points_scored = points,
         points_allowed = points_opp,
         fumbles = fumbles_lost,
         pass_completion = pass_cmp,
         expected_points_offense = exp_pts_tot,
         pass_first_down = pass_fd
  )

## Merging Team Names ----
#| echo: false

#combining data for teams with different names
nfl_data_clean <- nfl_data_clean |>
  mutate(team = case_when(
    team %in% c("Oakland Raiders", "Las Vegas Raiders") ~ "Las Vegas Raiders", 
    team %in% c("St. Louis Rams", "Los Angeles Rams") ~ "Los Angeles Rams",
    team %in% c("Washington Commanders", "Washington Redskins", "Washington Football Team") ~ "Washington Commanders",
    team %in% c("San Diego Chargers", "Los Angeles Chargers") ~ "Los Angeles Chargers",
    TRUE ~ team # keep other teams unchanged
  ))

## Adding the `Conference`` Variable ----
#| echo: false 

nfl_data_clean <- nfl_data_clean |>
  
  #create "conference" variable
  mutate(conference = case_when(
    # AFC Teams
    team %in% c(
      "Buffalo Bills", "Miami Dolphins", "New England Patriots", "New York Jets",
      "Baltimore Ravens", "Cincinnati Bengals", "Cleveland Browns", "Pittsburgh Steelers",
      "Houston Texans", "Indianapolis Colts", "Jacksonville Jaguars", "Tennessee Titans",
      "Denver Broncos", "Kansas City Chiefs", "Las Vegas Raiders", "Oakland Raiders", "Los Angeles Chargers", "San Diego Chargers"
    ) ~ "AFC",
    
    # NFC Teams
    team %in% c(
      "Dallas Cowboys", "New York Giants", "Philadelphia Eagles", "Washington Commanders", "Washington Redskins", "Washington Football Team",
      "Chicago Bears", "Detroit Lions", "Green Bay Packers", "Minnesota Vikings",
      "Atlanta Falcons", "Carolina Panthers", "New Orleans Saints", "Tampa Bay Buccaneers",
      "Arizona Cardinals", "Los Angeles Rams", "San Francisco 49ers", "Seattle Seahawks", "St. Louis Rams"
    ) ~ "NFC",
    
    # Default case for unexpected values
    TRUE ~ NA_character_
  ))

## Adding the `Division` Variable ----
#| echo: false

nfl_data_clean <- nfl_data_clean |>
  
  #create "division" variable
  mutate(division = case_when(
    # AFC North Teams
    team %in% c(
      "Baltimore Ravens", "Cincinnati Bengals", "Cleveland Browns", "Pittsburgh Steelers"
    ) ~ "North",
    
    #AFC East Teams
    team %in% c(
      "Buffalo Bills", "Miami Dolphins", "New England Patriots", "New York Jets"
    ) ~ "East",
    
    #AFC South Teams
    team %in% c(
      "Houston Texans", "Indianapolis Colts", "Jacksonville Jaguars", "Tennessee Titans"
    ) ~ "South",
    
    #AFC West Teams
    team %in% c(
      "Denver Broncos", "Kansas City Chiefs", "Oakland Raiders", "Las Vegas Raiders", "Los Angeles Chargers", "San Diego Chargers"
    ) ~ "West",
    
    # NFC North Teams
    team %in% c(
      "Chicago Bears", "Green Bay Packers", "Detroit Lions", "Minnesota Vikings"
    ) ~ "North",
    
    #NFC East Teams
    team %in% c(
      "Dallas Cowboys", "New York Giants", "Philadelphia Eagles", "Washington Commanders", "Washington Redskins", "Washington Football Team"
    ) ~ "East",
    
    #NFC South Teams
    team %in% c(
      "Atlanta Falcons", "Carolina Panthers", "New Orleans Saints", "Tampa Bay Buccaneers"
    ) ~ "South",
    
    #NFC West Teams
    team %in% c(
      "Arizona Cardinals", "Los Angeles Rams", "St. Louis Rams", "San Francisco 49ers", "Seattle Seahawks"
    ) ~ "West",
    
    # Default case for unexpected values
    TRUE ~ NA_character_
  ))

## Creating the `Playoff` and `Superbowl` Datasets ----

#creating playoff variable
# playoff data by year
playoff_teams <- list(
  "2003" = c("New England Patriots", "Kansas City Chiefs", "Indianapolis Colts", 
             "Tennessee Titans", "Baltimore Ravens", "Denver Broncos", 
             "Philadelphia Eagles", "Los Angeles Rams", "Carolina Panthers", 
             "Green Bay Packers", "Dallas Cowboys", "Seattle Seahawks"),
  "2004" = c("New England Patriots", "Pittsburgh Steelers", "Indianapolis Colts", 
             "Los Angeles Chargers", "New York Jets", "Denver Broncos", 
             "Philadelphia Eagles", "Atlanta Falcons", "Green Bay Packers", 
             "Minnesota Vikings", "Seattle Seahawks", "Los Angeles Rams"),
  "2005" = c("Pittsburgh Steelers", "Cincinnati Bengals", "New England Patriots",
             "Jacksonville Jaguars", "Denver Broncos", "Indianapolis Colts",
             "Carolina Panthers", "New York Giants", "Washington Commanders",
             "Tampa Bay Buccaneers", "Chicago Bears", "Seattle Seahawks"),
  "2006" = c("Indianapolis Colts", "Kansas City Chiefs", "New England Patriots",
             "New York Jets", "Baltimore Ravens", "Los Angeles Chargers",
             "Seattle Seahawks", "Dallas Cowboys", "Philadelphia Eagles",
             "New York Giants", "New Orleans Saints", "Chicago Bears"),
  "2007" = c("Jacksonville Jaguars", "Pittsburgh Steelers", "Los Angeles Chargers",
             "Tennessee Titans", "New England Patriots", "Indianapolis Colts",
             "New York Giants", "Tampa Bay Buccaneers", "Seattle Seahawks",
             "Washington Commanders", "Green Bay Packers", "Dallas Cowboys"),
  "2008" = c("Los Angeles Chargers", "Indianapolis Colts", "Baltimore Ravens",
             "Miami Dolphins", "Tennessee Titans", "Pittsburgh Steelers",
             "Philadelphia Eagles", "Minnesota Vikings", "Arizona Cardinals",
             "Atlanta Falcons", "Carolina Panthers", "New York Giants"),
  "2009" = c("New York Jets", "Cincinatti Bengals", "Baltimore Ravens",
             "New England Patriots", "Indianapolis Colts", "Los Angeles Chargers",
             "Dallas Cowboys", "Philadelphia Eagles", "Arizona Cardinals",
             "Green Bay Packers", "New Orleans Saints", "Minnesota Vikings"),
  "2010" = c("New York Jets", "Indianapolis Colts", "Baltimore Ravens",
             "Kansas City Chiefs", "Pittsburgh Steelers", "New England Patriots",
             "Seattle Washington", "New Orleans Saints", "Green Bay Packers",
             "Philadelphia Eagles", "Atlanta Falcons", "Chicago Bears"),
  "2011" = c("Houston Texans", "Cincinatti Bengals", "Denver Broncos",
             "Pittsburgh Steelers", "New England Patriots", "Baltimore Ravens",
             "New York Giants", "Atlanta Falcons", "New Orleans Saints",
             "Detroit Lions", "San Francisco 49ers", "Green Bay Packers"),
  "2012" = c("Houston Texans", "Cincinnati Bengals", "Baltimore Ravens",
             "Indianapolis Colts", "Denver Broncos", "New England Patriots",
             "Green Bay Packers", "Minnesota Vikings", "Seattle Seahawks",
             "Washington Commanders", "Atlanta Falcons", "San Francisco 49ers"),
  "2013" = c("Denver Broncos", "New England Patriots", "Cincinnati Bengals",
             "Indianapolis Colts", "Kansas City Chiefs", "Los Angeles Chargers",
             "Seattle Seahawks", "Carolina Panthers", "Philadelphia Eagles",
             "Green Bay Packers", "New Orleans Saints", "San Francisco 49ers"),
  "2014" = c("New England Patriots", "Denver Broncos", "Pittsburgh Steelers",
             "Indianapolis Colts", "Cincinnati Bengals", "Baltimore Ravens",
             "Seattle Seahawks", "Green Bay Packers", "Dallas Cowboys",
             "Carolina Panthers", "Arizona Cardinals", "Detroit Lions"),
  "2015" = c("Denver Broncos", "New England Patriots", "Cincinnati Bengals",
             "Houston Texans", "Kansas City Chiefs", "Pittsburgh Steelers",
             "Carolina Panthers", "Arizona Cardinals", "Minnesota Vikings",
             "Washington Commanders", "Seattle Seahawks", "Green Bay Packers"),
  "2016" = c("New England Patriots", "Kansas City Chiefs", "Pittsburgh Steelers",
             "Houston Texans", "Las Vegas Raiders", "Miami Dolphins",
             "Dallas Cowboys", "Atlanta Falcons", "Green Bay Packers",
             "Seattle Seahawks", "New York Giants", "Detroit Lions"),
  "2017" = c("New England Patriots", "Pittsburgh Steelers", "Jacksonville Jaguars", 
             "Tennessee Titans", "Kansas City Chiefs", "Buffalo Bills",
             "Philadelphia Eagles", "Minnesota Vikings", "Los Angeles Rams", 
             "New Orleans Saints", "Carolina Panthers", "Atlanta Falcons"),
  "2018" = c("Kansas City Chiefs", "New England Patriots", "Houston Texans", 
             "Baltimore Ravens", "Los Angeles Chargers", "Indianapolis Colts",
             "Los Angeles Rams", "New Orleans Saints", "Chicago Bears", 
             "Dallas Cowboys", "Seattle Seahawks", "Philadelphia Eagles"),
  "2019" = c("Baltimore Ravens", "Kansas City Chiefs", "New England Patriots", 
             "Houston Texans", "Buffalo Bills", "Tennessee Titans",
             "San Francisco 49ers", "Green Bay Packers", "New Orleans Saints", 
             "Minnesota Vikings", "Seattle Seahawks", "Philadelphia Eagles"),
  "2020" = c("Kansas City Chiefs", "Buffalo Bills", "Pittsburgh Steelers", 
             "Tennessee Titans", "Baltimore Ravens", "Cleveland Browns", 
             "Indianapolis Colts","Green Bay Packers", "New Orleans Saints", 
             "Seattle Seahawks", "Washington Commanders", "Tampa Bay Buccaneers",
             "Los Angeles Rams", "Chicago Bears"),
  "2021" = c("Tennessee Titans", "Kansas City Chiefs", "Buffalo Bills",
             "Cincinnati Bengals", "Las Vegas Raiders", "New England Patriots", 
             "Indianapolis Colts", "Los Angeles Chargers", "Green Bay Packers", 
             "Tampa Bay Buccaneers", "Dallas Cowboys", "Los Angeles Rams", 
             "Arizona Cardinals", "San Francisco 49ers", "Philadelphia Eagles",
             "New Orleans Saints"),
  "2022" = c("Kansas City Chiefs", "Buffalo Bills", "Cincinnati Bengals", 
             "Jacksonville Jaguars", "Los Angeles Chargers", "Baltimore Ravens", 
             "Miami Dolphins", "Tennessee Titans", "Philadelphia Eagles", 
             "San Francisco 49ers", "Minnesota Vikings", "Tampa Bay Buccaneers", 
             "Dallas Cowboys", "New York Giants", "Seattle Seahawks", 
             "Washington Commanders"),
  "2023" = c("Kansas City Chiefs", "Buffalo Bills", "Cincinnati Bengals", 
             "Jacksonville Jaguars", "Miami Dolphins", "Baltimore Ravens", 
             "Los Angeles Chargers", "Denver Broncos", "Philadelphia Eagles", 
             "San Francisco 49ers", "Dallas Cowboys", "Minnesota Vikings", 
             "Tampa Bay Buccaneers", "Detroit Lions", "Green Bay Packers", 
             "Washington Commanders")
  
)

#list of all NFL teams
all_teams <- c(
  "Arizona Cardinals", "Atlanta Falcons", "Baltimore Ravens", "Buffalo Bills",
  "Carolina Panthers", "Chicago Bears", "Cincinnati Bengals", "Cleveland Browns",
  "Dallas Cowboys", "Denver Broncos", "Detroit Lions", "Green Bay Packers",
  "Houston Texans", "Indianapolis Colts", "Jacksonville Jaguars", "Kansas City Chiefs",
  "Las Vegas Raiders", "Los Angeles Chargers", "Los Angeles Rams", "Miami Dolphins",
  "Minnesota Vikings", "New England Patriots", "New Orleans Saints", "New York Giants",
  "New York Jets", "Philadelphia Eagles", "Pittsburgh Steelers", "San Francisco 49ers",
  "Seattle Seahawks", "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Commanders"
)


# adding the `Superbowl` Variable
#superbowl winners by year
superbowl_winner <- list(
  "2003" = "New England Patriots",
  "2004" = "New England Patriots",
  "2005" = "New England Patriots", 
  "2006" = "Pittsburgh Steelers",
  "2007" = "Indianapolis Colts",
  "2008" = "New York Giants",
  "2009" = "Pittsburgh Steelers",
  "2010" = "New Orleans Saints",
  "2011" = "Green Bay Packers", 
  "2012" = "New York Giants",
  "2013" = "Baltimore Ravens",
  "2014" = "Seattle Seahawks", 
  "2015" = "New England Patriots",
  "2016" = "Denver Broncos",
  "2017" = "New England Patriots",
  "2018" = "Philadelphia Eagles",
  "2019" = "New England Patriots",
  "2020" = "Kansas City Chiefs",
  "2021" = "Tampa Bay Buccaneers",
  "2022" = "Los Angeles Rams",
  "2023" = "Kansas City Chiefs"
  )


# list of all NFL teams
all_teams <- c(
  "Arizona Cardinals", "Atlanta Falcons", "Baltimore Ravens", "Buffalo Bills",
  "Carolina Panthers", "Chicago Bears", "Cincinnati Bengals", "Cleveland Browns",
  "Dallas Cowboys", "Denver Broncos", "Detroit Lions", "Green Bay Packers",
  "Houston Texans", "Indianapolis Colts", "Jacksonville Jaguars", "Kansas City Chiefs",
  "Las Vegas Raiders", "Los Angeles Chargers", "Los Angeles Rams", "Miami Dolphins",
  "Minnesota Vikings", "New England Patriots", "New Orleans Saints", "New York Giants",
  "New York Jets", "Philadelphia Eagles", "Pittsburgh Steelers", "San Francisco 49ers",
  "Seattle Seahawks", "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Commanders"
)
  
#create the dataset for playoff teams by year
nfl_playoff_data <- bind_rows(lapply(names(playoff_teams), function(year) {
  data.frame(
    year = as.numeric(year),
    team = all_teams,
    playoff = ifelse(all_teams %in% playoff_teams[[year]], "Yes", "No")
  )
}))

# Create the dataset for Super Bowl winners by year
nfl_superbowl_data <- bind_rows(lapply(names(superbowl_winner), function(year) {
  data.frame(
    year = as.numeric(year),
    team = all_teams,
    superbowl = ifelse(all_teams %in% superbowl_winner[[year]], "Yes", "No")
  )
}))

## Joining the `Playoff` and `Superbowl` Datasets to nfl_data_clean ----

# merge the playoff and Super Bowl data with nfl_data_clean
nfl_data_clean <- nfl_data_clean %>%
  left_join(nfl_playoff_data, by = c("year", "team")) |>
  left_join(nfl_superbowl_data, by = c("year", "team"))


## Removing Variables ----
#| echo: false

#remove the "ties" variable
nfl_data_clean <- nfl_data_clean |>
  select(-ties)

