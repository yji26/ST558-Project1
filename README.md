ST 558 - Summer 2020 - Project 1
================
Yun Ji
6/12/2020

  - [Technical Overview](#technical-overview)
      - [JSON Data](#json-data)
      - [JSON Processing in R](#json-processing-in-r)
  - [Query NHL API Functions](#query-nhl-api-functions)
  - [Query Data from the NHL API](#query-data-from-the-nhl-api)
  - [Plotting Data](#plotting-data)
      - [History and Winning Percentage of NHL
        Franchises](#history-and-winning-percentage-of-nhl-franchises)
      - [Season Results for Central
        Teams](#season-results-for-central-teams)
      - [Season Results Barplot](#season-results-barplot)
      - [Season Results Boxplot](#season-results-boxplot)
      - [Querying Player-Level Data](#querying-player-level-data)
      - [Displaying Player Data](#displaying-player-data)
  - [Blog](#blog)
  - [My GitHub Project Repository](#my-github-project-repository)

## Technical Overview

### JSON Data

JavaScript Object Notation, abbreviated as JSON, is a human-readable
text object used to store and transmit data. JSON data objects consist
of one or more key-value pairs, where the key describes the data field
and its value is the data itself. For example, a JSON can contain a
key-value pair of `apple: 10` indicating 10 apples and another key-value
pair of `orange: 15` for 15 oranges. Key-value pairs in JSON can also be
nested, thus imposing hierarchy on the data if needed: an example key of
`apples` can have as its value a collection of different types of apples
and their counts, such as `red delicious: 8` and `mcintosh: 2`.

JSON data objects are not dependent on any single programming language,
and indeed most modern programming languages have functions and packages
for creating or reading JSON data. Because of these characteristics,
JSON has found wide adoption as a method for transmitting numerical and
text data between disparate computer systems and databases.

### JSON Processing in R

In R, there are free, open-source packages to process JSON data objects;
these include `rjson`, `RJSONIO` and `jsonlite`. Each package handles
JSON slightly differently, including how to process nested structures,
missing values, and special characters. `rjson` is generally fastest but
has the fewest advanced features. `RJSONIO` has many features but is by
far the slowest. `jsonlite` occupies a good middle ground, offering
speeds comparable to `rjson` while having nearly as many features as
`RJSONIO`, and is therefore my choice to process the JSON data objects
involved in this project. Speed and feature comparisons can be found on
[this
website](https://rstudio-pubs-static.s3.amazonaws.com/31702_9c22e3d1a0c44968a4a1f9656f1800ab.html).

## Query NHL API Functions

To query the NHL API, I first set up the variables and data structures
needed as inputs to my API query functions. The base URL is stored as a
character string, and the individual API endpoints are stored in a list
called `endpoints` - this is to help future-proof expanding the queries
in order to add other API endpoints later on.

``` r
baseURL <- "https://records.nhl.com/site/api"

endpoints <- list()
endpoints$franchise <- "/franchise"
endpoints$teamTotals <- "/franchise-team-totals"
endpoints$seasonRecords <- "/franchise-season-records?cayenneExp=franchiseId=ID"
endpoints$goalieRecords <- "/franchise-goalie-records?cayenneExp=franchiseId=ID"
endpoints$skaterRecords <- "/franchise-skater-records?cayenneExp=franchiseId=ID"
```

Next I created functions to support querying the API. The function
`createEndpoint` checks the user input for a valid endpoint, and if
valid returns the complete endpoint URL as the output.

``` r
createEndpoint <- function(baseURL, endpointList, endpointName) {
  endpoint <- endpointList[[endpointName]]
  
  if(is.null(endpoint)) {
    stop(paste0("Invalid endpoint for ", endpointName))
  } else {
    return(paste0(baseURL, endpoint))
  }
}
```

Functions `getDataNoOptions` and `getDataByFranchiseId` handles the two
types of endpoints in the NHL API: those requiring no further input from
the user, and those that require the user to specify a numerical value
for franchise ID. The functions will query the API for JSON data,
flatten the JSON object and convert the data returned into a `tibble`
data frame.

``` r
getDataNoOptions <- function(fullURL) {
  rawData <- fromJSON(content(GET(fullURL), "text"), flatten = TRUE)
  return(tbl_df(rawData$data))
}

getDataByFranchiseId <- function(fullURL, franchiseId) {
  franchiseId <- as.numeric(franchiseId)
  
  if(is.na(franchiseId)) {
    stop("Franchise ID must be a number")
  } else if(franchiseId < 1 | franchiseId > 38) {
    stop("Franchise ID must between 1 and 38")
  }

  fullURL <- str_replace(fullURL, "ID", as.character(franchiseId))
  rawData <- fromJSON(content(GET(fullURL), "text"), flatten = TRUE)
  return(tbl_df(rawData$data))
}
```

Finally, the wrapper function `runNhlApi` acts as an orchestrator and
handles the top-down logic of getting data from the NHL API. Note that
unnamed arguments are used in `runNhlApi` to handle the additional user
input needed by `getDataByFranchiseId`.

``` r
runNhlApi <- function(baseURL, endpointList, endpointName, ...) {
  fullURL <- createEndpoint(baseURL, endpointList, endpointName)

  if(endpointName %in% c("franchise","teamTotals")) {
    data <- getDataNoOptions(fullURL)
  } else if(endpointName %in% c("seasonRecords","goalieRecords","skaterRecords","seasonResults")) {
    data <- getDataByFranchiseId(fullURL, ...)
  } else {
    stop("No data found!")
  }
  return(data)
}
```

## Query Data from the NHL API

I ran my functions with the appropriate user inputs to query the needed
data from the NHL API. For the franchise-specific data, I picked the
Blackhawks since I am from Chicago.

``` r
franchise <- runNhlApi(baseURL, endpoints, "franchise")
teamTotals <- runNhlApi(baseURL, endpoints, "teamTotals")
seasonRecordsBlackhawks <- runNhlApi(baseURL, endpoints, "seasonRecords", 11)
goalieRecordsBlackhawks <- runNhlApi(baseURL, endpoints, "goalieRecords", 11)
skaterRecordsBlackhawks <- runNhlApi(baseURL, endpoints, "skaterRecords", 11)
```

A sample of each data frame is shown below:

``` r
franchise
```

    ## # A tibble: 38 x 6
    ##       id firstSeasonId lastSeasonId mostRecentTeamId teamCommonName
    ##    <int>         <int>        <int>            <int> <chr>         
    ##  1     1      19171918           NA                8 Canadiens     
    ##  2     2      19171918     19171918               41 Wanderers     
    ##  3     3      19171918     19341935               45 Eagles        
    ##  4     4      19191920     19241925               37 Tigers        
    ##  5     5      19171918           NA               10 Maple Leafs   
    ##  6     6      19241925           NA                6 Bruins        
    ##  7     7      19241925     19371938               43 Maroons       
    ##  8     8      19251926     19411942               51 Americans     
    ##  9     9      19251926     19301931               39 Quakers       
    ## 10    10      19261927           NA                3 Rangers       
    ## # ... with 28 more rows, and 1 more variable: teamPlaceName <chr>

``` r
teamTotals
```

    ## # A tibble: 104 x 30
    ##       id activeFranchise firstSeasonId franchiseId gameTypeId gamesPlayed
    ##    <int>           <int>         <int>       <int>      <int>       <int>
    ##  1     1               1      19821983          23          2        2937
    ##  2     2               1      19821983          23          3         257
    ##  3     3               1      19721973          22          2        3732
    ##  4     4               1      19721973          22          3         272
    ##  5     5               1      19261927          10          2        6504
    ##  6     6               1      19261927          10          3         515
    ##  7     7               1      19671968          16          3         433
    ##  8     8               1      19671968          16          2        4115
    ##  9     9               1      19671968          17          2        4115
    ## 10    10               1      19671968          17          3         381
    ## # ... with 94 more rows, and 24 more variables: goalsAgainst <int>,
    ## #   goalsFor <int>, homeLosses <int>, homeOvertimeLosses <int>, homeTies <int>,
    ## #   homeWins <int>, lastSeasonId <int>, losses <int>, overtimeLosses <int>,
    ## #   penaltyMinutes <int>, pointPctg <dbl>, points <int>, roadLosses <int>,
    ## #   roadOvertimeLosses <int>, roadTies <int>, roadWins <int>,
    ## #   shootoutLosses <int>, shootoutWins <int>, shutouts <int>, teamId <int>,
    ## #   teamName <chr>, ties <int>, triCode <chr>, wins <int>

``` r
seasonRecordsBlackhawks
```

    ## # A tibble: 1 x 57
    ##      id fewestGoals fewestGoalsAgai~ fewestGoalsAgai~ fewestGoalsSeas~
    ##   <int>       <int>            <int> <chr>            <chr>           
    ## 1    16         133              164 1973-74 (78)     1953-54 (70)    
    ## # ... with 52 more variables: fewestLosses <int>, fewestLossesSeasons <chr>,
    ## #   fewestPoints <int>, fewestPointsSeasons <chr>, fewestTies <int>,
    ## #   fewestTiesSeasons <chr>, fewestWins <int>, fewestWinsSeasons <chr>,
    ## #   franchiseId <int>, franchiseName <chr>, homeLossStreak <int>,
    ## #   homeLossStreakDates <chr>, homePointStreak <int>,
    ## #   homePointStreakDates <chr>, homeWinStreak <int>, homeWinStreakDates <chr>,
    ## #   homeWinlessStreak <int>, homeWinlessStreakDates <chr>, lossStreak <int>,
    ## #   lossStreakDates <chr>, mostGameGoals <int>, mostGameGoalsDates <chr>,
    ## #   mostGoals <int>, mostGoalsAgainst <int>, mostGoalsAgainstSeasons <chr>,
    ## #   mostGoalsSeasons <chr>, mostLosses <int>, mostLossesSeasons <chr>,
    ## #   mostPenaltyMinutes <int>, mostPenaltyMinutesSeasons <chr>,
    ## #   mostPoints <int>, mostPointsSeasons <chr>, mostShutouts <int>,
    ## #   mostShutoutsSeasons <chr>, mostTies <int>, mostTiesSeasons <chr>,
    ## #   mostWins <int>, mostWinsSeasons <chr>, pointStreak <int>,
    ## #   pointStreakDates <chr>, roadLossStreak <int>, roadLossStreakDates <chr>,
    ## #   roadPointStreak <int>, roadPointStreakDates <chr>, roadWinStreak <int>,
    ## #   roadWinStreakDates <chr>, roadWinlessStreak <int>,
    ## #   roadWinlessStreakDates <chr>, winStreak <int>, winStreakDates <chr>,
    ## #   winlessStreak <int>, winlessStreakDates <chr>

``` r
goalieRecordsBlackhawks
```

    ## # A tibble: 48 x 29
    ##       id activePlayer firstName franchiseId franchiseName gameTypeId gamesPlayed
    ##    <int> <lgl>        <chr>           <int> <chr>              <int>       <int>
    ##  1   237 FALSE        Tony               11 Chicago Blac~          2         873
    ##  2   318 TRUE         Corey              11 Chicago Blac~          2         488
    ##  3   343 FALSE        Ed                 11 Chicago Blac~          2         415
    ##  4   376 FALSE        Alain              11 Chicago Blac~          2          66
    ##  5   382 FALSE        Jacques            11 Chicago Blac~          2          53
    ##  6   410 FALSE        Mark               11 Chicago Blac~          2          27
    ##  7   439 FALSE        Jeff               11 Chicago Blac~          2         173
    ##  8   499 FALSE        Bob                11 Chicago Blac~          2          41
    ##  9   523 FALSE        Greg               11 Chicago Blac~          2          13
    ## 10   543 FALSE        Paul               11 Chicago Blac~          2          41
    ## # ... with 38 more rows, and 22 more variables: lastName <chr>, losses <int>,
    ## #   mostGoalsAgainstDates <chr>, mostGoalsAgainstOneGame <int>,
    ## #   mostSavesDates <chr>, mostSavesOneGame <int>, mostShotsAgainstDates <chr>,
    ## #   mostShotsAgainstOneGame <int>, mostShutoutsOneSeason <int>,
    ## #   mostShutoutsSeasonIds <chr>, mostWinsOneSeason <int>,
    ## #   mostWinsSeasonIds <chr>, overtimeLosses <int>, playerId <int>,
    ## #   positionCode <chr>, rookieGamesPlayed <int>, rookieShutouts <int>,
    ## #   rookieWins <int>, seasons <int>, shutouts <int>, ties <int>, wins <int>

``` r
skaterRecordsBlackhawks
```

    ## # A tibble: 864 x 30
    ##       id activePlayer assists firstName franchiseId franchiseName gameTypeId
    ##    <int> <lgl>          <int> <chr>           <int> <chr>              <int>
    ##  1 16923 FALSE            926 Stan               11 Chicago Blac~          2
    ##  2 16989 FALSE            549 Bobby              11 Chicago Blac~          2
    ##  3 17016 FALSE            395 Chris              11 Chicago Blac~          2
    ##  4 17079 FALSE            719 Denis              11 Chicago Blac~          2
    ##  5 17109 FALSE              4 Mike               11 Chicago Blac~          2
    ##  6 17160 TRUE             633 Patrick            11 Chicago Blac~          2
    ##  7 17190 FALSE            554 Doug               11 Chicago Blac~          2
    ##  8 17198 FALSE              0 Norm               11 Chicago Blac~          2
    ##  9 17201 FALSE             12 Clarence           11 Chicago Blac~          2
    ## 10 17203 FALSE              4 Sid                11 Chicago Blac~          2
    ## # ... with 854 more rows, and 23 more variables: gamesPlayed <int>,
    ## #   goals <int>, lastName <chr>, mostAssistsGameDates <chr>,
    ## #   mostAssistsOneGame <int>, mostAssistsOneSeason <int>,
    ## #   mostAssistsSeasonIds <chr>, mostGoalsGameDates <chr>,
    ## #   mostGoalsOneGame <int>, mostGoalsOneSeason <int>, mostGoalsSeasonIds <chr>,
    ## #   mostPenaltyMinutesOneSeason <int>, mostPenaltyMinutesSeasonIds <chr>,
    ## #   mostPointsGameDates <chr>, mostPointsOneGame <int>,
    ## #   mostPointsOneSeason <int>, mostPointsSeasonIds <chr>, penaltyMinutes <int>,
    ## #   playerId <int>, points <int>, positionCode <chr>, rookiePoints <int>,
    ## #   seasons <int>

Besides the `id` column which is a row number and therefore not very
useful, the other columns have comprehensible names for data (at least
to a hockey fan), but some of the data values require additional
cleanup, as we will see below.

## Plotting Data

### History and Winning Percentage of NHL Franchises

For my first plot, I filtered the list of franchises to just the active
franchises, performed data wrangling to get a team’s first season, fill
`NA` values in column `ties` and compute a win percentage using regular
season wins, ties, and games played. I then created a scatterplot of
their regular season win percentage vs. the starting year of the
franchise. I also created the variable `nation` to differentiate
Canadian and U.S. teams, and color-coded the data points of the
scatterplot by nation.

``` r
canada_franchises <- c(1,2,4,5,20,21,25,30,35)

teamTotals <- teamTotals %>%
  mutate(nation = ifelse(franchiseId %in% canada_franchises, "Canada", "USA"))

regularSeasonTeamTotals <- teamTotals %>% 
  filter(gameTypeId == 2, is.na(lastSeasonId)) %>%
  mutate(firstSeason = as.numeric(substr(firstSeasonId,1,4)),
         ties = ifelse(is.na(ties), 0, ties),
         winPercent = (wins+0.5*ties) / gamesPlayed)

g <- ggplot(data = regularSeasonTeamTotals, aes(x = firstSeason, y = winPercent, color = nation))
g + geom_point(size = 4, alpha = 0.5) + 
  labs(title = "NHL Active Franchises Regular Season Win Percentage vs. Starting Year",
       x = "Starting Year", y = "Win Percentage",
       color = "Nation", alpha = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("red","blue"))
```

![](YunJi_Project1_files/figure-gfm/scatterplot%20-%20Win%20Percent%20vs.%20Starting%20Year-1.png)<!-- -->

From the plot, I see that there are many more U.S. teams than Canadian
teams, which makes sense as the U.S. is more populous. In addition,
there is a big gap in starting year where no active franchises began
operations between the early 1930s and the mid 1960s - a long period of
separation between the “Original Six” NHL teams and the newer teams
added in the NHL’s expansion from the late 1960s onwards (there were a
few NHL teams that were founded during this interim period, however they
ceased operations and are therefore not included among active
franchises). Most teams’ lifetime winning percentages hover around 50%
as one might expect, but there is more variability as one gets closer to
the present day.

### Season Results for Central Teams

From the NHL API, I’ve also queried other endpoints to obtain the
season-by-season results of individual teams. To do this, I added a new
endpoint to my list of `endpoints`:

``` r
endpoints$seasonResults <- "/franchise-season-results?cayenneExp=franchiseId=ID"
```

I then used this endpoint to get season data for the Chicago Blackhawks
as well as for each of Chicago’s rival teams in the NHL Central
Division. Finally I concatenated them into a single dataframe for the
entire Central Divison, then filtered for regular season results only.

``` r
seasonResultsBlackhawks <- runNhlApi(baseURL, endpoints, "seasonResults", 11)
seasonResultsBlues <- runNhlApi(baseURL, endpoints, "seasonResults", 18)
seasonResultsStars <- runNhlApi(baseURL, endpoints, "seasonResults", 15)
seasonResultsWild <- runNhlApi(baseURL, endpoints, "seasonResults", 37)
seasonResultsPredators <- runNhlApi(baseURL, endpoints, "seasonResults", 34)
seasonResultsJets <- runNhlApi(baseURL, endpoints, "seasonResults", 35)
seasonResultsAvalanche <- runNhlApi(baseURL, endpoints, "seasonResults", 27)

seasonResultsCentral <- rbind(seasonResultsBlackhawks,
                              seasonResultsBlues,
                              seasonResultsStars,
                              seasonResultsWild,
                              seasonResultsPredators,
                              seasonResultsJets,
                              seasonResultsAvalanche)

seasonResultsCentralRegular <- seasonResultsCentral %>%
  filter(gameTypeId == 2) %>%
  mutate(ties = ifelse(is.na(ties), 0, ties),
         winPercent = (wins+0.5*ties) / gamesPlayed)
```

Some data exploration is in order. If a team has won enough games to
reach the playoffs in a given season, its playoff results will show up
in the `seriesTitle` column of the table (if a team did not make the
playoffs, `seriesTitle` is simply `NA`). Using this fact, I created a
contingency table showing the counts playoff appearances for each
Central Division team since the year 2000.

``` r
playoffAppearancesSince2000 <- seasonResultsCentralRegular %>% 
  filter(as.numeric(substr(seasonId,1,4)) >= 2000)

cont_table <- table(playoffAppearancesSince2000$teamName, playoffAppearancesSince2000$seriesTitle)
kable(cont_table, caption = "NHL Central Playoff Appearances since 2000")
```

|                     | 1st Round | 2nd Round | Conference Finals | Conference Quarterfinals | Conference Semifinals | Stanley Cup Final |
| ------------------- | --------: | --------: | ----------------: | -----------------------: | --------------------: | ----------------: |
| Atlanta Thrashers   |         0 |         0 |                 0 |                        1 |                     0 |                 0 |
| Chicago Blackhawks  |         2 |         0 |                 2 |                        3 |                     0 |                 3 |
| Colorado Avalanche  |         2 |         1 |                 1 |                        2 |                     3 |                 1 |
| Dallas Stars        |         1 |         2 |                 1 |                        3 |                     2 |                 0 |
| Minnesota Wild      |         3 |         2 |                 1 |                        3 |                     0 |                 0 |
| Nashville Predators |         2 |         2 |                 0 |                        5 |                     2 |                 1 |
| St. Louis Blues     |         2 |         1 |                 2 |                        4 |                     2 |                 1 |
| Winnipeg Jets       |         2 |         0 |                 1 |                        0 |                     0 |                 0 |

NHL Central Playoff Appearances since 2000

There are a few issues with the above table: the **Atlanta Thrashers**
are defunct, having relocated in 2011 as the **Winnipeg Jets**. League
changes in 2013 also changed the names of some playoff series: the
**Conference Quarterfinals** became the **1st Round** of the NHL
playoffs, and the **Conference Semifinals** become the **2nd Round** of
the playoffs. Therefore I updated the out-of-date data to their
modern-days names, then re-ran the contingency table.

``` r
playoffAppearancesSince2000 <- seasonResultsCentralRegular %>% 
  filter(as.numeric(substr(seasonId,1,4)) >= 2000) %>%
  mutate(seriesTitle = ifelse(seriesTitle == "Conference Quarterfinals", "1st Round", seriesTitle),
         seriesTitle = ifelse(seriesTitle == "Conference Semifinals", "2nd Round", seriesTitle),
         teamName = ifelse(teamName == "Atlanta Thrashers", "Winnipeg Jets", teamName))

cont_table <- table(playoffAppearancesSince2000$teamName, playoffAppearancesSince2000$seriesTitle)
kable(cont_table, caption = "NHL Central Playoff Appearances since 2000 (improved)")
```

|                     | 1st Round | 2nd Round | Conference Finals | Stanley Cup Final |
| ------------------- | --------: | --------: | ----------------: | ----------------: |
| Chicago Blackhawks  |         5 |         0 |                 2 |                 3 |
| Colorado Avalanche  |         4 |         4 |                 1 |                 1 |
| Dallas Stars        |         4 |         4 |                 1 |                 0 |
| Minnesota Wild      |         6 |         2 |                 1 |                 0 |
| Nashville Predators |         7 |         4 |                 0 |                 1 |
| St. Louis Blues     |         6 |         3 |                 2 |                 1 |
| Winnipeg Jets       |         3 |         0 |                 1 |                 0 |

NHL Central Playoff Appearances since 2000 (improved)

This table looks better than before. While both the Nashville Predators
and St. Louis Blues have reached the playoffs 12 times since 2000, the
Chicago Blackhawks took the most advantage of their playoff appearances,
reaching (and winning\!) the Stanley Cup Final three times in the last
20 years. The Atlanta Thrashers/Winnipeg Jets have experienced the most
futility, reaching the playoffs only four times and getting past the 1st
round only once.

### Season Results Barplot

Using the `group_by` and `summarise` functions, I aggregated all seasons
played by each franchise and tallied the total games played, regular
season and playoffs. This includes seasons before year 2000, and
required a couple more team name changes for the **Quebec Nordiques**
becoming the **Colorado Avalanche** and the **Minnesota North Stars**
migrating south as the **Dallas Stars**. Since I don’t need `geom_bar`
to aggregate my data, I used the `stat = "identity"` option; plotting
regular season and playoff bars side-by-side for each franchise instead
of stacked required the option `position = "dodge"`. I also used custom
bar colors and modified the x-axis franchise names to improve the look
of the chart.

``` r
seasonResultsCentralAgg <- seasonResultsCentral %>%
  mutate(teamName = ifelse(teamName == "Atlanta Thrashers", "Winnipeg Jets", teamName),
         teamName = ifelse(teamName == "Minnesota North Stars", "Dallas Stars", teamName),
         teamName = ifelse(teamName == "Quebec Nordiques", "Colorado Avalanche", teamName)) %>%
  group_by(teamName, gameTypeId) %>%
  summarise(totalGamesPlayed = sum(gamesPlayed)) %>%
  mutate(gameType = ifelse(gameTypeId == 2, "Regular Season", "Playoffs"),
         gameType = fct_relevel(gameType, "Regular Season", "Playoffs"))

g <- ggplot(data = seasonResultsCentralAgg, aes(x = teamName, y = totalGamesPlayed, fill = gameType))
g + geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = "", values = c("grey40","orange")) +
  scale_x_discrete(labels = c("Chicago Blackhawks" = "Chicago\nBlackhawks",
                              "Colorado Avalanche" = "Colorado\nAvalanche",
                              "Dallas Stars" = "Dallas\nStars",
                              "Minnesota Wild" = "Minnesota\nWild",
                              "Nashville Predators" = "Nashville\nPredators",
                              "St. Louis Blues" = "St. Louis\nBlues",
                              "Winnipeg Jets" = "Winnipeg\nJets")) +
  labs(title = "NHL Central Teams Total Games Played\nRegular Season and Playoffs",
       x = "", y = "Games Played") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](YunJi_Project1_files/figure-gfm/season%20barplot-1.png)<!-- -->

I spent quite some time getting this chart to display what I intended. I
learned that `geom_bar` can be used with a raw data frame to aggregate
and visualize some data, but it is not flexible enough to handle all
types of aggregations. Sometimes it’s simpler to create a new data frame
with the exact data you want to present, then use `geom_bar` focusing
only on the visualization aspect. And although the barplot is a clear
visualization, it doesn’t speak to the relative success of these teams:
older teams played more regular season games and more playoff games,
whereas newer teams had fewer of both.

### Season Results Boxplot

To compare the relative success of Central Division teams, I plotted the
win percentage for each season that each team experienced as both a
boxplot and a scatterplot. Instead of using `teamName`, this time I
utilized `franchiseId` as the team’s identifier and renamed this label
from the `franchiseId` number to their corresponding team names. Instead
of listing teams alphabetically as in the barplot, this time I ordered
teams by time of founding to see if in the division there are any trends
going from oldest to newest teams.

Note that since the scatterplot data points must come in front of the
boxplot, the `geom_point` function is invokved after the `geom_boxplot`
function in order to overlay on top of it. With the scatterplot, the
`jitter` position option was used to separate the data points across the
horizontal axis. Custom colors used to represent each team come from
their official team colors, taken from [this helpful
website](https://teamcolorcodes.com/nhl-team-color-codes/).

``` r
g <- ggplot(data=seasonResultsCentralRegular, aes(x = as.factor(franchiseId), y = winPercent, 
                                                  color = as.factor(franchiseId)))
g + geom_boxplot(color = "black", outlier.shape = NA) + 
  geom_point(position = "jitter") + 
  labs(title = "NHL Central Teams Win Percentages for Each Regular Season",
       x = "", y = "Regular Season Win Percentage") + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_x_discrete(labels=c("11" = "Chicago\nBlackhawks",
                            "15" = "Dallas\nStars",
                            "18" = "St. Louis\nBlues",
                            "27" = "Colorado\nAvalanche",
                            "34" = "Nashville\nPredators",
                            "35" = "Winnipeg\nJets",
                            "37" = "Minnesota\nWild")) +
  scale_color_manual(values=c("#CF0A2C","#006847","#002F87","#6F263D","#FFB81C","#041E42","#154734"))
```

![](YunJi_Project1_files/figure-gfm/season%20boxplot-1.png)<!-- -->

With the longest history, the Chicago Blackhawks experienced the widest
dispersion of seasonal outcomes and the boxplot reflects this. Newer
teams such as the Minnesota Wild and Winnipeg Jets have played fewer
seasons, thus have fewer data points and somewhat less dispersion. In
the regular season, the Colorado Avalanche and St. Louis Blues have
enjoyed the greatest levels of success, with the highest median win
percentages.

### Querying Player-Level Data

I also explored player-level data from the `skaterRecords` and
`goalieRecords` endpoints in the NHL API. Again, I downloaded data for
teams from the Central Division, then concatenated them into a single
data frame for use.

``` r
skaterRecordsBlues <- runNhlApi(baseURL, endpoints, "skaterRecords", 18)
skaterRecordsStars <- runNhlApi(baseURL, endpoints, "skaterRecords", 15)
skaterRecordsWild <- runNhlApi(baseURL, endpoints, "skaterRecords", 37)
skaterRecordsPredators <- runNhlApi(baseURL, endpoints, "skaterRecords", 34)
skaterRecordsJets <- runNhlApi(baseURL, endpoints, "skaterRecords", 35)
skaterRecordsAvalanche <- runNhlApi(baseURL, endpoints, "skaterRecords", 27)

skaterRecordsCentral <- rbind(skaterRecordsBlackhawks,
                              skaterRecordsBlues,
                              skaterRecordsStars,
                              skaterRecordsWild,
                              skaterRecordsPredators,
                              skaterRecordsJets,
                              skaterRecordsAvalanche)

goalieRecordsBlues <- runNhlApi(baseURL, endpoints, "goalieRecords", 18)
goalieRecordsStars <- runNhlApi(baseURL, endpoints, "goalieRecords", 15)
goalieRecordsWild <- runNhlApi(baseURL, endpoints, "goalieRecords", 37)
goalieRecordsPredators <- runNhlApi(baseURL, endpoints, "goalieRecords", 34)
goalieRecordsJets <- runNhlApi(baseURL, endpoints, "goalieRecords", 35)
goalieRecordsAvalanche <- runNhlApi(baseURL, endpoints, "goalieRecords", 27)

goalieRecordsCentral <- rbind(goalieRecordsBlackhawks,
                              goalieRecordsBlues,
                              goalieRecordsStars,
                              goalieRecordsWild,
                              goalieRecordsPredators,
                              goalieRecordsJets,
                              goalieRecordsAvalanche)

playerRecordsCentral <- rbind(skaterRecordsCentral %>% select(franchiseName, positionCode), 
                              goalieRecordsCentral %>% select(franchiseName, positionCode))
```

### Displaying Player Data

Since the `positionCode` column codes player positions as single-letter
codes, I created an additional field to provide the full names for each
position, then displayed the counts of player positions in a contingency
table (using `rowSums` to create a new variable for `Total Players`) and
a stacked barplot.

``` r
playerRecordsCentral <- playerRecordsCentral %>%
  mutate(positionName = "Unknown") %>%
  mutate(positionName = ifelse(positionCode == "C", "Center", positionName),
         positionName = ifelse(positionCode == "D", "Defenseman", positionName),
         positionName = ifelse(positionCode == "L", "Left Wing", positionName),
         positionName = ifelse(positionCode == "R", "Right Wing", positionName),
         positionName = ifelse(positionCode == "G", "Goaltender", positionName))

cont_table <- table(playerRecordsCentral$franchiseName, playerRecordsCentral$positionName)
cont_table <- cbind(cont_table, "Total Players" = rowSums(cont_table))
kable(cont_table, caption = "NHL Central Division Teams - Number of Players by Position")
```

|                     | Center | Defenseman | Goaltender | Left Wing | Right Wing | Total Players |
| ------------------- | -----: | ---------: | ---------: | --------: | ---------: | ------------: |
| Chicago Blackhawks  |    214 |        283 |         48 |       199 |        168 |           912 |
| Colorado Avalanche  |    117 |        172 |         34 |       102 |        102 |           527 |
| Dallas Stars        |    152 |        204 |         37 |       134 |        125 |           652 |
| Minnesota Wild      |     75 |         73 |         14 |        49 |         47 |           258 |
| Nashville Predators |     68 |         87 |         10 |        61 |         54 |           280 |
| St. Louis Blues     |    143 |        215 |         42 |       121 |        127 |           648 |
| Winnipeg Jets       |     67 |         98 |         17 |        63 |         46 |           291 |

NHL Central Division Teams - Number of Players by Position

``` r
g <- ggplot(data = playerRecordsCentral, aes(x = franchiseName, fill = positionName))
g + geom_bar() +
  scale_y_continuous(limits = c(0,1000), breaks = seq(0,1200,200), expand = c(0,0)) +
  scale_fill_manual(name = "Player Position",
                    values = c("darkgrey","darkgreen","darkblue","darkred","darkorange")) +
  scale_x_discrete(labels = c("Chicago Blackhawks" = "Chicago\nBlackhawks",
                              "Colorado Avalanche" = "Colorado\nAvalanche",
                              "Dallas Stars" = "Dallas\nStars",
                              "Minnesota Wild" = "Minnesota\nWild",
                              "Nashville Predators" = "Nashville\nPredators",
                              "St. Louis Blues" = "St. Louis\nBlues",
                              "Winnipeg Jets" = "Winnipeg\nJets")) +
  labs(title = "NHL Central Teams: Number of Players at Each Position",
       x = "", y = "Number of Players") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](YunJi_Project1_files/figure-gfm/player%20positions-1.png)<!-- -->

Trends here are consistent with other plots in this report: the
Blackhawks, with the longest history, have had the greatest number of
players with 912. The three youngest teams, the Minnesota Wild,
Nasheville Predators and Winnipeg Jets have the lowest player counts.
Nasheville’s 10 goaltenders represent the lowest count in the table,
thanks to the longtime play of goalie Pekka Rinne.

## Blog

This was an interesting project that coalesced many of the data
ingestion, exploration, wrangling and visualization methods we have thus
far learned in ST 558. The NHL Statistics API was free and easy to use,
making it quite a good resource for applying the materials learned.
Although it did not have all data (I wanted to make a chart of Stanley
Cup winners but could not find the appropriate data), there were plenty
to use for a variety of tables and plots.

A difficult part of the project for me was in setting up the functions
used to query the API and parse the data. Initially I tried to do
everything in one function (not good programming practice) but
underestimated its complexity. I then reconsidered, and sketched out on
a sheet of paper an outline of key components of the function, before
implementing them in R in a methodical way. This enabled me to become
better organized in my thinking, and made the programming part easier as
well. I will keep up this practice going forward.

Another new experience was developing the project on Github from the
RStudio IDE. This was less of a learning curve since I’ve used Git
before, albeit only from a CLI, and the obstacles faced here was mostly
setting up Git and syncing it with the IDE. Once up and running, I found
the commit/pull/push process fairly easy.

My takeaway from the project is that it got me more practice parsing
JSON objects, and quite a lot of hands-on experience with `ggplot2`. I
was able to use less common plot features such as custom hexadecimal
colors, and in reading through the documentation I realized how much
flexibility and user options existed in this one package. I don’t have a
lot of experience in data visualization, and it’s a topic I will be
exploring further outside the scope of this course.

## My GitHub Project Repository

<https://github.com/yji26/ST558-Project1>
