Project 1
================
Yun Ji
6/12/2020

  - [Query API Functions](#query-api-functions)
  - [Query the Data from the NHL API](#query-the-data-from-the-nhl-api)
  - [Plotting Data](#plotting-data)
      - [History and Winning Percentage of NHL
        Franchises](#history-and-winning-percentage-of-nhl-franchises)
      - [Season Results for Central
        Teams](#season-results-for-central-teams)

## Query API Functions

To query the NHL API, I first set up the variables and data structures
needed as inputs to my API query functions. The base URL is stored as a
character string, and the individual API endpoints are stored in a list
called `endpoints` - this is to help future-proof expanding the queries
to include other API endpoints later on.

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
valid returns the endpoint URL as the output.

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
for franchise ID.

``` r
getDataNoOptions <- function(fullURL) {
  rawData <- fromJSON(content(GET(fullURL), "text"), flatten=TRUE)
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
  rawData <- fromJSON(content(GET(fullURL), "text"), flatten=TRUE)
  return(tbl_df(rawData$data))
}
```

Finally, the wrapper function `runNhlApi` acts as an orchestrator and
handles the top-down logic of getting data from the NHL API. The data is
returned as a `tibble` table. Note that unnamed arguments are used in
`runNhlApi` to handle the additional user input needed by
`getDataByFranchiseId`.

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

## Query the Data from the NHL API

I run my functions with the appropriate user inputs to query the needed
data from the NHL API. For the franchise-specific data, I picked the
Blackhawks since I am from Chicago.

``` r
franchise <- runNhlApi(baseURL, endpoints, "franchise")
teamTotals <- runNhlApi(baseURL, endpoints, "teamTotals")
seasonRecordsBlackhawks <- runNhlApi(baseURL, endpoints, "seasonRecords", 11)
goalieRecordsBlackhawks <- runNhlApi(baseURL, endpoints, "goalieRecords", 11)
skaterRecordsBlackhawks <- runNhlApi(baseURL, endpoints, "skaterRecords", 11)
```

## Plotting Data

### History and Winning Percentage of NHL Franchises

For my first plot, I filtered the list of franchises to just the active
franchises, then created a scatterplot of their regular season win
percentage vs. the starting year of the franchise. I also created the
variable `nation` to differentiate Canadian and U.S. teams, and
color-coded the data points of the scatterplot by nation.

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
    labs(title = "NHL Active Teams Regular Season Win Percentage vs. Starting Year",
         x = "Starting Year", y = "Win Percentage",
         color = "Nation", alpha = "") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values = c("red","blue"))
```

![](YunJi_Project1_files/figure-gfm/scatterplot%20-%20Win%20Percent%20vs.%20Starting%20Year-1.png)<!-- -->

From the plot, we can see that there are many more U.S. teams than
Canadian teams, which makes sense as the U.S. is more populous. In
addition, we see that there is a big gap in starting year where no
active franchises began operations between the early 1930s and the mid
1960s - a long period of separation between the “Original Six” NHL teams
and the newer teams added in the NHL’s aggressive expansion from the
late 1960s onwards (there were actually a few NHL teams that were
founded during this interim period, however they ceased operations and
are therefore not included among active franchises). Most teams’
lifetime winning percentage hover around 50% as one might expect, but we
also see more variability as we get closer to the present day.

### Season Results for Central Teams

From the NHL API, I’ve also queried other endpoints to obtain the
season-by-season results of individual teams. To do this, I added the
new endpoint to my `list` of endpoints:

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

At this point some data exploration is in order. If a team has won
enough games to reach the playoffs in a given season, its playoff
results will show up in the `seriesTitle` column of the table (if a team
did not make the playoffs, `seriesTitle` is simply `NA`). Using this
fact, I created a contingency table showing the counts playoff
appearances for each Central Division team since the year 2000.

``` r
playoffAppearancesSince2000 <- seasonResultsCentralRegular %>% 
  filter(as.numeric(substr(seasonId,1,4)) >= 2000)

cont_table <- table(playoffAppearancesSince2000$teamName, playoffAppearancesSince2000$seriesTitle)
kable(cont_table, caption="NHL Central Playoff Appearances since 2000")
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
kable(cont_table, caption="NHL Central Playoff Appearances since 2000 (improved)")
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

This table looks better than before. While the St. Louis Blues have
reached the playoffs 12 times since 2000, the Chicago Blackhawks took
the most advantage of their playoff appearances, reaching (and
winning\!) the Stanley Cup Final three times in the last 20 years. The
Atlanta Thrashers/Winnipeg Jets have experienced the most futility,
reaching the playoffs only four times and getting past the 1st round
only once.
