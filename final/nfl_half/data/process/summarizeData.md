Goal: extract variables to be used when modelling
=================================================






```r
## Load data directory information
load("../download/data-info.Rdata")
source("processingFunctions.R")

## Get the file names
files <- list.files(dataDir, pattern = "csv", full.names = TRUE)
names(files) <- substr(gsub(".*/", "", files), 1, 4)

## Read data
data <- lapply(files, readPBP)
```

```
## Reading file /Users/lcollado/Google Drive/NFL/2002_nfl_pbp_data.csv
```

```
## Classifying play types
```

```
## Finding passing, rushing and penalty yards
```

```
## Reading file /Users/lcollado/Google Drive/NFL/2003_nfl_pbp_data.csv
```

```
## Classifying play types
```

```
## Finding passing, rushing and penalty yards
```

```
## Reading file /Users/lcollado/Google Drive/NFL/2004_nfl_pbp_data.csv
```

```
## Classifying play types
```

```
## Finding passing, rushing and penalty yards
```

```
## Reading file /Users/lcollado/Google Drive/NFL/2005_nfl_pbp_data.csv
```

```
## Classifying play types
```

```
## Finding passing, rushing and penalty yards
```

```
## Reading file /Users/lcollado/Google Drive/NFL/2006_nfl_pbp_data.csv
```

```
## Classifying play types
```

```
## Finding passing, rushing and penalty yards
```

```
## Reading file /Users/lcollado/Google Drive/NFL/2007_nfl_pbp_data.csv
```

```
## Classifying play types
```

```
## Finding passing, rushing and penalty yards
```

```
## Reading file /Users/lcollado/Google Drive/NFL/2008_nfl_pbp_data.csv
```

```
## Classifying play types
```

```
## Finding passing, rushing and penalty yards
```

```
## Reading file /Users/lcollado/Google Drive/NFL/2009_nfl_pbp_data.csv
```

```
## Classifying play types
```

```
## Finding passing, rushing and penalty yards
```

```
## Reading file /Users/lcollado/Google Drive/NFL/2010_nfl_pbp_data.csv
```

```
## Classifying play types
```

```
## Finding passing, rushing and penalty yards
```

```
## Reading file /Users/lcollado/Google Drive/NFL/2011_nfl_pbp_data.csv
```

```
## Classifying play types
```

```
## Finding passing, rushing and penalty yards
```

```
## Reading file /Users/lcollado/Google
## Drive/NFL/2012_nfl_pbp_data_reg_season.csv
```

```
## Classifying play types
```

```
## Finding passing, rushing and penalty yards
```

```r

## Check how big it is in mem
print(object.size(data), units = "Mb")
```

```
## 134.5 Mb
```

```r

## Save data (12.4 mb)
save(data, file = "data.Rdata", compress = "gzip")
```



Now that I have finally read in the data, I can now calculate the team stats per year



```r
## List of teams
teams <- sort(unique(data[["2012"]]$off))[-1]

## Get the team stats for all teams in all years, first half only
first <- lapply(data, function(df) {
    res <- lapply(teams, function(team) {
        getTeamStats(team, df, half = "first")
    })
    res <- do.call(rbind, res)
})

## Now do the same for the second half only
second <- lapply(data, function(df) {
    res <- lapply(teams, function(team) {
        getTeamStats(team, df, half = "second")
    })
    res <- do.call(rbind, res)
})


## Finally, for both halfs
full <- lapply(data, function(df) {
    res <- lapply(teams, function(team) {
        getTeamStats(team, df, half = "full")
    })
    res <- do.call(rbind, res)
})

## Save team summaries
save(first, file = "first.Rdata")
save(second, file = "second.Rdata")
save(full, file = "full.Rdata")
```


Next, I have to summarize the data by game


```r
## Build data sets that will be used to train the model
gameFirst <- lapply(names(first), function(year) {
    print(paste("Processing year", year))
    gameSummary(first[[year]], data[[year]])
})
```

```
## [1] "Processing year 2002"
```

```
## Game 20020922_CAR@MIN has no 2nd quarter data
```

```
## Game 20020922_CAR@MIN has no 2nd quarter data
```

```
## [1] "Processing year 2003"
```

```
## Game 20040110_CAR@STL has no information
```

```
## Game 20040110_CAR@STL has no information
```

```
## Game 20040118_IND@NE has no information
```

```
## Game 20040118_IND@NE has no information
```

```
## [1] "Processing year 2004"
```

```
## Game 20040926_GB@IND has no information
```

```
## Game 20040926_GB@IND has no information
```

```
## [1] "Processing year 2005"
```

```
## Game 20051023_NO@STL has no information
```

```
## Game 20051023_NO@STL has no information
```

```
## Game 20051023_DET@CLE has no 2nd quarter data
```

```
## Game 20051023_DET@CLE has no 2nd quarter data
```

```
## [1] "Processing year 2006"
```

```
## Game 20061126_CHI@NE has no information
```

```
## Game 20061126_CHI@NE has no information
```

```
## Game 20061127_GB@SEA has no information
```

```
## Game 20061127_GB@SEA has no information
```

```
## [1] "Processing year 2007"
```

```
## Game 20070930_HOU@ATL has no 2nd quarter data
```

```
## Game 20070930_HOU@ATL has no 2nd quarter data
```

```
## Game 20071104_CIN@BUF has no 2nd quarter data
```

```
## Game 20071104_CIN@BUF has no 2nd quarter data
```

```
## [1] "Processing year 2008"
```

```
## Game 20081102_NYJ@BUF has no 2nd quarter data
```

```
## Game 20081102_NYJ@BUF has no 2nd quarter data
```

```
## [1] "Processing year 2009"
## [1] "Processing year 2010"
```

```
## Game 20101125_CIN@NYJ has no 2nd quarter data
```

```
## Game 20101125_CIN@NYJ has no 2nd quarter data
```

```
## [1] "Processing year 2011"
## [1] "Processing year 2012"
```

```r
print(object.size(gameFirst), units = "Mb")
```

```
## 1.2 Mb
```

```r

gameFull <- lapply(names(full), function(year) {
    print(paste("Processing year", year))
    gameSummary(full[[year]], data[[year]])
})
```

```
## [1] "Processing year 2002"
```

```
## Game 20020922_CAR@MIN has no 2nd quarter data
```

```
## Game 20020922_CAR@MIN has no 2nd quarter data
```

```
## [1] "Processing year 2003"
```

```
## Game 20040110_CAR@STL has no information
```

```
## Game 20040110_CAR@STL has no information
```

```
## Game 20040118_IND@NE has no information
```

```
## Game 20040118_IND@NE has no information
```

```
## [1] "Processing year 2004"
```

```
## Game 20040926_GB@IND has no information
```

```
## Game 20040926_GB@IND has no information
```

```
## [1] "Processing year 2005"
```

```
## Game 20051023_NO@STL has no information
```

```
## Game 20051023_NO@STL has no information
```

```
## Game 20051023_DET@CLE has no 2nd quarter data
```

```
## Game 20051023_DET@CLE has no 2nd quarter data
```

```
## [1] "Processing year 2006"
```

```
## Game 20061126_CHI@NE has no information
```

```
## Game 20061126_CHI@NE has no information
```

```
## Game 20061127_GB@SEA has no information
```

```
## Game 20061127_GB@SEA has no information
```

```
## [1] "Processing year 2007"
```

```
## Game 20070930_HOU@ATL has no 2nd quarter data
```

```
## Game 20070930_HOU@ATL has no 2nd quarter data
```

```
## Game 20071104_CIN@BUF has no 2nd quarter data
```

```
## Game 20071104_CIN@BUF has no 2nd quarter data
```

```
## [1] "Processing year 2008"
```

```
## Game 20081102_NYJ@BUF has no 2nd quarter data
```

```
## Game 20081102_NYJ@BUF has no 2nd quarter data
```

```
## [1] "Processing year 2009"
## [1] "Processing year 2010"
```

```
## Game 20101125_CIN@NYJ has no 2nd quarter data
```

```
## Game 20101125_CIN@NYJ has no 2nd quarter data
```

```
## [1] "Processing year 2011"
## [1] "Processing year 2012"
```

```r
print(object.size(gameFull), units = "Mb")
```

```
## 1.2 Mb
```

```r

## Save information
save(gameFirst, file = "gameFirst.Rdata", compress = "gzip")
save(gameFull, file = "gameFull.Rdata", compress = "gzip")
```





