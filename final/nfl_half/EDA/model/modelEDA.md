Getting a prediction model for 2012
===================================





```r
## Load data
load("../../data/process/first.Rdata")
load("../../data/process/second.Rdata")
load("../../data/process/full.Rdata")
load("../../data/process/gameFirst.Rdata")
load("../../data/process/gameFull.Rdata")

## Fix names
names(gameFirst) <- names(first)
names(gameFull) <- names(full)

## Specify data to use when to train the model
gameFi2011 <- do.call(rbind, gameFirst[as.character(2006:2011)])

## Load libraries
```

