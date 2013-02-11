## Some useful commands
# qrsh -l mem_free=15G,h_vmem=20G
# R --min-vsize=3G --min-nsize=10M

## Load libraries
library(IRanges)

## Load data
load("pre-small.Rdata")
load("raw-basic.Rdata")

## Read in units information
units <- read.csv("Units.csv", header=TRUE, skip=1, colClasses=rep("character", 2))

## Find which units are being used
units.used <- lapply(pre, function(x) {
	table(x$Unit)
})
units.used
## 105 and 118 are used in the PM data files. Well, only 16 entries in total use 118.
## For ozone, it's split nearly 50-50% between 007 and 008. 040 is used much less frequently.

## Meaning of units that are used
units.uni <- unique(unlist(lapply(units.used, function(x) { names(x)})))
subset(units, Unit %in% units.uni)
#    Unit                   Unit.Desc
#7    007           Parts per million
#8    008           Parts per billion
#40   040       Parts per 100 million
#104  105 Micrograms/cubic meter (LC)
#117  118            Liters/minute LC

## Liters/minute and Micrograms/cubic meter seem incompatible to me!

## Read in State codes
states <- read.csv("StateCountyCodes.csv", skip=1, colClasses="character")

states.used <- lapply(pre, function(x) {
	table(x$State)
})
## Checking that all State Codes are in the reference table
states.uni <- unique(unlist(lapply(states.used, function(x) { names(x)}))) 
states.uni %in% states$State.Code
## All good :)

county.used <- lapply(pre, function(x) {
	table(x$County)
})
## Checking that all State Codes are in the reference table
county.uni <- unique(unlist(lapply(county.used, function(x) { names(x)}))) 
county.uni %in% states$County.Code
## All good :)

## Are County and State codes unique?
tapply(states$State.Code, states$State.Name, table)
## State codes look good :)
tapply(states$County.Code, states$State.Name, table)
## County codes are re-used in different states!!
states$State.County <- paste(states$State.Code, states$County.Code, sep="-")

## Construct my own matching function since match() doesn't return the names that I would want to
myMatch <- function(x, match) {
	new.vals <- sapply(runValue(x), function(y) {
		names(match)[which(match == y)]
	})
	Rle(new.vals, runLength(x))
}

## Construct vectors for myMatch
state.m <- unique(states$State.Code)
names(state.m) <- sapply(state.m, function(x) { states$State.Abbr[ which(states$State.Code == x)[1]] })
county.m <- unique(states$State.County)
names(county.m) <- sapply(county.m, function(x) { states$County.Name[ which(states$State.County == x)[1]] })

## Create a clean version of the data
# For now, I'll ignore the columns POC, Method, Start.Time and Null
# Added Site back because Jean-Philippe has code to use it to map the location of the monitor in a map
clean <- DataFrameList(lapply(pre, function(input) {
	
	## Remove those few entries that have Unit 118. Also remove entries with NA Value
	x <- input[input$Unit != "118" & !is.na(input$Value),]	
	
	## Adjusting PM at parts per million
	for(unit in c("008", "040")) {
		idx <- x$Unit == unit
		if(sum(idx) > 0) {
			if(unit == "008") {
				## Going from parts per billion to parts per million
				x$Value[idx] <- x$Value[idx] / 1000
			} else{
				## Going from parts per 100 million to parts per million
				x$Value[idx] <- x$Value[idx] / 100
			}
		}
	}
	
	## Construct the clean DataFrame
	res <- DataFrame(State = myMatch(x$State, state.m), 
		County = myMatch(paste(x$State, x$County, sep="-"), county.m),
		Site = x$Site,
		Date = x$Date,
		Value = x$Value
	)
	
	## Done
	return(res)
}))

print(object.size(clean), units="Mb")
# 897.1 Mb

## Save the clean data
save(clean, file = "clean.Rdata", compress="gzip") ## bzip2 was 69M but took forever to compress
