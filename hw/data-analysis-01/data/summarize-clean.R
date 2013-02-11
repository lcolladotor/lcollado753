## Some useful commands
# qrsh -l mem_free=15G,h_vmem=20G
# R --min-vsize=3G --min-nsize=10M

## Load libraries
library(IRanges)

## Load data
load("clean.Rdata")

## Proceed to summarize the data


## Have to consider State too, not only County
if(FALSE) {
	length(table(paste(x$State, x$County, x$Site, sep="-")))
	# 1217
	length(table(paste(x$County, x$Site, sep="-")))
	# 1198
}

## Means by State-County-Site-Month
Site <- lapply(clean, function(x) {
	## Extract month information
	month <- substring(x$Date, 1, 6)
	
	## Combine variables of interest
	comb <- paste(x$State, x$County, x$Site, month, sep="|")
	
	## Get means by combination of interest
	# This step is slow....
	means <- tapply(x$Value, comb, mean, na.rm = FALSE)
	se <- tapply(x$Value, comb, sd, na.rm = FALSE)
	
	## Separate values of interest
	comb.split <- strsplit(names(means), "\\|")
	state <- unlist(lapply(comb.split, function(x) { x[1] }))
	county <- unlist(lapply(comb.split, function(x) { x[2] }))
	site <- unlist(lapply(comb.split, function(x) { x[3] }))
	month2 <- unlist(lapply(comb.split, function(x) { x[4] }))
	month3 <- as.Date(paste(month2,"15", sep="/"), format="%Y%m/%d")
	
	res <- data.frame(State = state, County = county, Site = site, Month = month3, Mean.Value = means, row.names = 1:length(state), SE = se)
	
	return(res)
})

## Means by State-County-Month
County <- lapply(clean, function(x) {
	## Extract month information
	month <- substring(x$Date, 1, 6)
	
	## Combine variables of interest
	comb <- paste(x$State, x$County, month, sep="|")
	
	## Get means by combination of interest
	# This step is slow....
	means <- tapply(x$Value, comb, mean, na.rm = FALSE)
	se <- tapply(x$Value, comb, sd, na.rm = FALSE)
	
	## Separate values of interest
	comb.split <- strsplit(names(means), "\\|")
	state <- unlist(lapply(comb.split, function(x) { x[1] }))
	county <- unlist(lapply(comb.split, function(x) { x[2] }))
	month2 <- unlist(lapply(comb.split, function(x) { x[3] }))
	month3 <- as.Date(paste(month2,"15", sep="/"), format="%Y%m/%d")
	
	res <- data.frame(State = state, County = county, Month = month3, Mean.Value = means, row.names = 1:length(state), SE = se)
	
	return(res)
})

print(object.size(Site), units="Mb")
print(object.size(County), units="Mb")

## Save files
save(Site, file="Site.Rdata", compress="gzip")
save(County, file="County.Rdata", compress="gzip")