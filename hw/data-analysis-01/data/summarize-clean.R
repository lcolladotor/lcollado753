## Some useful commands
# qrsh -l mem_free=15G,h_vmem=20G
# R --min-vsize=3G --min-nsize=10M

## Load libraries
library(IRanges)

## Load data
load("clean.Rdata")


## Have to consider State too, not only County
if(FALSE) {
	length(table(paste(x$State, x$County, x$Site, sep="-")))
	# 1217
	length(table(paste(x$County, x$Site, sep="-")))
	# 1198
}

## Construct my main summary function
mySummary <- function(data, bySite = TRUE, verbose = TRUE) {
	result <- lapply(data, function(x) {
		if(verbose) print(date())
		## Extract month information
		month <- substring(x$Date, 1, 6)
	
		## Combine variables of interest
		if(bySite) {
			comb <- paste(x$State, x$County, x$Site, month, sep="|")
		} else {
			comb <- sort(paste(x$State, x$County, month, sep="|"))
		}
		
	
		## Find the combinations that are split
		comb.split.ir <- splitRanges(comb)
		find.split <- table(runValue(comb))
		split.flag <- sum(find.split > 1) > 0
		if(verbose) print(split.flag)
		if(split.flag) {
			find.split.names <- names(find.split[find.split > 1])
			find.split.i <- which(names(comb.split.ir) %in% find.split.names)
		
			## Collect data for those combinations that are split
			split.l <- lapply(comb.split.ir[find.split.i], function(y) {
				x$Value[ y ]
			})
		}
		
	
		## Collect data for the combinations that are not split
		comb.ir <- successiveIRanges(runLength(comb))
		names(comb.ir) <- runValue(comb)
		if(split.flag) {
			comb.ir <- comb.ir[ !names(comb.ir) %in% find.split.names, ]
		}
		values <- Views(x$Value, comb.ir)
	
		## Get means (etc) by combination of interest
		if(verbose) print("Calculating means (etc) of interest")
		if(split.flag) {
			means <- c(viewMeans(values), unlist(lapply(split.l, mean)))
			se <- c(viewApply(values, sd), unlist(lapply(split.l, mean)))
			q1 <- c(viewApply(values, function(x) { quantile(x, 0.25) }), unlist(lapply(split.l, quantile, 0.25)))
			q3 <- c(viewApply(values, function(x) { quantile(x, 0.75) }), unlist(lapply(split.l, quantile, 0.75)))
		} else{
			means <- viewMeans(values)
			se <- viewApply(values, sd)
			q1 <- viewApply(values, function(x) { quantile(x, 0.25) })
			q3 <- viewApply(values, function(x) { quantile(x, 0.75) })
		}
	
	
		## Separate values of interest
		if(verbose) print("Separating values of interest")
		if(split.flag) {
			to.split <- c(names(comb.ir), names(split.l))
		} else {
			to.split <- names(comb.ir)	
		}
	
		comb.split <- strsplit(to.split, "\\|")
		state <- unlist(lapply(comb.split, function(x) { x[1] }))
		county <- unlist(lapply(comb.split, function(x) { x[2] }))
		if(bySite) {
			site <- unlist(lapply(comb.split, function(x) { x[3] }))
			month2 <- unlist(lapply(comb.split, function(x) { x[4] }))
		} else {
			month2 <- unlist(lapply(comb.split, function(x) { x[3] }))
		}		
		month3 <- as.Date(paste(month2,"15", sep="/"), format="%Y%m/%d")
	
		if(bySite) {
			res <- data.frame(State = state, County = county, Site = site, Month = month3, Mean.Value = means, row.names = 1:length(state), SE = se, Q1 = q1, Q3 = q3)
		} else {
			res <- data.frame(State = state, County = county, Month = month3, Mean.Value = means, row.names = 1:length(state), SE = se, Q1 = q1, Q3 = q3)
		}
		
		res <- res[order(res$Month), ]
		return(res)
	})
	
	types <- list("PM" = grep("88101", names(data)), "Ozone" = grep("44201", names(data)))
	final <- lapply(types, function(x) {
		do.call(rbind, result[x])
	})	
	return(final)
}

## Proceed to summarize the data

## Means by State-County-Site-Month
month.site <- mySummary(clean, bySite = TRUE)
## Means by State-County-Month
month.county <- mySummary(clean, bySite = FALSE)

## Check object sizes
print(object.size(month.site), units="Mb")
print(object.size(month.county), units="Mb")

## Save files
save(month.site, file="month-site.Rdata", compress="gzip")
save(month.county, file="month-county.Rdata", compress="gzip")