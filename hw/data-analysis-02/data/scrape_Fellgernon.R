### Scrape data from Tumblr (Fellgernon Bit and Fellger Byte). Only for the date.

## Load libraries
library(XML)


## tumblrScrap scrapes the tumblr data for date
tumblrScrap <- function(i, main, page) {
	## Define url
	if(i == 1) {
		url  <- main
	} else {
		url <- paste0(page, i)
	}
	
	## Access site
	tumblr <- htmlTreeParse(url, useInternalNodes=TRUE)

	## Extract date 
	month <- xpathSApply(tumblr, "//span[@class='month']", xmlValue)
	day <- xpathSApply(tumblr, "//span[@class='day']", xmlValue)
	ago <- xpathSApply(tumblr, "//div[@class='postmeta']", xmlValue)
	r <- regexpr("[Posted|Reblogged](.*?)ago", ago)
	time.ago <- gsub("(Posted )|(Reblogged )|( ago)", "", regmatches(ago, r))
	getyear <- function(x, y) {
		months <- paste(2:11, "months")
		if(x %in% months) {
			"2012"
		} else if (x == "1 year"){
			if(y %in% c("Jan", "Feb")) {
				"2012"
			} else {
				"2011"
			}
		} else {
			"2013"
		}
	}
	year <- mapply(getyear, time.ago, month)
	date <- paste(year, month, day, sep="-")
	date <- as.Date(date, format="%Y-%b-%d")
	
	## End
	data.frame(date, stringsAsFactors=FALSE)
}

## Test the function
if(FALSE){
	do.call(rbind, lapply(1:2, function(i) { tumblrScrap(i, main = "http://fellgernon.tumblr.com/", page = "http://fellgernon.tumblr.com/page/") }))
	do.call(rbind, lapply(1:2, function(i) { tumblrScrap(i, main = "http://fellger.tumblr.com/", page = "http://fellger.tumblr.com/page/") }))
}

## Run for all Fellgernon Bit posts # 18 pages
fellgernon <- do.call(rbind, lapply(1:18, function(i) { tumblrScrap(i, main = "http://fellgernon.tumblr.com/", page = "http://fellgernon.tumblr.com/page/") }))

## Now for Fellger Byte posts # 9 pages
fellger <- do.call(rbind, lapply(1:9, function(i) { tumblrScrap(i, main = "http://fellger.tumblr.com/", page = "http://fellger.tumblr.com/page/") }))

## Remove the Fellger Byte exclusive posts
dates <- c(fellgernon$date, fellger[fellger$date <= as.Date("2012-11-08"), ])
dates <- sort(dates)


## Load visits data to Fellgernon.tumblr.com from Google Analytics
visit <- read.csv("Analytics fellgernon.tumblr.com Audience Overview 20111119-20130227.csv", skip=6, header=TRUE, colClasses=c("character", "character") )
## Remove last line which has the total
visit <- visit[-nrow(visit), ]
visit$Visits <- as.numeric(visit$Visits)
visit$Day <- as.Date(visit$Day, format="%m/%d/%y")

## Final version
fell <- visit
colnames(fell) <- c("date", "visits")
fell$post <- visit$Day %in% dates

## Save information
date.downloaded <- date()
save(fell, date.downloaded, file="fell.Rdata", compress="gzip")

