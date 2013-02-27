### Scrape data from Tumblr and WP. Mainly date, author and title.

## Load libraries
library(XML)


### Tumblr. Pages go back to 46

## tumblrScrap scrapes the tumblr data for date, author, title and type of post
tumblrScrap <- function(i) {
	## Define url
	if(i == 1) {
		url  <- "http://simplystatistics.tumblr.com/"
	} else {
		url <- paste0("http://simplystatistics.tumblr.com/page/", i)
	}
	
	## Access site
	tumblr <- htmlTreeParse(url, useInternalNodes=TRUE)

	## Extract date and author
	date.author <- gsub("(^\\s+)|(\\s+$)|(\r\n)", "", xpathSApply(tumblr, "//div[@class='date']", xmlValue))
	date.author <- strsplit(date.author, " - Author: ")
	date <- unlist(lapply(date.author, function(x) { x[1] }))
	date <- as.Date(date, format="%B %d, %Y")
	author <- unlist(lapply(date.author, function(x) { x[2] }))


	## Extract titles
	content <- xpathSApply(tumblr, "//div[@class='post-content']", xmlValue)
	title <- gsub("^\\s+", "", unlist(lapply(strsplit(content, "\r\n"), function(x) {x[4]} )))
	if(sum(nchar(title) == 0) > 0) {
		title[nchar(title) == 0] <- NA
	}

	## Type of post
	type <- gsub("(^\\s+)|(\\s+$)|(\r\n)", "", xpathSApply(tumblr, "//div[@class='type']", xmlValue))
	
	## End
	data.frame(date, author, title, type, stringsAsFactors=FALSE)
}

## Test the function
if(FALSE){
	do.call(rbind, lapply(1:2, tumblrScrap))
}

## Run for all tumblr posts
tumblr <- do.call(rbind, lapply(1:46, tumblrScrap))


### Wordpress, 51 pages

## Function that extracts date, author and title (no type specified in wp!)
wpScrap <- function(i) {
	## Define url
	if(i == 1) {
		url  <- "http://simplystatistics.org/"
	} else {
		url <- paste0("http://simplystatistics.org/page/", i, "/")
	}
	
	## Access site
	wp <- htmlTreeParse(url, useInternalNodes=TRUE)
	
	## Extract date, author and title
	date <- as.Date(xpathSApply(wp, "//span[@class='entry-date']", xmlValue), format="%B %d, %Y")
	author <- xpathSApply(wp, "//span[@class='author vcard']", xmlValue)
	title <- xpathSApply(wp, "//h2[@class='entry-title']", xmlValue)
	if(sum(nchar(title) == 0) > 0) {
		title[nchar(title) == 0] <- NA
	}
	
	## End
	data.frame(date, author, title, stringsAsFactors=FALSE)
}

## Test the function
if(FALSE){
	do.call(rbind, lapply(1:2, wpScrap))
}

## Run for all tumblr posts
wp <- do.call(rbind, lapply(1:51, wpScrap))

## Switch names in tumblr to long versions used in WP
tumblr$author <- ifelse(tumblr$author == "leekgroup", "Jeff Leek", ifelse(tumblr$author == "rafalab", "Rafael Irizarry", "Roger Peng"))

## Save information
date.downloaded <- date()
save(tumblr, wp, date.downloaded, file="info.Rdata", compress="gzip")