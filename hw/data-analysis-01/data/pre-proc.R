#### Download data

## Construct urls
urlPM <- paste("http://www.epa.gov/ttn/airs/airsaqs/detaildata/501files/RD_501_88101_", 2003:2012, "[1].zip", sep="")
urlOzone <- paste("http://www.epa.gov/ttn/airs/airsaqs/detaildata/501files/RD_501_44201_", 2003:2012, "[1].zip", sep="")
## 2004 has a slightly different name
urlOzone[2] <- "http://www.epa.gov/ttn/airs/airsaqs/detaildata/501files/Rd_501_44201_2004.Zip"

## Construct file names
url <- data.frame(url = c(urlPM, urlOzone), stringsAsFactors = FALSE)
m <- regexec("files/(.+[Z|z]ip)$", url$url)
url$file <- unlist(lapply(regmatches(url$url, m), function(x) { x[2] }))
url$type <- rep(c("PM", "Ozone"), each =10)
url$year <- rep(2003:2012, 2)
url$file[12] <- "RD_501_44201_2004.zip"

## Download the zip files
apply(url, 1, function(x) { download.file(x[1], destfile = x[2]) })
dateDownloaded <- date()

## Extract zip files
sapply(url$file, unzip)

## Files are rather big! I'll keep them only in the cluster.

## getHeader determines which header to use depending on the number of columns in the data
## which are either 27 or 28
getHeader <- function(file, df = NULL, colClasses = NULL) {
	## Read the first few lines or use the previously read file
	if(is.null(df)) {
		df <- read.table(file, sep="|", nrows=10)
	}
	
	## Determine the number of columns
	head <- readLines(file, n = 2)
	extract <- lapply(head, function(x) {
		xx <- gsub("# ", "", x)
		strsplit(xx, "\\|")[[1]]
	})
	ncols <- lapply(extract, length)
	i <- which(ncols == ncol(df))
	
	## Get the number of rows
	call <- paste0("wc -l ", file, "| cut -d '", substr(file, 1, 1), "' -f1")
	lines <- as.numeric(system(call, intern = TRUE))
	
	if(ncols[i] == 28 && is.null(colClasses)) {
		res <- read.table(file, sep="|", nrows = lines + 2, col.names = extract[[i]], colClasses = c(rep("character", 12), "numeric", rep("character", 13), rep("numeric", 2)))
	} else if (!is.null(colClasses)){
		res <- read.table(file, sep="|", nrows = lines + 2, col.names = extract[[i]], colClasses = colClasses)
	} else{
		res <- read.table(file, sep="|", nrows = lines + 2, col.names = extract[[i]])	
	}
	if("Date" %in% colnames(res)) {
		res$Date <- as.Date(res$Date, format="%Y%m%d")
	}
	if("Start.Time" %in% colnames(res)) {
		res$Start.Time <- as.POSIXct(res$Start.Time, format = "%H:%M")
	}
	return(res)
}

## Testing getHeader
if(FALSE) {
	oz <- read.table("Oz2012.txt", sep="|")
	tmp <- getHeader("Oz2012.txt")
	## Works =)
	head(tmp)

	pm <- getHeader("RD_501_88101_2003-0.txt")
	print(object.size(pm), units="Mb")
}


## Loading data and saving it in an Rdata file
raw <- lapply(list.files(pattern="txt"), getHeader)
names(raw) <- list.files(pattern="txt")
print(object.size(raw), units="Mb")
## 18809.4 Mb


## The save() would break (lack of mem), probably because of the compression
## save(raw, dateDownloaded, file="raw.Rdata", compress="bzip2")
save.image("raw-image.Rdata")
save(url, dateDownloaded, file="raw-basic.Rdata")

## Exploring the data
if(FALSE) {
	max(unlist(lapply(raw, nrow)))
	## 9325064 ## It's Ozone, 2011
	summary(as.factor(raw[[9]][, "Qualifier...1"]))
	#              1       2       3       9      IE      IL      IM      IO      IP 
	#9309078     413    7235     392    1571    1136      30       3      67       1 
	#     IT      RG      RL      RM      RO      RT      VB 
	#     91      71     118     287     672     478    3421 
	15986 / ( 15986 + 9309078) * 100
	#0.1714305
	## Very low percent is non-NA. 
	lapply(raw, function(x) { summary(as.factor(x$RD))}) ## All RD
	lapply(raw, function(x) { summary(as.factor(x$Action.Code))}) ## All I
	## State.Code, keep
	## County.Code, keep
	## Site.ID, keep
	lapply(raw, function(x) { summary(as.factor(x$Parameter))}) ## Either 44201 or 88101.
	lapply(raw, function(x) { summary(as.factor(x$POC))}) ## Up to 9 values. No NAs. Keep
	lapply(raw, function(x) { summary(as.factor(x$Sample.Duration))}) ## Mostly 7, some 1. Doesn't seem like it'll be useful.
	lapply(raw, function(x) { summary(as.factor(x$Unit))}) ## Up to 3 per file, 5 types in total. Keep
	lapply(raw, function(x) { summary(as.factor(x$Method))}) ## No NA's, and varied information. Keep
	## Date, keep
	## Start.Time, keep
	## Sample.Value, keep
	lapply(raw, function(x) { summary(as.factor(x$Null.Data.Code))}) ## Majority is NA, but some have 20k+ elements. Could use a logical here just to keep track of whether there is a code or not since they seem important from https://aqs.epa.gov/aqsweb/codes/data/QualifierCodes_0_A.html
	lapply(raw, function(x) { summary(as.factor(x$Sampling.Frequency))}) ## All NA for Ozone. PM is mostly NA with some 30k+ This data seems redundant with Date and Start.Time from reading https://aqs.epa.gov/aqsweb/codes/data/CollectionFrequencies.html
	lapply(raw, function(x) { summary(as.factor(x$Monitor.Protocol..MP..ID))}) ## All NA
	## Qualifiers, ignore
	lapply(raw, function(x) { summary(as.factor(x$Alternate.Method.Detectable.Limit))}) ## Great majority is NA. Clearly more data in PM than Ozone
	lapply(raw, function(x) { summary(as.factor(x$Uncertainty))}) ## Except for 1 entry, all NA in Ozone. Great majority NA in PM.
}

## Extract columns of interest and then save the new file
raw.small <- lapply(raw, function(x) {
	cbind(x[, c("State.Code", "County.Code", "Site.ID", "POC", "Unit", "Method", "Date", "Start.Time", "Sample.Value")], data.frame("Null.flag" = !is.na(x$Null.Data.Code)))
})
names(raw.small) <- names(raw)
print(object.size(raw.small), units="Mb")
save(raw.small, file="raw-small.Rdata")


