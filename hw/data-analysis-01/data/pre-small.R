## Load library
library(IRanges)

## Load data
load("raw-basic.Rdata")

## Function that will read the data and store it in a DataFrame
readFile <- function(file) {
	## Get the number of rows
	call <- paste0("wc -l ", file, "| cut -d '", substr(file, 1, 1), "' -f1")
	lines <- as.numeric(system(call, intern = TRUE))
	
	## Get column names
	head <- readLines(file, n = 1)
	extract <- strsplit(gsub("# ", "", head), "\\|")[[1]]
	
	## Columns to keep
	keep <- c(3:5, 7, 9:14)
	cols <- rep("NULL", 28)
	cols[keep] <- c(rep("character", 8), "numeric", "character") 
	
	## Read in table trying to maximize the efficiency
	df <- read.table(file, sep="|", nrows = lines + 2, skip = 2, header = FALSE, comment.char = "#", quote = "", col.names = extract, colClasses = cols)
	
	## Summarize into a DataFrame
	res <- DataFrame(State = Rle(df$State.Code), County = Rle(df$County.Code), Site = Rle(df$Site.ID), POC = Rle(df$POC), Unit = Rle(df$Unit), Method = Rle(df$Method), Date = Rle(df$Date), Start.Time = Rle(df$Start.Time), Value = Rle(df$Sample.Value), Null = Rle(df$Null.Data.Code))
	
	## Compare object sizes
	print(print(object.size(df), units="Mb") - print(object.size(res), units="Mb"), units="Mb")
	
	## Finish
	return(res)
}

## Test function
if(FALSE) {
	oz <- readFile("Oz2012.txt")
	## Works =)
}

## Process data
pre <- DataFrameList(lapply(list.files(pattern="txt"), readFile))
names(pre) <- list.files(pattern="txt")
print(object.size(pre), units="Mb")

## Save the object
save(pre, file="pre-small.Rdata")

### To later load the file remember to first 
## library(IRanges)

## Example of what you can do
if(FALSE) {
	library(IRanges)
	
	## Load
	load("raw-basic.Rdata")
	load("pre-small.Rdata")
}

## Mean by state for entry 1
tapply(pre[[1]]$Value, pre[[1]]$State, mean, na.rm = TRUE)
	
## Map entry with url table
map <- sapply(gsub("-0.txt", "", names(pre)), function(x) { grep(x, url$file) })
	
## Mean by state with year, type for all entries
means <- lapply(1:length(pre), function(i) {
	x <- pre[[i]]
	y <- tapply(x$Value, x$State, mean, na.rm = TRUE)
	data.frame(Mean = y, State = names(y), type = factor(url$type[map[i]]), year = url$year[map[i]], stringsAsFactors = FALSE)
})
means.df <- do.call(rbind, means)
rownames(means.df) <- 1:nrow(means.df)
dim(means.df)
#1065    4
head(means.df)
save(means.df, map, file = "example-means.Rdata")
## Note that this previous example is incomplete because I have yet to fix the values according to the Units used.