## Load data
load("info.Rdata")
load("gaData.rda")

### Merge tumblr and wp scrapped data

## Last tumblr post
last <- sort(tumblr$date, decreasing=TRUE)[1]

## wp after the move to tumblr (including the overlap)
wp.new <- wp[ wp$date >= last - 3,]
## Dropping that one overlap (link roundoup 11/18/12)
wp.new <- wp.new[ -nrow(wp.new),]

## Complete the merge
wp.new$type <- NA
posts <- rbind(tumblr, wp.new)
## sort
posts <- posts[order(posts$date),]

### simplify gaData
simply <- gaData
colnames(simply) <- c("date", "vTum", "vWP", "twiTum", "twiWP")
simply$v <- simply$vTum + simply$vWP
simply$twi <- simply$twiTum + simply$twiWP
simply$post <- simply$date %in% posts$date

## simple EDA
plot(simply$date, simply$v, col=ifelse(simply$post, "blue", "orange"))

## Remove the early 2011 info before a post was made
simply <- simply[simply$date >= simply$date[which(simply$v > 0)[1]], ]
rownames(simply) <- 1:nrow(simply)

## Save data
save(simply, posts, file="simplified.Rdata", compress="gzip")

