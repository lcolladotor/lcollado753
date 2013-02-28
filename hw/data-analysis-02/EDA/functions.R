## Does the run means
getRunMeans <- function(window = 2) {
	runmeans <- data.frame(start = simply$date - window, end = simply$date + window, mean = rep(NA, nrow(simply)))
	for(i in 1:nrow(runmeans)) {
		runmeans$mean[i] <- mean(simply$v[ simply$date >= runmeans$start[i] & simply$date <= runmeans$end[i] ])
	}
	return(runmeans)
}

## Function that gets the start and end
limitPeak <- function(df) {
	data.frame(start = df$date[1], end = df$date[nrow(df)])
}

## Main function for finding the peaks
getPeaks <- function(df, diff, q, space = 1) {
	i <- which(diff > quantile(diff, q))
	dates <- df$date[i]
	dates.diff <- diff(dates)
	
	## For peaks far from each other find the start and end date of the peak
	j <- which(dates.diff <= space)
	good <- i[-c(j, j+1)]
	res <- lapply(good, function(x) {
		limitPeak(df[x,])
	})
	res <- do.call(rbind, res)

	## In peaks too close to each other, find the day with the highest value and call it the peak day
	if(length(j) > 0) {
		while(length(j) > 0) {
			k <- j[1]
			j <- j[-1]
			if(length(j) > 0) {
				while(j[1] - k[length(k)] == 1) {
					k <- c(k, j[1])
					j <- j[-1]
					if(length(j) == 0) break
				}
			}
			info <- df[i[c(k, k[length(k)]+1)],]
			res <- rbind(res, limitPeak(info))
			## Now that we have the k groups
		}
	}
	
	## End
	res <- res[order(res$start), ]
	res$ndays <- as.integer(res$end - res$start) + 1
	return(res)
}

## Main function that completes the peaks (It adds tons of info)
completePeaks <- function(peak, df, post) {
	
	### Get information from before the peak
	
	## pre start day (1st day of data or last peak end + 1)
	pre.start <- sapply(1:nrow(peak), function(x) {
		if(x == 1) {
			res <- df$date[1]
		} else {
			res <- peak$end[x-1] + 1
		}
		return(res)
	}, simplify=FALSE)
	pre.start <- do.call(c, pre.start)
	
	## pre end day (1 day before the peak begins)
	pre.end <- sapply(1:nrow(peak), function(x) {
		peak$start[x] - 1
	}, simplify=FALSE)
	pre.end <- do.call(c, pre.end)
	
	## Get max visits on pre-peak
	pre.v.max <- mapply(function(x, y) {
		z <- subset(df, date >= x & date <= y)
		max(z$v)
	}, pre.start, pre.end)
	pre.twi.max <- mapply(function(x, y) {
		z <- subset(df, date >= x & date <= y)
		max(z$twi)
	}, pre.start, pre.end)
	
	## Get min visits on pre-peak
	pre.v.min <- mapply(function(x, y) {
		z <- subset(df, date >= x & date <= y)
		min(z$v)
	}, pre.start, pre.end)
	pre.twi.min <- mapply(function(x, y) {
		z <- subset(df, date >= x & date <= y)
		min(z$twi)
	}, pre.start, pre.end)
	
	## Total visits on pre-peak
	pre.v.tot <- mapply(function(x, y) {
		z <- subset(df, date >= x & date <= y)
		sum(z$v)
	}, pre.start, pre.end)
	pre.twi.tot <- mapply(function(x, y) {
		z <- subset(df, date >= x & date <= y)
		sum(z$twi)
	}, pre.start, pre.end)
	
	## Mean visits on pre-peak
	pre.v.mean <- mapply(function(x, y) {
		z <- subset(df, date >= x & date <= y)
		mean(z$v)
	}, pre.start, pre.end)
	pre.twi.mean <- mapply(function(x, y) {
		z <- subset(df, date >= x & date <= y)
		mean(z$twi)
	}, pre.start, pre.end)
	
	## SD visits on pre-peak
	pre.v.sd <- mapply(function(x, y) {
		z <- subset(df, date >= x & date <= y)
		sd(z$v)
	}, pre.start, pre.end)
	pre.twi.sd <- mapply(function(x, y) {
		z <- subset(df, date >= x & date <= y)
		sd(z$twi)
	}, pre.start, pre.end)
		
	## ndays on pre-peak
	pre.ndays <- mapply(function(x, y) {
		z <- subset(df, date >= x & date <= y)
		nrow(z)
	}, pre.start, pre.end)
	
	## Avg  # posts / day on pre-peak
	pre.posts <- mapply(function(x, y) {
		z <- subset(post, date >= x & date <= y)
		nrow(z)
	}, pre.start, pre.end)
	pre.posts.day <- pre.posts / pre.ndays
	
		
	### Get information from the peak
	
	## posts by author
	author <- mapply(function(x, y) {
		z <- subset(post, date >= x & date <= y)
		roger <- sum(z$author == "Roger Peng")
		rafa <- sum(z$author == "Rafael Irizarry")
		jeff <- sum(z$author == "Jeff Leek")
		#other <- sum(z$author %in% c("admin", "Steven Salzberg"))
		data.frame(Roger = roger, Rafa = rafa, Jeff = jeff) #, Other = other)
	}, peak$start, peak$end, SIMPLIFY=FALSE)
	peak.author <- do.call(rbind, author)
	
	## Number of posts
	peak.nposts <- rowSums(peak.author)
	
	## peak Avg controversial ranking of the posts
	peak.posts.cont.mean <- mapply(function(x, y) {
		z <- subset(post, date >= x & date <= y)
		if(nrow(z) > 0) {
			res <- mean(z$cont)
		} else {
			res <- 0	
		}
		return(res)
	}, peak$start, peak$end)
	
	## Max controversial ranking of the posts
	peak.posts.cont.max <- mapply(function(x, y) {
		z <- subset(post, date >= x & date <= y)
		if(nrow(z) > 0) {
			res <- max(z$cont)
		} else {
			res <- 0	
		}
		return(res)
	}, peak$start, peak$end)
	
	## Min controversial ranking of the posts
	peak.posts.cont.min <- mapply(function(x, y) {
		z <- subset(post, date >= x & date <= y)
		if(nrow(z) > 0) {
			res <- min(z$cont)
		} else {
			res <- 0	
		}
		return(res)
	}, peak$start, peak$end)
	
	## peak max
	peak.v.max <- mapply(function(x, y) {
		z <- subset(df, date >= x & date <= y)
		max(z$v)
	}, peak$start, peak$end)
	peak.twi.max <- mapply(function(x, y) {
		z <- subset(df, date >= x & date <= y)
		max(z$twi)
	}, peak$start, peak$end)
	
	## peak mean
	peak.v.mean <- mapply(function(x, y) {
		z <- subset(df, date >= x & date <= y)
		mean(z$v)
	}, peak$start, peak$end)
	peak.twi.mean <- mapply(function(x, y) {
		z <- subset(df, date >= x & date <= y)
		mean(z$twi)
	}, peak$start, peak$end)
	
	## peak start day
	## peak end day
	## peak ndays
	
	### Get information from after the peak
	
	## post-peak start
	post.start <- sapply(1:nrow(peak), function(x) {
		peak$end[x] + 1
	}, simplify=FALSE)
	post.start <- do.call(c, post.start)
	
	## post-peak end
	post.end <- sapply(1:nrow(peak), function(x) {
		if(x == nrow(peak)) {
			res <- df$date[nrow(df)] 
		} else {
			res <- peak$start[x +1] - 1
		}
		return(res)
	}, simplify=FALSE)
	post.end <- do.call(c, post.end)
	
	## post-peak mean
	post.v.mean <- mapply(function(x, y) {
		z <- subset(df, date >= x & date <= y)
		mean(z$v)
	}, post.start, post.end)
	post.twi.mean <- mapply(function(x, y) {
		z <- subset(df, date >= x & date <= y)
		mean(z$twi)
	}, post.start, post.end)
	
	
	### Outcome of interest
	outcome <- 100 * (post.v.mean - pre.v.mean) / (peak.v.max - pre.v.mean)
	
	### Group results
	res <- cbind(peak, pre.start, pre.end, pre.v.max, pre.twi.max, pre.v.min, pre.twi.min, pre.v.tot, pre.twi.tot, pre.v.mean, pre.twi.mean, pre.v.sd, pre.twi.sd, pre.ndays, pre.posts, pre.posts.day, peak.author, peak.nposts, peak.posts.cont.mean, peak.posts.cont.max, peak.posts.cont.min, peak.v.max, peak.twi.max, peak.v.mean, peak.twi.mean, post.start, post.end, post.v.mean, post.twi.mean, outcome)
	
	## Done!
	return(res)
}

## Refines the peaks by removing incorrectly called peaks
peakRefine <- function(peak, df, post, verbose = TRUE) {
	i <- which(peak$peak.v.max - peak$pre.v.mean <= 0)
	if(length(i) > 0) {
		if(verbose) {
			print(paste("Removing entry(ies)", i))
		}
		res <- completePeaks(peak[-i, c("start", "end", "ndays")], df, post)
		if(sum(res$peak.v.max - res$pre.v.mean <= 0) > 0) {
			res <- peakRefine(res, df, post)
		}
	} else {
		res <- peak
	}
	rownames(res) <- 1:nrow(res)
	return(res)
}