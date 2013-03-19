## Gets the paired predictions
getPred <- function(f, newdata, average=FALSE) {
	
	if(FALSE){
		## Testing
		f <- fitStep
		newdata <- info2012
		i <- 1
	}
	
	idx <- rep(c(TRUE, FALSE), nrow(newdata)/2)
	
	if(average){
		## Hm... maybe later I'll check if I can use this to improve the preds
		cnames <- colnames(newdata)
		cnames <- cnames[!cnames %in% c("teamA", "teamB", "win", "local", "resumes", "date")]
		toavg <- newdata[, cnames]
		leagueavg <- colMeans(toavg)
		leaguedf <- data.frame(matrix(leagueavg, nrow=1))
		colnames(leaguedf) <- names(leagueavg)
		leaguedf$local <- FALSE
		leaguedf$resumes <- FALSE
		logitAvg <- predict(f, newdata=leaguedf)
	}	
	
	res <- sapply(which(idx), function(i) {
		logitA <- predict(f, newdata=info2012[i, ])
		logitB <- predict(f, newdata=info2012[i+1, ])
		p1 <- ilogit(logitA - logitB)
		return(c(p1, 1-p1))
	})
	as.vector(res)
}

## This function evaluates a prediction according to a given number of breaks (bin)
evalPred <- function(pred, bin, truth, plot=TRUE) {
	if(FALSE){
		## Testing
		pred <- pStep
		bin <- 20
		truth <- info2012
	}
	
	groups <- cut(pred, bin)
	
	real <- tapply(truth$win, groups, mean)
	endpoints <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", names(real)) ), upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", names(real)) ))
	
	intervalMean <- rowMeans(endpoints)
	
	plot(intervalMean, real)
	abline(a=0, b=1, col="red")
	
	list(centers=intervalMean, real=real)
}
