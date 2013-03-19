## Define the function that will read the data for one year and process some variables on the play by play scale
readPBP <- function(file, verbose=TRUE) {
	
	if(FALSE){
		file <- files[1]
		verbose <- TRUE
	}
		
	## Read the file
	if(verbose) message(paste("Reading file", file))
	new <- read.csv(file, header=TRUE, stringsAsFactors=FALSE)
	
	
	
	## Classify plays
	type <- rep(NA, nrow(new))
	if(verbose) message("Classifying play types")
	
	## Challenge plays are more complicated. Some are reversed, some are upheld
	challenge <- grep("challenged", new$description, ignore.case=TRUE)
	
	## For challenge plays that are reversed, remove the initial play description
	reversed <- grep("reversed", new$description, ignore.case=TRUE)
	tochange <- challenge[challenge %in% reversed]
	new$description[tochange] <- gsub(".*(REVERSED|reversed).", "", new$description[tochange])
	
	## Find penalty plays
	penalty <- grep("PENALTY", new$description, ignore.case=TRUE)
	
	## For penalty plays where "No Play" is ruled, remove the play description prior to the penalty
	noplay <- grep("No Play", new$description)
	tochange <- penalty[penalty %in% noplay]
	new$description[tochange] <- gsub(".*PENALTY", "", new$description[tochange])
	
	
	## Find the kick off
	gameid <- unique(new$gameid)
	kickoff <- unlist(sapply(gameid, function(x) {
		first <- which(new$gameid == x)[1]
		is.kick <- grepl("kicks", new$description[first])
		if(is.kick) {
			res <- first
		} else {
			res <- NULL
		}
		return(res)
	}))
	
	## Some miscellaneous plays (kicks, fgs, extra points)
	misc <- which(is.na(new$down) & is.na(new$togo))
	misc <- misc[!misc %in% kickoff]
	
	## Find kicks, punts
	punt <- grep("punt", new$description)
	## Do not count as a kick when they actually punted (like 4 cases)
	kick <- misc[grep("kicks", new$description[misc])]
	kick <- kick[!kick %in% punt]
	
	
	## Find extra point attempts
	extrapt <- misc[grep("extra point", new$description[misc])]
	## An extra point mentioned on a 'kicks' is a kick. Sometimes they comment on penalties from extra points on kick descriptions
	extrapt <- extrapt[!extrapt %in% kick]
	
	## Find two point conversion attempts
	twopts <- misc[grep("TWO-POINT", new$description[misc])]
	
	## Check what is left of the misc plays
	if(length(misc) - length(unique(c(kick, extrapt, twopts))) > 0) {
		weird <- misc[!misc %in% c(kick, extrapt, twopts)]
		weird <- weird[!weird %in% penalty]
	}
	
	## Find sacks, interceptions, fumbles
	int <- grep("INTERCEPTED", new$description)
	sack <- grep("sacked", new$description)
	fumble <- grep("FUMBLES", new$description)
	
	## Find pass attemps
	ipass <- grep("pass incomplete", new$description) 
	# Consider punts those that were punts and then the returning team attempts a pass
	ipass <- ipass[!ipass %in% punt]
	ipass <- c(ipass, twopts[twopts %in% grep("is incomplete", new$description)])
	## Do not count as incomplete passes those that get intercepted
	ipass <- ipass[!ipass %in% int]
	cpass <- grep("pass to ", new$description)
	
	
	## Find field goal attempts
	fg <- grep("field goal", new$description)
	## Remove fake field goals
	fg <- fg[!fg %in% grep("fake field goal", new$description, ignore.case=TRUE)]
	## Note a fg can't be an extra point
	fg <- fg[!fg %in% extrapt]
	## Fg and punt can't go together. Sometimes they line up to FG and then punt, so it's a punt
	fg <- fg[!fg %in% punt]
	## Don't count as a fg cases where they line up to FG and then pass
	fg <- fg[!fg %in% ipass & !fg %in% cpass]
	
	## Define which are actually good and those missed/blocked
	fgma <- fg[grep("GOOD", new$description[fg])]
	fgmi <- fg[!fg %in% fgma]
		
	
	
	
	
	## Find kneels
	kneel <- grep("kneels", new$description)
	
	## Save play-type results
	# maybe use paste(, sep="|") then table() to see which overlap. Like kick and penalty
	type[kickoff] <- "kickoff"
	type[kick] <- "kick"
	type[punt] <- "punt"
	type[fgma] <- "made-fg"
	type[fgmi] <- "missed-fg"
	type[extrapt] <- "extrapoint"
	type[cpass] <- "complete-pass"
	
	## Give priority to incomplete passes over complete passes. Can happen when there is a pass followed by an incomplete pass on the same play
	type[ipass] <- "incomplete-pass"
	
	# Interception status takes priority over cpass, ipass descriptions
	type[int] <- "interception"
	
	## Sack and kneels
	type[sack] <- "sack"
	type[kneel] <- "kneel"
	# on-side kick?
	
	## Check for any unexpected cases
	test <- table(c(kickoff, kick, punt, fgma, fgmi, extrapt, ipass, int, kneel))
	if(sum(test>1) > 0){
		warning(paste(sum(test>1), "cases where there is an unexpected overlap in file", file, "Check rows", paste(names(test)[test>1], collapse=",")))
	}
	
	## Find anything weird left
	type[is.na(type) & (new$description == "(2:00)" | new$description == " ")] <- "ignorable"
	
	## Assign remaining as runs
	run <- is.na(type)
	type[run] <- "run"
	
	## Check
	check <- nrow(new) - sum(!is.na(type)) == 0
	if(!check) warning("Not all plays have an assigned type.")
	
	## Save play-type into final object. 
	new$type <- type
	
	## Specify whether the two point attempt was successful or not
	twotype <- rep(NA, rep(nrow(new)))
	twotype[twopts] <- "succeed"
	twotype[twopts[twopts %in% grep("FAILS", new$description)] ] <- "fail"
	new$twotype <- twotype
	
	## Fumble type: regular fumble, fumble and sack, fumble and other plays (kicks, interceptions which are then fumbled, ...)
	fumble.type <- rep(NA, rep(nrow(new)))
	weird <- fumble[fumble %in% c(kickoff, kick, punt, fgma, fgmi, extrapt, ipass, int, kneel)]
	fumble.type[fumble] <- "fumble"
	fumble.type[weird] <- "weird"
	fumble.type[fumble[fumble %in% sack]] <- "fumble-sack"
	new$ftype <- fumble.type
	
	## Challenge type: Reversed or upheld
	challenge.type <- rep(NA, rep(nrow(new)))
	challenge.type[challenge[challenge %in% reversed]] <- "reversed"
	challenge.type[challenge[!challenge %in% reversed]] <- "upheld"
	
	## Penalty type: noplay, play (some have declined penalties, offsetting penalties, others are enforced)
	penalty.type <- rep(NA, rep(nrow(new)))
	penalty.type[penalty[penalty %in% noplay]] <- "noplay"
	penalty.type[penalty[!penalty %in% noplay]] <- "play"
	declined <- grep("declined", new$description[which(penalty.type == "play")])
	declined <- which(penalty.type == "play")[declined]
	penalty.type[declined] <- "declined"
	offsetting <- grep("offsetting", new$description[which(penalty.type %in% c("noplay", "play"))])
	offsetting <- which(penalty.type %in% c("noplay", "play"))[offsetting]
	penalty.type[offsetting] <- "declined"
	
	## Save challenge and penalty types
	new$ctype <- challenge.type
	new$ptype <- penalty.type	
	
	#### Yards
	if(verbose) message("Finding passing, rushing and penalty yards")
	
	### Find passing yards
	cpass <- new$type == "complete-pass"
	pyds <- rep(NA, sum(cpass))
	
	## Plays with any yardage movement
	r <- regexpr("for -*([0-9]+) yard", new$description[cpass])
	pyds[r > 0] <- as.integer(gsub("(for | yard)", "", regmatches(new$description[cpass], r)))
	
	## Plays with no gain
	r <- grepl("for no gain", new$description[cpass])
	pyds[r] <- 0
	
	## two point attempts. If succeed give 2 yds, if fail, give 0 (might not be so punishing)
	pyds[is.na(pyds) & new$twotype[which(cpass)] == "succeed"] <- 2
	pyds[is.na(pyds)] <- 0
	
	## Save results
	passyds <- rep(NA, nrow(new))
	passyds[cpass] <- pyds
	new$passyds <- passyds
	
	### Find sack yds
	sack <- new$type == "sack"
	yds <- rep(NA, sum(sack))
	
	r <- regexpr("for -*([0-9]+) yard", new$description[sack])
	yds[r > 0] <- as.integer(gsub("(for | yard)", "", regmatches(new$description[sack], r)))
	## Remaining ones are sacks on two-point conversions. Since no yardage is reported, I'll leave them as NA
	sackyds <- rep(NA, nrow(new))
	sackyds[sack] <- yds
	new$sackyds <- sackyds
	
	### Find rushing yards. Do not consider plays where there was a penalty called and no play was determined
	run <- new$type == "run"
	runNoPlay <- which(new$ptype[which(run)] == "noplay")
	run[which(run)[runNoPlay]] <- FALSE
	
	yds <- rep(NA, sum(run))
	
	## Runs with movement
	r <- regexpr("for -*([0-9]+) yard", new$description[run])
	yds[r > 0] <- as.integer(gsub("(for | yard)", "", regmatches(new$description[run], r)))
	
	## Runs for no gain
	r <- grepl("for no gain", new$description[run])
	yds[r & is.na(yds)] <- 0
	
	## two point attempts. If succeed give 2 yds, if fail, give 0 (might not be so punishing)
	yds[is.na(yds) & new$twotype[which(run)] == "succeed"] <- 2
	yds[is.na(yds) & new$twotype[which(run)] == "fail"] <- 0
	
	## Remaining 'run' plays are either in the weird category or penalties
	runyds <- rep(NA, nrow(new))
	runyds[run] <- yds
	new$runyds <- runyds
	
	### Find penalty yards
	pen <- new$ptype %in% c("noplay", "play")
	yds <- rep(NA, sum(pen))
	
	## Penalties with some yrds enforced
	r <- regexpr("([0-9]+) yard(s*)(\\s+)enforced", new$description[pen])
	yds[r > 0] <- as.integer(gsub("( yard(s*)(\\s+)enforced)", "", regmatches(new$description[pen], r)))
	
	## Penalties enforced with weird coding
	r <- regexpr("; ([0-9]+) yard(s*)&", new$description[pen])
	if(sum(r > 0) > 0) {
		yds[r > 0]  <- as.integer(gsub("(; | yard(s*)&)", "", regmatches(new$description[pen], r)))
	}
	
	penyds <- rep(NA, nrow(new))
	penyds[pen] <- yds
	new$penyds <- penyds
	
	### Who got penalized? Offense or defense?
	i <- which(!is.na(new$penyds))
	r <- regexpr(" on ([A-Z]+)[-| ]", new$description[i])
	whop <- gsub("( on |[-| ])", "", regmatches(new$description[i], r))
	teamp <- rep("def", length(i))
	teamp[new$off[i] == whop] <- "off"
	
	## Save result
	team.pen <- rep(NA, nrow(new))
	team.pen[i] <- teamp	
	new$teamp <- team.pen
	
	

	
	### Find running success
	
	## Consider run plays where there is no penalty. Only a small subset are runs and have penalties
	## Simarly, ignore run plays where there was a fumble
	i <- which(!is.na(new$runyds) & is.na(new$penyds) & is.na(new$ftype))
	runsucc <- sapply(i, function(x) {
		if(new$down[x] %in% 1:3) {
			res <- ifelse(new$runyds[x] > 0, TRUE, FALSE)
		} else {
			res <- ifelse(new$togo[x] - new$runyds[x] <= 0, TRUE, FALSE)
		}
		return(res)
	})
	run.good <- rep(NA, nrow(new))
	run.good[i] <- runsucc
	new$rsucc <- run.good
	
	
	## Done
	return(new)
}

## Function that gets team statistics
getTeamStats <- function(team, df, half) {
	
	if(FALSE) {
		df <- data[["2002"]]
		team <- "GB"
		half <- "first"
	}
	if(!half %in% c("first", "second", "full")) error("'half' must be 'first', 'second', or 'full'")
	
	if(half == "first") {
		qtrs <- c(1, 2)
	} else if(half == "second")  {
		qtrs <- c(3, 4)
	} else {
		qtrs <- c(1:4)
	}
	df <- subset(df, qtr %in% qtrs)
	
	## Working data subsets for team of interest	
	off <- subset(df, off == team)
	rownames(off) <- 1:nrow(off)
	
	def <- subset(df, def == team)
	rownames(def) <- 1:nrow(def)
	
	## Define passing attempts
	passAtt <- which(!is.na(off$passyds) | off$type == "interception" | off$type == "incomplete-pass" | off$type == "sack")
		
	## Find net passing yards per attempt
	netPassYds <- (sum(off$passyds, na.rm=TRUE) - sum(off$sackyds, na.rm=TRUE)) / length(passAtt)
	
	## Find offensive interceptions per attempt
	offInt <- length(which(off$type == "interception")) / length(passAtt)
	
	## Find running success rate
	runSucc <- sum(off$rsucc == TRUE, na.rm=TRUE) / length(!is.na(off$rsucc))
	
	## Find offensive fumbles per attempt
	offAtt <- length(passAtt) + length(!is.na(off$rsucc))
	fumble <- sum(off$ftype %in% c("fumble", "fumble-sack")) / offAtt
	
	## Find team penalty yards per play	
	penalty <- ( sum(off$penyds[which(off$teamp == "off")]) + sum(def$penyds[which(def$teamp == "def")])) / (nrow(off) + nrow(def))
	
	## Find defensive net passing yds per attempt
	defPassAtt <- which(!is.na(def$passyds) | def$type == "interception" | def$type == "incomplete-pass" | def$type == "sack")
	defNetPassYds <- (sum(def$passyds, na.rm=TRUE) - sum(def$sackyds, na.rm=TRUE)) / length(defPassAtt)
	
	## Find defensive rushing yards per attempt
	defRushAtt <- which(def$type == "run" | def$ftype == "fumble")
	defRushYds <- sum(def$runyds, na.rm=TRUE) / length(defRushAtt)
	
	## Find defensive interceptions per attempt
	defInt <- length(which(def$type == "interception")) / length(defPassAtt)
	
	## Construct summary info
	
	data.frame(team=team, oPassYdsAtt=netPassYds, oInt=offInt, oRun=runSucc, oFumble=fumble, pen=penalty, dPassYdsAtt=defNetPassYds, dRunAtt=defRushYds, dInt=defInt)
}

## This function extracts the relevant information for teams A and B
gameConstruct <- function(teamA, teamB, teamdf, playdf, id, is.local) {
	
	if(FALSE) {
		## For testing
		id <- gameid[1]
		teamA <- teams[[1]][1]
		teamB <- teams[[1]][2]
		is.local <- FALSE
	}
	
	#print(id)
	
	## Figure out which was the last play of the game
	last <- function(x) { x[length(x)] }
	gamend <- last(which(playdf$gameid==id))
		
	## Final score diff
	scorediff <- playdf$offscore[gamend] - playdf$defscore[gamend]
	
	if(is.na(scorediff)) {
		## Some games are missing the information completely
		message(paste("Game", id, "has no information"))
		scorediff <- NA
		win <- NA
		halfdiff <- NA
		is.resumes <- NA
	} else {
		if(scorediff == 0) {
			## Consider a tie a win for both teams
			win <- TRUE
		} else {
			## Figure out if the team was on the off or def on the last play
			is.off <- teamA == playdf$off[gamend]
			## Assume teamA won
			win <- TRUE
			if(is.off & scorediff <0) {
				## Was offensive team, yet the def team had more points
				win <- FALSE
			} else if(!is.off & scorediff > 0) {
				## Was def team, yet the off team had more points
				win <- FALSE
			}
		}
	
		## Figure out the last play of the 1st half
		halftime <- last(which(playdf$gameid==id & playdf$qtr==2))
	
		## Some games don't have data on the 2nd quarter!
		if(length(halftime) == 0) {
			message(paste("Game", id, "has no 2nd quarter data"))
			halfdiff <- NA
		} else {
			## Figure out the half time score difference (+ means teamA is winning)
			halfdiff <- playdf$offscore[halftime] - playdf$defscore[halftime]
			is.off <- teamA == playdf$off[halftime]
			if(!is.off) {
				halfdiff <- halfdiff * (-1)
			}
		}
		
		## TeamA starts 2nd half?
		## Assume that the 1st play of the 3rd quarter is a kick
		firstq3 <- which(playdf$gameid==id & playdf$qtr==3)[1] + 1
		is.resumes <- teamA == playdf$off[firstq3]
	}
	
	
	## Game day
	date <- as.Date(substr(id, 1, 8), format="%Y%m%d")
		
	## Merge team stats
	stats <- data.frame(teamdf[teamdf$team == teamA, 2:ncol(teamdf)], teamdf[teamdf$team == teamB, 2:ncol(teamdf)])
	colnames(stats) <- paste0(rep(c("teamA", "teamB"), each=ncol(teamdf)-1), colnames(teamdf)[2:ncol(teamdf)])
	
	## Add other info
	res <- stats
	res$local <- is.local
	res$win <- win
	res$halfdiff <- halfdiff
	res$date <- date
	res$resumes <- is.resumes
	res$teamA <- teamA
	res$teamB <- teamB
	
	## Finish
	return(res)
}

## gameSummary extracts all the game summaries for a given pair of team summary and games
gameSummary <- function(teamdf, playdf) {
	
	if(FALSE) {
		## For testing
		teamdf <- first[["2011"]]
		playdf <- data[["2011"]]
		i <- 1
	}
	
	## Ignore weird cases (2 in 2010) of empty rows
	playdf <- subset(playdf, gameid != "")
	
	gameid <- unique(playdf$gameid)
	teams <- strsplit(gsub("[0-9]+_", "", gameid), "@")
	
	res <- lapply(1:length(gameid), function(i) {
		if(teams[[i]][1] %in% c("NFC", "AFC")) {
			## Remove pro-bowl
			result <- NULL
		} else {
			res1 <- gameConstruct(teamA=teams[[i]][1], teamB=teams[[i]][2], teamdf=teamdf, playdf=playdf, id=gameid[i], is.local=FALSE)
			res2 <- gameConstruct(teamA=teams[[i]][2], teamB=teams[[i]][1], teamdf=teamdf, playdf=playdf, id=gameid[i], is.local=TRUE)
			result <- rbind(res1, res2)
		}
		return(result)
	})
	res <- do.call(rbind, res)
	rownames(res) <- 1:nrow(res)
	
	## Find game number (no longer week because some teams get to the SB through the wildcard round and others don't)
	gameA <- sapply(1:nrow(res), function(i) {
		if(i == 1) {
			week <- 1
		} else {
			week <- sum(res$teamA[1:i] == res$teamA[i])
		}
		return(week)
	})
	
	gameB <- sapply(1:nrow(res), function(i) {
		if(i == 1) {
			week <- 1
		} else {
			week <- sum(res$teamB[1:i] == res$teamB[i])
		}
		return(week)
	})
	
	## Find game day winning record
	gwrA <- sapply(1:nrow(res), function(i) {
		if(gameA[i] == 1) {
			gwr <- 0
		} else {
			gwr <- sum(res$win[which(res$teamA[1:(i-1)] == res$teamA[i])]) / (gameA[i]-1)
		}
		return(gwr)
	})
	
	gwrB <- sapply(1:nrow(res), function(i) {
		if(gameB[i] == 1) {
			gwr <- 0
		} else {
			gwr <- sum(res$win[which(res$teamA[1:(i-2)] == res$teamB[i])]) / (gameB[i]-1)
		}
		return(gwr)
	})
	
	## Add results
	res$gameA <- gameA
	res$gwrA <- gwrA
	res$gameB <- gameB
	res$gwrB <- gwrB		
	
	## Finish
	return(res)
}
