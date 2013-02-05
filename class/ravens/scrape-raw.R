## Instructions
## https://github.com/jtleek/jhsph753/blob/master/assignments/feb5-inclass.md

library(XML)
html3 <- htmlTreeParse("http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens", useInternalNodes=TRUE)

## A bit on xpath syntax
## http://www.w3schools.com/xpath/xpath_syntax.asp

## Get wins and scores
win <- xpathSApply(html3, "//li[@class='game-status win'] | //li[@class='game-status loss']", xmlValue)
score <- xpathSApply(html3, "//li[@class='score']", xmlValue)
where <- xpathSApply(html3, "//li[@class='game-status']", xmlValue)
team <- xpathSApply(html3, "//li[@class='team-name']", xmlValue)

## Group into a data.frame
data <- data.frame(Win = win, Score = score, Where = where, Team = team)
data

## Another way of doing it (from Jiawei)
xpathSApply(html3, "//ul[@class='game-schedule']", xmlValue)

dateDownloaded <- date()
dateDownloaded

save(data, dateDownloaded, file="raw.Rdata")