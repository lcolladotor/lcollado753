Scrapping Ravens data from ESPN
===============================

Instructions
https://github.com/jtleek/jhsph753/blob/master/assignments/feb5-inclass.md


```r
## Load lib and read data
library(XML)
html3 <- htmlTreeParse("http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens", 
    useInternalNodes = TRUE)
```


A bit on xpath syntax
http://www.w3schools.com/xpath/xpath_syntax.asp


```r
## Get wins and scores
win <- xpathSApply(html3, "//li[@class='game-status win'] | //li[@class='game-status loss']", 
    xmlValue)
score <- xpathSApply(html3, "//li[@class='score']", xmlValue)
where <- xpathSApply(html3, "//li[@class='game-status']", xmlValue)
team <- xpathSApply(html3, "//li[@class='team-name']", xmlValue)
## From James and Jiawei
dates <- xpathSApply(html3, "//tr[contains(@class,'row team')]/td[2]", xmlValue)


## Get Baltimore and opponent's scores
score.split <- lapply(strsplit(score, "-"), function(x) {
    gsub(" [0-9]*OT", "", x)
})
fooAssign <- function(x, y) {
    res <- as.numeric(x)
    if (y == "L") {
        res <- rev(res)
    }
    return(res)
}
score.mat <- mapply(fooAssign, score.split, win)

dates <- as.Date(paste(dates, rep(c(2013, 2012), c(4, 16)), sep = "-"), format = "%a, %b %d-%Y")

## Group into a data.frame
data <- data.frame(Win = win, Ravens = score.mat[1, ], Opp = score.mat[2, ], 
    Where = where, Team = team, Date = dates)
data
```

```
##    Win Ravens Opp Where          Team       Date
## 1    W     24   9    vs  Indianapolis 2013-01-06
## 2    W     38  35     @        Denver 2013-01-12
## 3    W     28  13     @   New England 2013-01-20
## 4    W     34  31     @ San Francisco 2013-02-03
## 5    W     44  13    vs    Cincinnati 2012-09-10
## 6    L     23  24     @  Philadelphia 2012-09-16
## 7    W     31  30    vs   New England 2012-09-23
## 8    W     23  16    vs     Cleveland 2012-09-27
## 9    W      9   6     @   Kansas City 2012-10-07
## 10   W     31  29    vs        Dallas 2012-10-14
## 11   L     13  43     @       Houston 2012-10-21
## 12   W     25  15     @     Cleveland 2012-11-04
## 13   W     55  20    vs       Oakland 2012-11-11
## 14   W     13  10     @    Pittsburgh 2012-11-18
## 15   W     16  13     @     San Diego 2012-11-25
## 16   L     20  23    vs    Pittsburgh 2012-12-02
## 17   L     28  31     @    Washington 2012-12-09
## 18   L     17  34    vs        Denver 2012-12-16
## 19   W     33  14    vs      New York 2012-12-23
## 20   L     17  23     @    Cincinnati 2012-12-30
```




```r
## Another way of doing it (from Jiawei)
xpathSApply(html3, "//ul[@class='game-schedule']", xmlValue)
```

```
##  [1] "vsIndianapolis" "W24-9"          "@Denver"        "W38-35 2OT"    
##  [5] "@New England"   "W28-13"         "@San Francisco" "W34-31"        
##  [9] "vsCincinnati"   "W44-13"         "@Philadelphia"  "L24-23"        
## [13] "vsNew England"  "W31-30"         "vsCleveland"    "W23-16"        
## [17] "@Kansas City"   "W9-6"           "vsDallas"       "W31-29"        
## [21] "@Houston"       "L43-13"         "@Cleveland"     "W25-15"        
## [25] "vsOakland"      "W55-20"         "@Pittsburgh"    "W13-10"        
## [29] "@San Diego"     "W16-13 OT"      "vsPittsburgh"   "L23-20"        
## [33] "@Washington"    "L31-28 OT"      "vsDenver"       "L34-17"        
## [37] "vsNew York"     "W33-14"         "@Cincinnati"    "L23-17"
```




```r
dateDownloaded <- date()
dateDownloaded
```

```
## [1] "Tue Feb 12 11:56:42 2013"
```

```r
save(data, dateDownloaded, file = "raw.Rdata")
```



