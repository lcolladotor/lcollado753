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


## Group into a data.frame
data <- data.frame(Win = win, Score = score, Where = where, Team = team, Date = dates)
data
```

```
##    Win     Score Where          Team        Date
## 1    W      24-9    vs  Indianapolis  Sun, Jan 6
## 2    W 38-35 2OT     @        Denver Sat, Jan 12
## 3    W     28-13     @   New England Sun, Jan 20
## 4    W     34-31     @ San Francisco  Sun, Feb 3
## 5    W     44-13    vs    Cincinnati Mon, Sep 10
## 6    L     24-23     @  Philadelphia Sun, Sep 16
## 7    W     31-30    vs   New England Sun, Sep 23
## 8    W     23-16    vs     Cleveland Thu, Sep 27
## 9    W       9-6     @   Kansas City  Sun, Oct 7
## 10   W     31-29    vs        Dallas Sun, Oct 14
## 11   L     43-13     @       Houston Sun, Oct 21
## 12   W     25-15     @     Cleveland  Sun, Nov 4
## 13   W     55-20    vs       Oakland Sun, Nov 11
## 14   W     13-10     @    Pittsburgh Sun, Nov 18
## 15   W  16-13 OT     @     San Diego Sun, Nov 25
## 16   L     23-20    vs    Pittsburgh  Sun, Dec 2
## 17   L  31-28 OT     @    Washington  Sun, Dec 9
## 18   L     34-17    vs        Denver Sun, Dec 16
## 19   W     33-14    vs      New York Sun, Dec 23
## 20   L     23-17     @    Cincinnati Sun, Dec 30
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
## [1] "Tue Feb  5 17:50:30 2013"
```

```r
save(data, dateDownloaded, file = "raw.Rdata")
```



