
```r
## Download data compiled by Brian Burke The links are listed at
## http://www.advancednflstats.com/2010/04/play-by-play-data.html

## Specify the links
links <- c("https://docs.google.com/file/d/0BxEXxf9odCnMNGQzY2YyNmUtMTlhYy00YmQyLTg3ZTUtMGI2NDhjNGU4Zjg5/edit?hl=en#", 
    "https://docs.google.com/file/d/0BxEXxf9odCnMODljMmIxNzItNzJjNy00ODJiLWJiNDItMDBlZGMwMjkwOTlk/edit?hl=en#", 
    "https://docs.google.com/file/d/0BxEXxf9odCnMMGUyNjVkMWEtOWE2YS00YzI3LWJjYjEtZWU2MTIyNmJhOTk0/edit?hl=en#", 
    "https://docs.google.com/file/d/0BxEXxf9odCnMYmIyMTdjNWItMjhiNS00NjJkLWIyYWEtZmI1ZGM4NmFmZGQy/edit?hl=en#", 
    "https://docs.google.com/file/d/0BxEXxf9odCnMN2YxNGM0MzUtYTc2Mi00YjVjLWI3N2EtMzIwMDA0Y2E5OTg1/edit?hl=en#", 
    "https://docs.google.com/file/d/0BxEXxf9odCnMYWZkOWU1YTItYTUzNS00MmM4LTk1MTktYmI3Y2E1Zjc3OTIy/edit?hl=en#", 
    "https://docs.google.com/file/d/0BxEXxf9odCnMZDJmYzIzNWQtNjIyNS00NzQzLWJiMTEtYWI5M2U0MTI4Njlk/edit?hl=en#", 
    "https://docs.google.com/file/d/0BxEXxf9odCnMMDAyOGRhMjYtMzlkMC00NGQwLTgxMWUtOWNmYWMxY2Q2ODY3/edit?num=50&sort=name&layout=list#", 
    "https://docs.google.com/file/d/0BxEXxf9odCnMMWRkMDc0MDgtZDZhMi00ZGRlLTlkYjEtOTNkZjViZDI0ZGY2/edit?sort=name&layout=list&pid=0BxEXxf9odCnMZTdjMTVmMmItZjY5OS00NjQwLTg4ODYtNGFhZjFiMzhiZmQ2&cindex=8#", 
    "https://docs.google.com/file/d/0BxEXxf9odCnMbmZvYzE3cjBzblE/edit", "https://docs.google.com/file/d/0BxEXxf9odCnMOGdHRUVvOV9GVVk/edit")

names(links) <- 2002:2012

## Open them
sapply(links, browseURL)
```

```
## 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 
##    0    0    0    0    0    0    0    0    0    0    0
```

```r

## Since they are Google documents, I added them to my google drive
dateDownloaded <- date()
dateDownloaded
```

```
## [1] "Sun Mar 17 22:28:29 2013"
```

```r

## Save the data path
dataDir <- "/Users/lcollado/Google Drive/NFL"
dir(dataDir)
```

```
##  [1] "2002_nfl_pbp_data.csv"            "2003_nfl_pbp_data.csv"           
##  [3] "2004_nfl_pbp_data.csv"            "2005_nfl_pbp_data.csv"           
##  [5] "2006_nfl_pbp_data.csv"            "2007_nfl_pbp_data.csv"           
##  [7] "2008_nfl_pbp_data.csv"            "2009_nfl_pbp_data.csv"           
##  [9] "2010_nfl_pbp_data.csv"            "2011_nfl_pbp_data.csv"           
## [11] "2012_nfl_pbp_data_reg_season.csv"
```

```r

## Save the data location for later use
save(dateDownloaded, dataDir, file = "data-info.Rdata")
```



