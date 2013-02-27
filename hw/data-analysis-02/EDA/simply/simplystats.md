Exploring the simplystats data
==============================



```r
## Load data
load("../../data/simplified.Rdata")

## Visits vs time
plot(simply$date, simply$v, col = ifelse(simply$post, "blue", "orange"))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-11.png) 

```r

## Twitter visits vs time
plot(simply$date, simply$twi, col = ifelse(simply$post, "forest green", "purple"))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-12.png) 



