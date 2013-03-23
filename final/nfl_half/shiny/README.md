# Viewing options

* Use the shiny app through RStudio's shiny beta test server (preferred)
	* http://glimmer.rstudio.com/lcolladotor/NFLhalf/
* Run from R (kinda slow because it download's the whole repository)

```r
library(shiny)
runUrl("https://github.com/lcolladotor/lcollado753/archive/master.zip",
subdir = "final/nfl_half/shiny/")
```
* Run locally: clone the repository, open R, then

```r
library(shiny)
setwd("whereYouClonedTheRepo/lcollado753/final/nfl_half/shiny")
runApp()
```

