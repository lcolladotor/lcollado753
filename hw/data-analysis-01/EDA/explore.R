## Load libraries
library(ggplot2)

## Load data
load("../data/month-county.Rdata")
load("../data/month-site.Rdata")

## Read in State info to determine West from East (or Non-US)
states <- read.csv("../data/StateCountyCodes.csv", skip=1, colClasses="factor")
states.region <- states[, c("State.Abbr", "County.Name", "Region")]
names(states.region) <- c("State", "County", "Region")
states.region$Cardinal <- as.factor(sapply(states.region$Region, function(x) { 
		if(x %in% c("01", "02", "03", "04", "05")) {
			"East"
		} else if (x %in% c("06", "07", "08", "09", "10")) {
			"West"
		} else {
			"Non-US"
		}
	}))


## Merge means
means.county <- merge(month.county[[1]], month.county[[2]], by = c("State", "County", "Month"), all = TRUE, suffixes = paste0(".", names(month.county)))
means.site <- merge(month.site[[1]], month.site[[2]], by = c("State", "County", "Site", "Month"), all = TRUE, suffixes = paste0(".", names(month.site)))

## Add in Region and Cardinal
all.county <- merge(means.county, states.region, by = c("State", "County"), all.x = TRUE)
all.site <- merge(means.site, states.region, by = c("State", "County"), all.x = TRUE)

## Check object sizes
print(object.size(means.county), units = "Mb")
print(object.size(all.county), units = "Mb")
print(object.size(means.site), units = "Mb")
print(object.size(all.site), units = "Mb")

## Check resulting dimensions
dim(all.county)
dim(all.site)

## Summaries
summary(all.county)


## Complete.cases versions
comp.county <- all.county[complete.cases(all.county),]
comp.site <- all.site[complete.cases(all.site),]

## Check resulting dimensions
dim(comp.county)
dim(comp.site)

## New summaries
summary(comp.county) 
# Note that Non-US dissapears
summary(comp.site)


##
ggplot(comp.county, aes(x=Mean.Value.PM, y=Mean.Value.Ozone, colour = Cardinal)) + geom_point(alpha=1, shape=1) + geom_smooth(method=lm)

## rlm
library(MASS)
ggplot(comp.county, aes(x=Mean.Value.PM, y=Mean.Value.Ozone, colour = Cardinal)) + geom_point(alpha=1) + geom_smooth(method=rlm)

## gam
library(mgcv)
ggplot(comp.county, aes(x=Mean.Value.PM, y=Mean.Value.Ozone, colour = Cardinal)) + geom_point(alpha=1) + geom_smooth(method=gam, formula=y ~ s(x))

ggplot(comp.county, aes(x=Mean.Value.PM, y=Mean.Value.Ozone, colour = Cardinal)) + geom_point(alpha=1) + geom_smooth(method=gam, formula=y ~ s(x, bs="cs"))

## splines
library(splines)
ggplot(comp.county, aes(x=Mean.Value.PM, y=Mean.Value.Ozone, colour = Cardinal)) + geom_point(alpha=1) + geom_smooth(method=lm, formula=y ~ ns(x, 50))

## Subset PM <=50
ggplot(subset(comp.county, Mean.Value.PM <= 50), aes(x=Mean.Value.PM, y=Mean.Value.Ozone, colour = Cardinal)) + geom_point(alpha=1/5) + geom_smooth(method=lm)
ggplot(subset(comp.county, Mean.Value.PM <= 50), aes(x=Mean.Value.PM, y=Mean.Value.Ozone, colour = Cardinal)) + geom_point(alpha=1/5) + geom_smooth(method=gam, formula=y ~ s(x, bs="cs"))
ggplot(subset(comp.county, Mean.Value.PM <= 50), aes(x=Mean.Value.PM, y=Mean.Value.Ozone, colour = Cardinal)) + geom_point(alpha=1/5) + geom_smooth(method=lm, formula=y ~ ns(x, 5))

ggplot(subset(comp.county, State == "MD"), aes(x=Month, y=Mean.Value.PM)) + geom_point() + geom_smooth(method=loess)

## PM vs Month
ggplot(comp.county, aes(x=Month, y=Mean.Value.PM)) + geom_point(alpha=1/10) + geom_smooth(method=lm) + facet_grid(Cardinal ~.)
ggplot(comp.county, aes(x=Month, y=Mean.Value.PM)) + geom_line() + geom_smooth(method=lm) + facet_grid(Cardinal ~.)
ggplot(comp.county, aes(x=Month, y=Mean.Value.PM, colour=Cardinal)) + geom_point(alpha=1/20, shape=1) + geom_smooth(method=lm, formula = y ~ ns(x, 20))

## Densities
ggplot(comp.county, aes(x=Mean.Value.PM, fill=Cardinal)) + geom_density(alpha=0.3) #+ xlim(c(0, 50))
ggplot(comp.county, aes(x=Mean.Value.Ozone, fill=Cardinal)) + geom_density(alpha=0.3)

## 2-d density
ggplot(comp.county, aes(x=Mean.Value.PM, y=Mean.Value.Ozone)) + stat_density2d(aes(colour=..level..))
ggplot(comp.county, aes(x=Mean.Value.PM, y=Mean.Value.Ozone)) + stat_density2d(geom="tile", aes(fill=..density..), contour=FALSE)

## PM vs Month, mean per month
ggplot(comp.county, aes(x=Month, y=Mean.Value.PM, colour=Cardinal)) + stat_summary(fun.y = mean, geom="point") + geom_smooth(method=lm)
ggplot(comp.county, aes(x=Month, y=Mean.Value.PM, colour=Cardinal)) + stat_summary(fun.y = mean, geom="point") + geom_smooth(method=gam, formula= y ~ s(x, bs="cs"))
## like these two
ggplot(comp.county, aes(x=Month, y=Mean.Value.PM, colour=Cardinal)) + stat_summary(fun.y = mean, geom="point") + geom_smooth(method=lm, formula= y ~ ns(x, 24))
ggplot(comp.county, aes(x=Month, y=Mean.Value.Ozone, colour=Cardinal)) + stat_summary(fun.y = mean, geom="point") + geom_smooth(method=lm, formula= y ~ ns(x, 24))


## Attempting to plot both Ozone and PM vs time
df.county <- rbind(month.county[[1]], month.county[[2]])
df.county$Type <- rep(names(month.county), unlist(lapply(month.county, nrow)))
df.county <- merge(df.county, states.region, by = c("State", "County"), all.x = TRUE)
df.county <- df.county[df.county$Cardinal != "Non-US",]
df.county <- df.county[order(df.county$Month),]
head(df.county)
## ggplot(df.county, aes(x=Mean.Value, y=Month, colour=Cardinal)) + stat_summary(fun.y =mean, geom="point") + facet_grid(Type ~., scale="free_y") ## too slow
qplot(Month, Mean.Value, data=df.county, geom = "point", stat="summary", fun.y =mean) + facet_grid(Type ~ Cardinal, scale="free_y") + geom_smooth(method=lm, formula= y~ns(x, 24)) ## Use this

# Old Heatmap
ggplot(comp.county, aes(x=Month, y=State, fill=Mean.Value.PM)) + geom_raster()
ggplot(comp.county, aes(x=Month, y=State, fill=Mean.Value.Ozone)) + geom_raster()

## Heatmap using plyr
library(plyr)
state.county <- ddply(comp.county, c("Month", "State"), summarise, Month.Mean.PM = mean(Mean.Value.PM, na.rm=TRUE), Month.Mean.Ozone = mean(Mean.Value.Ozone, na.rm=TRUE))
ggplot(state.county, aes(x=Month, y=State, fill=Month.Mean.PM)) + geom_raster()
ggplot(state.county, aes(x=Month, y=State, fill=Month.Mean.Ozone)) + geom_raster()

## Heatmap with RColorBrewer
library(RColorBrewer)
cols <- brewer.pal(11, "PuOr")
ggplot(state.county, aes(x=Month, y=State, fill=Month.Mean.PM)) + geom_raster() + scale_fill_gradient2(midpoint = median(state.county$Month.Mean.PM), mid=cols[6], low=cols[1], high=cols[11]) + theme(panel.background = element_rect(fil="lightblue")) 


ggplot(subset(comp.county, State == "AR"), aes(x=Month, y=Mean.Value.PM)) + geom_point() + geom_smooth(method=loess)

## Map?
library(maps)
map.county <- map_data("county")

save(df.county, comp.county, states, state.county, means.county, all.county, file="county-eda.Rdata")