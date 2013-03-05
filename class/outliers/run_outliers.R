## Do
## F-dist, n = 20, df1 = 5, df2 = 20


## Source myOutliers.R
source("../../../jhsph753/assignments/myOutliers.R")

## Generate data
set.seed(907)
data <- matrix(rf(n = 20 * 40, df1 = 5, df2 = 20), ncol = 40)

## Generate data
if(FALSE) {
	res <- myOutliers(data)
}


## Save data
save(res, file="res.Rdata")

## Make a plot
plot(res$data, res$outlierFunction(res$data))
