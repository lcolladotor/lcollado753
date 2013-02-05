###
load("trainData.rda")

length(trainData)
trainData[[1]]
trainData[[2]]

plot(trainData[[1]][1,], trainData[[1]][2,])



plot(trainData[[1]][1,])

## Predict for variable 1 at time points
# 18, 19, 20

### Summarize information according to the method I submitted
mat <- matrix(NA, ncol = 50, nrow = ncol(trainData[[1]]))
for(k in 1:ncol(trainData[[1]])) {
	for(i in 1:length(trainData)) {
		if(k == 1) {
			cols <- which(trainData[[i]][1,] <= trainData[[1]][1,k])
		} else{
			cols <- which(trainData[[i]][1,] <= trainData[[1]][1,k] & trainData[[i]][1,] > trainData[[1]][1,k-1] )
		}
		if(length(cols) > 0) {
			mat[k, i] <- mean(trainData[[i]][2,cols])
		} else {
			mat[k, i] <- NA
		}
	}
}

## Explore summary of info
summary(mat)
heatmap(mat)

## explore V1
qqnorm(trainData[[1]][2,])
qqline(trainData[[1]][2,])
## Could use lm

## Impute using mean cols
mat2 <- mat
for(i in 1:ncol(mat)) {
	mat2[is.na(mat[,i]), i] <- mean(mat[,i], na.rm = TRUE)
}

##
colnames(mat2) <- paste("V", 1:ncol(mat2), sep="")
mat2 <- data.frame(mat2)
fit <- lm(V1 ~ ., data = mat2[1:17,])



## Explore time
