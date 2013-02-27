## Load data
load("../data/fell.Rdata")

## Simple EDA
plot(fell$date, fell$visits, col=ifelse(fell$post, "blue", "orange"))
tapply(fell$visit, fell$post, mean)
boxplot(fell$visit ~ fell$post)
t.test(fell$visit ~ fell$post)