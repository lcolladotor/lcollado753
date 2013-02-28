### Script for scoring the controversy of the post titles

## Load data. It build up on previous add_controversy.R runs
file <- dir(".", "posts.controversy.Rdata")
if(length(file) == 1) {
	load(file)
} else {
	load("simplified.Rdata")
}


if(FALSE) {
	posts <- head(posts)
}


## Specify column name. For example, leo1
col.name <- as.character(readline("Enter the name of the new column for your controversy ranking of the post titles. Preferably without spaces. "))

i <- sample(1:length(posts$title))
res <- rep(NA, length(i))
for(j in 1:length(i)) {
	print(paste0("Title ", j, "/", length(i)))
	msg <- paste("How controversial is this title:", posts$title[i[j]])
	print(msg)
	rank <- as.integer(readline("Enter your controversy ranking. 1 to 5 (integers). 5 is very controversial. "))
	res[j] <- rank
}
print("You are done!")
new <- data.frame(res)
colnames(new) <- col.name
posts <- cbind(posts, new)
save(posts, file="posts.controversy.Rdata", compress="gzip")

print("Look at your controversy rankings")
barplot(table(new))