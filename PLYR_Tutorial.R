#2013_04_18
#plyr package--- look at hadley
library(plyr)
library(reshape)
library(ggplot2)
setwd("~/Documents/Maloof Lab/R_club/plyr-tutorial/examples")
bnames <- read.csv("bnames.csv", stringsAsFactors = FALSE)
head(bnames, 15)
tail(bnames, 15)
#Proportion of us children that have a name by in the top 100.
head(bnames, n = 100L)
names(bnames)
ddply(bnames, c("name"), summarise, tot = sum(percent))

bnames.2 <- ddply(bnames, c("year"), head = head(bnames, n = 100L))

bnames.2 <- ddply(bnames[1:100,], ~ year)
tail(bnames.2)

bnames.2 <- ddply(bnames, c("year", "sex"), summarise, tot = sum(percent))


#####ddply page 8
letter <- function(x, n = 1){
	if(n < 0) {
		nc <- nchar(x)
		n <- nc + n + 1
	}
	tolower(substr(x, n, n))
}

vowels <- function (x) {
	nchar(gsub("[^aeiou]", "", x))
}

bnames <- transform(bnames,
	first = letter(name, 1),
	last  = letter(name, -1),
	length = nchar(name),
	vowels = vowels(name)
	)

summarise(bnames,
	max_percen = max(percent),
	min_percen = min(percent)
	)

####Group wise transformations
#### The task is hard for complicated slices
one <- subset(bnames, sex == "boy" & year == 2008)
one$rank <- rank(-one$percent, ties.method = "first")
head(one)


#####NON_DDPLY WAY
#split
pieces <- split(bnames, list (bnames$sex, bnames$year))

#apply
results <- vector("list", length(pieces))
for(i in seq_along(pieces)){
	piece <- pieces[[i]]
	piece <- transform(piece,
		rank = rank(-percent, ties.method = "first"))
	results[[i]] <- piece
}

# combine
result <- do.call("rbind", results)

####All of the preceeding code using ddply
bnames <- ddply(bnames, c("sex", "year"), transform,
	rank = rank(-percent, ties.method = "first"))


##other examples
ddply(bnames, c("name"), summarise, tot = sum(percent))
ddply(bnames, c("length"), summarise, tot = sum(percent))
ddply(bnames, c("year", "sex"), summarise, tot = sum(percent))

f1 <- ddply(bnames, c("year", "sex", "first"), summarise, tot = sum(percent))
qplot(year, tot, data = f1, geom = "line", colour = sex, facets = ~ first)
















































