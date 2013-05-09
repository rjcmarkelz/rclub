#2013_04_18
#plyr package--- look at hadley
library(plyr)
setwd("~/Documents/Maloof Lab/R_club/plyr-tutorial/examples")
bnames <- read.csv("bnames.csv", stringsAsFactors = FALSE)
head(bnames)

#Proportion of us children that have a name by in the top 100.
head(bnames, n = 100L)
names(bnames)
ddply(bnames, c("name"), summarise, tot = sum(percent))

bnames.2 <- ddply(bnames, c("year"), head = head(bnames, n = 100L))

bnames.2 <- ddply(bnames[1:100,], ~ year)
tail(bnames.2)

bnames.2 <- ddply(bnames, c("year", "sex"), summarise, tot = sum(percent))