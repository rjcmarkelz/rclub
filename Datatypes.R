setwd("~/Documents/Maloof Lab/R_club")
elf3 <- read.csv("ELF3s_bolting_Rclub.csv", header=TRUE) #ask Mike about this
elf3 <- na.omit(elf3)
summary(elf3)
str(elf3)

#1
#What data types are represented in each column?
#Plant- Factor
#diameter- Factor
#n_leaves- Integer(a subset of numeric)
#flowering_time- Integer
#treatment- Factor
#genotype- Factor
#transformation- Factor
#flat- Integer
#col- Factor
#row- Integer

#2
#a) Are there any columns that you think have the wrong data type?
#Yes
#b) Which ones?
#c) Why?
#Plant- these are the individual plants, not factors, should be character
#diameter- this is diameter of each plant, these should be numeric
#flat- this could be a factor you want to include in your statistical model
#row- this could potentially be used as a factor combined with column 
#     to see if there were "edge" effects in trays?

#3
#How would you change the columns to their correct types?
elf3$Plant <- as.character(elf3$Plant)
is.character(elf3$Plant)

elf3$diameter <- as.numeric(as.character(elf3$diameter))
####NA's introduced by coercion.

is.numeric(elf3$diameter)

elf3$flat <- as.factor(elf3$flat)
is.factor(elf3$flat)

elf3$row <- as.factor(elf3$row)
is.factor(elf3$row)


#4
#Are there any obvious mistakes in this data frame beyond what you might
#have found in answering Q3?
summary(elf3)
#it appears that there are double the amount of Ba1 transformations (138)
#and no Ba2 transformations compared to the ~70 of the others

#also there is one data point with 52 leaves that took 53 days to flower
#this point is suspect because it is 5X the mean

#5
#Make the "Sha" genotype the reference level for the genotype column
levels(elf3$genotype)
elf3$genotype <- relevel(elf3$genotype, ref = "Sha")
levels(elf3$genotype)

#6
#Change the order of levels in "trasformation" to be Sh1, Sh2, Sh3, Ba1, Ba3
levels(elf3$transformation)
elf3$transformation <- factor(elf3$transformation, 
	levels= c("Sh1", "Sh2", "Sh3", "Ba1", "Ba3"))

elf3[order(elf3$transformation),]
levels(elf3$transformation)

#7
#What are the id variables and measure variables in the Tomato data set?

tomato <- read.csv("TomatoR2CSHL.csv", header=TRUE)
tomato <- na.omit(tomato)
summary(tomato)
str(tomato)
head(tomato)

#id--- shelf, flat, col, row, trt, days, date, lat, lon, alt, species, ndvi,
# who
#measure--- hyp, int1, int2, int3, int4, inleng, totleng, petleng, leafleng,
# leafwid, leafnum

#8
#Subset the tomato data set to keep the int1-int4 measurements
# and the relevant metadata.
library(plyr)
library(reshape)

tomato
tomato.2 <- tomato[, c("shelf", "flat", "col", "row", "trt", "days",
                               "date", "lat", "lon", "alt", "species", 
                               "ndvi", "who", "int1", "int2", "int3", "int4"
                               )
                    ]
str(tomato.2)

tomato.2$flat <- as.factor(tomato.2$flat)
is.factor(tomato.2$flat)

tomato.2$row <- as.factor(tomato.2$row)
is.factor(tomato.2$row)

tomato.2$days <- as.factor(tomato.2$days)
is.factor(tomato.2$days)

tomato.2$lat <- as.factor(as.character(tomato.2$lat))
is.factor(tomato.2$lat)

tomato.2$lon <- as.factor(as.character(tomato.2$lon))
is.factor(tomato.2$lon)

tomato.2$ndvi <- as.factor(tomato.2$ndvi)
is.factor(tomato.2$ndvi)

#9
#Without melting or casting your new data frame, 
# calculate the mean of each internode. 
dim(tomato.2)
int.means <- apply(tomato.2[14:17], 2, mean)
##OR##
mean(tomato.2$int1)
mean(tomato.2$int2)


#10
#Melt the new data frame
tomato.melt <- melt(tomato, 
	                id.vars = c("shelf", "flat", "col", "row", "trt", "days",
                               "date", "lat", "lon", "alt", "species", 
                               "ndvi", "who"),
	                measure.vars = c("int1", "int2", "int3", "int4"),
	                variable_name = "internode")
head(tomato.melt)

#11
# Means of each internode using cast
cast(tomato.melt, internode ~ ., mean)

#12
# Use cast to obtain the mean for each internode for each species.
cast(tomato.melt, internode ~ species, mean)

#13
# Use cast to obtain the mean for each internode for 
# each species under each treatment.
boxplot <- cast(tomato.melt, internode ~ species ~ trt, mean)
boxplot
######START HERE########
####Clean up plot#######
#14
# Create a boxplot for each combination of species, internode, and treatment
boxplot.melt <-melt(boxplot, variable_name = "Internode Length")
boxplot.melt
as.data.frame(as.table(boxplot.melt))

library(ggplot2)
head(boxplot.melt)
base <- ggplot(boxplot.melt, aes(trt, value))
base + 
facet_grid(internode ~ species) + 
geom_boxplot() +
ggtitle("") +
xlab("Light Treatment") +
ylab("Internode length (mm)") +
theme(aspect.ratio = 0.5)














#Learning the Reshape package
#make a wide dataframe

widedf <- data.frame(
	      wildtype = rnorm(10, 4),
	      phyA = rnorm(10, 6),
	      phyB = rnorm(10, 7),
	      det1 = rnorm(10, 2))
widedf

#use the reshape package to change wide format into long format
library(plyr)
library(reshape)


longdf <- melt(widedf)
head(longdf)
tail(longdf)

#adding a "plate" value to the dataframe
widedf$plate <- rep(c("A", "B"), each = 5)
widedf

#now melt by using id.vars with the column name
longdf <- melt(widedf, id.vars = "plate")
head(longdf)

#or specify the measurement variables
longdf <- melt(widedf, measure.vars = c("wildtype", "phyA", "phyB", "det1"))

#or by using column numbers
longdf <- melt(widedf, measure.vars = 1:4)
head(longdf)

#adding a column name for a variable
longdf <- melt(widedf, id.vars = "plate", variable_name = "genotype")
head(longdf)


#check out other functions of melt with arrays and lists
?melt

#it will melt an array or list so you can use cast on it
array.test <- array(1:24, c(2, 3, 4))
array.test
melt(array.test)
melt(array.test, varnames = c("X", "Y", "Z"))
dimnames(array.test) <- lapply(dim(array.test), function(x) LETTERS[1:x])
array.test
melt(array.test)
melt(array.test, varnames = c("X", "Y", "Z"))
dimnames(array.test)[1] <- list(NULL)
array.test
melt(array.test)

a.list <- as.list(1:4)
melt(a.list)
names(a.list) <- letters[1:4]
a.list
melt(a.list)
##etc...

#cast is used for data in the long format
#summerize the mean
longdf
cast(longdf, plate ~ genotype, mean)

#if you change the order you change the orientation of the output
cast(longdf, genotype ~ plate, mean)

#specify for margin means
cast(longdf, plate ~ genotype, mean, margins = T)

#or if you want a count of one factor
cast(longdf, plate ~ .)

# or the means of one factor
cast(longdf, plate ~ ., mean)
cast(longdf, genotype ~ ., mean)



















































