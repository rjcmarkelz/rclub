###### JAN 31 2013
myData <- c(93, 45, 67, 23)
length(myData)

std.error <- function(x) {
  sd(x)/length(x)
  #function to calculate se of the mean
  #x should be a vector of numbers
  #returns a single number
}
std.error(myData)

myData.2 <- c(93, 45, 67, 23, NA, NA)
length(myData.2)
#6
std.error(myData.2)
#4.996295
##std.error <- function(x) sd(x)/length(x na.rm=T)
std.error.2 <- function(x) sd(x, na.rm=T)/length(x)
std.error.2(myData.2)
##7.494442

sem <- function(x, na.rm=F){
  if(na.rm==T) {
    x <-x[!is.na(x)]
  }
  sd(x)/sqrt(length(x))
}

myData <-c(10, 20, NA, 30)
sem(myData)
sem(myData, na.rm=T)


###Chapter 2 ggplot2
library(ggplot2)

set.seed(1410)
dsmall <-diamonds[sample(nrow(diamonds),100),]
qplot(carat, price, data = diamonds)
qplot(log(carat), log(price), data = diamonds)
qplot(carat, x * y * z, data = diamonds)

qplot(carat, price, data = dsmall, colour = color)
qplot(carat, price, data = dsmall, shape = cut)

qplot(carat, price, data = diamonds, alpha = I(1/10))
qplot(carat, price, data = diamonds, alpha = I(1/100))
qplot(carat, price, data = diamonds, alpha = I(1/200))

qplot(carat, price, data = dsmall, geom = c("point", "smooth"), se=FALSE)
qplot(carat, price, data = diamonds, geom = c("point", "smooth"))

qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
      span = 0.2)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
      span = 1)

library(mgcv)
qplot(carat, price, data=dsmall, geom=c("point", "smooth"), method= "gam", formula= y ~ s(x))
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
      method = "gam", formula = y ~ s(x, bs = "cs"))

library(splines)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
      method = "lm")
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
      method = "lm", formula = y ~ ns(x,5))

install.packages("MASS")
library(MASS)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
      method = "rlm", formula = y ~ ns(x,5))

qplot(color, price / carat, data = diamonds, geom = "jitter",
      alpha = I(1 / 5))
qplot(color, price / carat, data = diamonds, geom = "jitter",
      alpha = I(1 / 50))
qplot(color, price / carat, data = diamonds, geom = "jitter",
      alpha = I(1 / 200))
qplot(color, price / carat, data = diamonds, geom = "jitter",
      size=50, alpha = I(1 / 50))

qplot(carat, data = diamonds, geom = "histogram", binwidth = .1,
      xlim = c(0,3))


qplot(carat, data = diamonds, geom = "histogram", colour="grey")
qplot(carat, data = diamonds, geom = "density")

####SEE REMAINDER OF EXAMPLES

qplot(x = alt, y = totleng, data=data, geom= c("point", "smooth"), method="lm")

qplot(alt, totleng, data=data, geom= c("point", "smooth"), method="lm", formula = y ~ x, colour=trt)

qplot(alt, totleng, data=data, geom= c("point", "smooth"), method="lm", formula = y ~ x, colour=trt, shape=species)


#####Inclass exercise#
library(ggplot2)
setwd("~/Documents/Classes/QTL Mapping")

#import data
data <- read.csv("TomatoR2CSHL.csv")
summary(data) #get a quick summary
head(data) #first few lines
tail(data) #last few lines

#2.2a Plot a box plot of intleng for each species in the data set
qplot(species, intleng, data=data, geom= "boxplot")

#2.2b use facets to make a separate plot for the two treatement
qplot(species, intleng, facets= trt ~., data=data, geom= "boxplot")

#2.2cif your plots in 2.2b are arranged side by side, change them to be on top of one another. 
#If they are on top of one another, change them to be side-by-side
qplot(species, intleng, facets= .~trt, data=data, geom= "boxplot")

#2.3a, 2.3b, 2.3c, 2.3d You have a sneaking suspicion that there is a relationship between leaf width and leaf length.
# Make a plot showing how width varies as length changes. Looks like we are on to something; 
#however, those axis labels look ugly. Change them to be more informative. 
#Do you think that leaf dimensions are correlated with petiole length? Change the color of the points based on petiole length.
qplot(leafleng, leafwid, data=data, geom = "point", xlab= "Leaf Length (mm)", ylab="Leaf Width (mm)", colour=petleng) + scale_colour_gradient (limits=c(3, 30))

#2.3e You have a talk coming up and you want to impress the audience by making your data look more complex. Use facets to separate the plots into a grid of leaf number by species subplots.
qplot(leafleng, leafwid, data=data, facets= leafnum~species, geom = "point", xlab= "Leaf Length (mm)", ylab="Leaf Width (mm)", colour=petleng) + scale_colour_gradient (limits=c(3, 30))

#2.4  Reproduce the plot in the file "internode4.pdf"
qplot(int4, data=data, facets= species~who, main="Internode 4 Length", geom = "bar", colour=trt, fill=trt) 
?qplot

#2.5 Reproduce the plot in the file "PastedGraphic-3.pdf"
qplot(petleng, data=data, facets= date~species, main="Yet Another Figure", 
  xlab= "Petiole Length (mm)", ylab="Density", geom = "density", colour=who) + labs(colour="Dude")

#Chapter 3
# Read chapter 3. It is a little abstract but I think it is worth starting to become familiar with these terms. �
#Execute the chapter 3 code.
#Answer the following questions, using 1 - 2 sentences each:

#3.1 Define aesthetics as they are used in ggplot2.
#The size, shape, color, etc. attributes of geoms represented on an (x, y) grid.

#3.2 What does it mean to "map" an aesthetic?
#To create new column variable(s) that refer to the (x ,y) point(s). For example, this can be #used to separate rows by 2 categorical variables represented in different colors.

#3.3 Explain scales as they relate to data and aesthetics.
#Scales connect the data points (x,y) and the computer graphical representation.
#Example: Liter values 1:10 could be scaled from 1 (light grey) to 10 (black). 

#3.4 What are geoms?
#Goems are geometric objects that are displayed in ggplot.
#Examples: points, lines, bars.

#Code
qplot(displ, hwy, data = mpg, colour = factor(cyl))
qplot(displ, hwy, data=mpg, facets = . ~ year) + geom_smooth()

#3.6
p <- qplot(displ, hwy, data = mpg, colour = factor(cyl))
p
summary(p) #gives the data structure with all of the 
save(p, file="plot.rdata.chp3")
load("plot.rdata.chp3")
ggsave("plot.chp3.png", width= 6, height = 6)



