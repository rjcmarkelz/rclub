####ggplot2 Chapter 4

###4.2 Creating a plot
library(ggplot2)
p <- ggplot(diamonds, aes(carat, price, colour = cut))

###4.3 Layers
p <- p + layer(geom= "point")

p <- ggplot(diamonds, aes(x=carat))
p.2 <- p + layer(
	geom="bar",
	geom_params = list(fill = "steelblue"),
	stat="bin",
	stat_params = list(binwidth =2)
	)
p.2

p <- ggplot(diamonds, aes(x=carat))
geom_histogram(binwidth =2, fill = "steelblue")

ggplot(msleep, aes(sleep_rem / sleep_total, awake)) + geom_point() + geom_smooth()
####Equivalent qplot
qplot(sleep_rem / sleep_total, awake, data = msleep) + geom_smooth()

p <- ggplot(msleep, aes(sleep_rem / sleep_total, awake))
summary(p)

bestfit <- geom_smooth(method = "lm", 
	se = F,
	colour = "steelblue",
	alpha = 0.5, 
	size = 2)

qplot(sleep_rem, sleep_total, data = msleep) + bestfit
qplot(awake, brainwt, data= msleep, log = "y") + bestfit
qplot(bodywt, brainwt, data= msleep, log= "xy") + bestfit

####4.4 Data
p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()
p

mtcars <- transform(mtcars, mpg = mpg ^2)
p %+% mtcars

###4.5 Aesthetic mappings
aes( x = weight, y = height, colour = age)
aes(weight, height, colour = sqrt(age))


#####4.5.1 Plots and layers
p <- ggplot(mtcars)
summary(p)

p <- p + aes(wt, hp)
summary(p)

p <- ggplot(mtcars, aes(x=mpg, y=wt))
p + geom_point()
p + geom_point(aes(colour = factor(cyl)))
p + geom_point(aes(y = disp))

####4.5.2 Setting vs. mapping
p <-ggplot(mtcars, aes (mpg, wt))
p + geom_point(colour= "darkblue")
p + geom_point(aes(colour="darkblue"))

###4.5.3 Grouping
library(nlme)

####Multiple groups, one aesthetic.
p <- ggplot(Oxboys, aes (age, height, group=Subject)) + geom_line()
p 
p <- ggplot(Oxboys, aes (age, height, group=1)) + geom_line()
p

####Different groups on different layers.
p + geom_smooth(aes(group=Subject), method="lm", se = F)
p + geom_smooth(aes(group=1), method="lm", size = 2, se = F)

####Overriding the default grouping.
boysbox <- ggplot(Oxboys, aes(Occasion, height)) + geom_boxplot()
boysbox
boysbox + geom_line(aes(group =Subject), colour = "#3366FF")

####4.5.4 Matching aesthetics to graphic objects
xgrid <- with(df, seq(min(x), max(x), length = 50))
interp <- data.frame(
	x = xgrid,
	y = approx (df$x, df$y, xout = xgrid)$y,
		colour = approx(df$x, df$colour, xout = xgrid)$y
	)

qplot(x, y, data = df, colour = colour, size = I(5)) + 
geom_line(data = interp, size = 2)

#####4.7 Stat
ggplot(diamonds)ggplot(diamonds, aes(carat)) +
geom_histogram(aes(y = ..density..), binwidth = 0.1)

qplot(carat, ..density.., data = diamonds, geom="histogram",binwidth = 0.1)
####4.9 Pulling it all together
####4.9.1
d <- ggplot(diamonds, aes(carat)) + xlim(0, 3)
d + stat_bin(aes(ymax = ..count..), binwidth = 0.1, geom = "area")
d + stat_bin(
	aes(size = ..density..), binwidth = 0.1,
	geom = "point", position="identity")

d + stat_bin(
	aes(y = 1, fill = ..count..), binwidth = 0.1, 
	geom = "tile", position="identity"
	)

####4.9.3 Varying aesthetics and data
require(nlme, quit = TRUE, warn.conflicts = FALSE)

model <- lme(height ~ age, data = Oxboys,
	random = ~ 1 + age | Subject)

oplot <- ggplot(Oxboys, aes(age, height, group = Subject)) +
geom_line()

age_grid <- seq(-1, 1, length = 10)
subjects <- unique(Oxboys$Subject)
preds <- expand.grid(age = age_grid, Subject = subjects)
preds$height <- predict(model, preds)

oplot + geom_line(data = preds, colour = "#3366FF", size = 0.4)
Oxboys$fitted <- predict(model)
Oxboys$resid <- with(Oxboys, fitted - height)

oplot %+% Oxboys + aes(y=resid) + geom_smooth(aes(group=1))

model2 <- update(model, height ~ age + I(age ^ 2))
Oxboys$fitted2 <- predict(model2)
Oxboys$resid2 <- with(Oxboys, fitted2 - height)
oplot %+% Oxboys + aes(y = resid2) + geom_smooth(aes(group=1))


####qplot to ggplot
setwd("~/Documents/Classes/QTL Mapping")
data <- read.csv("TomatoR2CSHL.csv")
summary(data) #get a quick summary
head(data) #first few lines
tail(data) #last few lines

#Example 1
qplot(leafleng, leafwid, data=data, geom="point", 
	ylab= "Leaf Width (mm)", xlab="Leaf Length (mm)", asp=1) + geom_density2d()

ggplot(data, aes(x= leafleng, y=leafwid)) + 
	xlab("Leaf Length (mm)") +
	ylab("Leaf Width (mm)") +
	geom_point() + 
	geom_density2d() + 
	theme(aspect.ratio = 1)

#Example 2
qplot(petleng, hyp, data=data, 
	ylab= "Hypocotyl Length (mm)", 
	xlab="Petiole Length (mm)",
	colour=trt, geom="density2d", asp=1)

ggplot(data, aes(x= petleng, y=hyp, colour=trt)) + 
	xlab("Petiole Length (mm)") +
	ylab("Hypocotyl Length (mm)") +
	geom_density2d() + 
	theme(aspect.ratio = 1)
	
#Example 3
qplot(petleng, totleng, ylab= "Total Length (mm)", 
	xlab="Petiole Length (mm)",
	data=data, , linetype=trt, 
	geom="density2d", asp=1)

ggplot(data, 
	aes(x= petleng, y=totleng,linetype=trt)) + 
	xlab("Petiole Length (mm)") +
	ylab("Total Length (mm)") +
	geom_density2d() + 
	theme(aspect.ratio = 1) 
	
#Example 4
qplot(leafleng, leafwid, 
	ylab= "Leaf Width (mm)", 
	xlab="Leaf Length (mm)",
	data=data, facets = .~trt, 
	colour=who, geom="density2d", asp=1)

ggplot(data, aes(x= leafleng, y=leafwid,colour=who)) + 
	xlab("Leaf Length (mm)") +
	ylab("Leaf Width (mm)") +
	geom_density2d() + 
	theme(aspect.ratio = 1) +
	facet_grid(.~trt)

#Example 5
qplot(species, hyp, data=data, xlab="Species", ylab= "Hypocotyl Length (mm)",
	geom="boxplot", asp=.5) 

ggplot(data, 
	aes(x= species, y=hyp)) + 
	xlab("Species") +
	ylab("Hypocotyl Length (mm)") +
	geom_boxplot() + 
	theme(aspect.ratio = .5)

#Example 6
qplot(species, hyp, data=data, xlab="Species", ylab= "Hypocotyl Length (mm)",
	geom="boxplot", outlier.shape= NA, asp=.5) 

ggplot(data, 
	aes(x= species, y=hyp)) + 
	xlab("Species") +
	ylab("Hypocotyl Length (mm)") +
	geom_boxplot(outlier.shape= NA) + 
	theme(aspect.ratio = .5)

#Example 7
qplot(species, hyp, data=data, xlab="Species", ylab= "Hypocotyl Length (mm)",
	geom = c("jitter", "boxplot"), asp=.5) 

ggplot(data, 
	aes(x= species, y=hyp)) + 
	xlab("Species") +
	ylab("Hypocotyl Length (mm)") +
	geom_jitter() +
	geom_boxplot() + 
	theme(aspect.ratio = .5)

#Example 8
qplot(species, intleng, data=data, xlab="Species", ylab= "Internode Length (mm)",
	geom = c("violin", "point"))

ggplot(data, 
	aes(x= species, y=intleng)) + 
	xlab("Species") +
	ylab("Internode Length (mm)") +
	geom_violin() + 
	geom_point(size= 0.3) +
	theme(aspect.ratio = 1)

#Example 9
qplot(trt, intleng, data=data, 
	xlab="Treatment", ylab= "Internode Length (mm)",
	geom= "violin", colour=species) 

ggplot(data, 
	aes(x= trt, y=intleng, colour=species)) + 
	xlab("Light Treatment") +
	ylab("Internode Length (mm)") +
	geom_violin() + 
	theme(aspect.ratio = 0.75) +
	scale_colour_discrete(name="Species")


####I realized after I finished that this only works with 
#### a certain viewer window size! o well! See attached PDF.
x_coord <- c(-.25, .25, 0)
y_coord <- c(.25, .25, 0)
shape <- c("cir_1", "cir_2", "diamond")
col <- c("red", "red", "red")
cord <- data.frame(x_coord, y_coord, shape, col)
cord


ggplot(cord, aes(x=x_coord, y=y_coord, shape=shape, fill=col)) + 
geom_point(colour="red", size=c(25, 25, 34.5)) + 
scale_shape_manual(values=c(16,16,18)) +
scale_y_continuous("", limits=c(-3, 3), breaks = NULL) +
scale_x_continuous("To:Sharon   Love:Cody",limits=c(-3, 3), breaks = NULL) +
theme(aspect.ratio=1, legend.position="none") 



########02_14_2013
head(data)
head(seals)
head(diamonds)
head(economics)
head(midwest)
head(movies)
head(mpg)
head(msleep)
head(presidential)
head(volcano)


ggplot(data, aes(x= leafleng, y=leafwid, point=who, colour=who)) + 
	xlab("Leaf Length (mm)") +
	ylab("Leaf Width (mm)") +
	geom_point() +
	geom_density2d() + 
	theme(aspect.ratio = 1) +
	geom_vline(xintercept=38) +
	geom_hline(yintercept=42)

ggplot(data, aes(x=leafleng, y=leafwid, colour=who)) + 
	xlab("Leaf Length (mm)") +
	ylab("Leaf Width (mm)") +
	geom_point() +
	geom_rug() +
	theme(aspect.ratio = 1)


ggplot(data, aes(x=species, y=leafleng)) + 
	xlab("Leaf Length (mm)") +
	ylab("Leaf Width (mm)") +
	geom_point() +
	theme(aspect.ratio = 1)









