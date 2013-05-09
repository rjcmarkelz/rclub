
#Question 1
#Describe (in your own words) the different purposes of layers in a ggplot.

#Answer 1
#Layers are used in ggplot to succesively add colour, shapes, sizes to lines, points, and boxplots
# as a way to display raw data. Additional layers can perform statistical transformations (e.g. smoothing), or 
# subset the raw data to only display a portion (e.g. faceting). 


#Question 2
#Woe is me, I have too much data and my plots are overwhelming to look at. 
#Give me 7 tips (in your own words) for dealing with my problem.

#Answer 2
#(1) If your dataset is overwhelming to look at it is likely you have 
    #not thought about exactly what the data is suppose to tell the reader
#(2) Think about the significant relationship(s) in the data set that support/refute your hypothesis and start here.
#(3) Start by asking yourself what it is that you want to plot to convey? Build up from here.
#(4) Think about the simpliest way to display the relationships (whether through regression, bar charts, etc. in gray scale)
#(5) If these tried and true methods are still not enough, add new shapes to group the data
#(6) Often simple is better, but if you find that simple plots cannot get the job done, try adding some colour to highlight a treament
#(7) If a simple colour palette with different shapes still does not get the point across, 
#(8) Bonus: Send an email to mfcovington@ucdavis.edu, I hear he solves everyone's problems.
#(9) Double-Bonus: Skip steps 1-7 and send an email immediately to mfcovington@ucdavis.edu .
#(10)Triple-Bonus: Go immediately to Mike's office and demand to know how to draw and interpret your own data.

library(ggplot2)
library(ggmap)
library(mapproj)

NAM <- read.csv("NAM_lat_long_data.csv")
head(NAM)

nam_map <- get_map(location= c(lon = 30, lat = 35),
				zoom=3,
               maptype = 'satellite')

ggmap(nam_map) + 
geom_point(colour='red', size= 3, aes(x=long, y=lat), data=NAM) +
geom_text(data = NAM, aes(x = long, y = lat, label = Ecotype_name), 
          size = 5, vjust = 0, hjust = -0.25, colour='red') +
theme(aspect.ratio = 1) +
labs(title="Arabidopsis NAM Population Parental Origins",
			x="Longitude", y="Latitude")


#####Chapter 5 From Feb 28 2013
#5.3 Basic Plot Types
df <- data.frame(
x = c(3, 1, 5),
y = c(2, 4, 6),
label = c("a","b","c")
)
p <- ggplot(df, aes(x, y, label = label)) +
xlab(NULL) + ylab(NULL)
p + geom_point() + opts(title = "geom_point")
p + geom_bar(stat="identity") +
opts(title = "geom_bar(stat=\"identity\")")
p + geom_line() + opts(title = "geom_line")
p + geom_area() + opts(title = "geom_area")
p + geom_path() + opts(title = "geom_path")
p + geom_text() + opts(title = "geom_text")
p + geom_tile() + opts(title = "geom_tile")
p + geom_polygon() + opts(title = "geom_polygon")

#5.4 Displaying distributions
depth_dist <- ggplot(diamonds, aes(depth)) + xlim(58, 68)

depth_dist +
geom_histogram(aes(y = ..density..), binwidth = 0.1) +
facet_grid(cut ~ .)

depth_dist + geom_histogram(aes(fill = cut), binwidth = 0.1,
position = "fill")

depth_dist + geom_freqpoly(aes(y = ..density.., colour = cut),
binwidth = 0.1)

qplot(cut, depth, data=diamonds, geom="boxplot")
qplot(carat, depth, data=diamonds, geom="boxplot",
group = round_any(carat, 0.1, floor), xlim = c(0, 3))

qplot(class, cty, data=mpg, geom="jitter")
qplot(class, drv, data=mpg, geom="jitter")

qplot(depth, data=diamonds, geom="density", xlim = c(54, 70))
qplot(depth, data=diamonds, geom="density", xlim = c(54, 70),
fill = cut, alpha = I(0.2))

#5.5 Dealing with overplotting
df <- data.frame(x = rnorm(2000), y = rnorm(2000))
norm <- ggplot(df, aes(x, y))
norm + geom_point()
norm + geom_point(shape = 1)
norm + geom_point(shape = ".") # Pixel sized

#alphablending is transparency
norm + geom_point(alpha = 1/3) #dark
norm + geom_point(alpha = 1/5) #lighter
norm + geom_point(alpha = 1/10) #even lighter

td <- ggplot(diamonds, aes(table, depth)) +
xlim(50, 70) + ylim(50, 70)
td + geom_point()
td + geom_jitter()
jit <- position_jitter(width = 0.5)
td + geom_jitter(position = jit)
td + geom_jitter(position = jit, colour = "black", alpha = 1/10)
td + geom_jitter(position = jit, colour = "black", alpha = 1/50)
td + geom_jitter(position = jit, colour = "black", alpha = 1/200)

####Install hexbin package
d <- ggplot(diamonds, aes(carat, price)) + xlim(1,3) +
opts(legend.position = "none")
d + stat_bin2d()
d + stat_bin2d(bins = 10)
d + stat_bin2d(binwidth=c(0.02, 200))
d + stat_binhex()
d + stat_binhex(bins = 10)
d + stat_binhex(binwidth=c(0.02, 200))

#####5.7 Drawing maps
#install maps package
library(maps)
data(us.cities)
big_cities <- subset(us.cities, pop > 500000)
qplot(long, lat, data = big_cities) + borders("state", size = 0.5)

tx_cities <- subset(us.cities, country.etc == "TX")
ggplot(tx_cities, aes(long, lat)) +
borders("county", "texas", colour = "grey70") +
geom_point(colour = "black", alpha= 0.5)

states <- map_data("state")
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))
choro <- merge(states, arrests, by = "region") #reorder rows
choro <- choro[order(choro$order), ]
qplot(long, lat, data = choro, group = group, 
	fill = assault, geom = "polygon")
qplot(long, lat, data = choro, group = group,
    fill = assault / murder, geom = "polygon")

#load plyr package
library(plyr)
ia <- map_data("county", "iowa")
mid_range <- function(x) mean(range(x, na.rm = TRUE))
centres <- ddply(ia, .(subregion), colwise(mid_range, .(lat, long)))

ggplot(ia, aes(long, lat)) +
  geom_polygon(aes(group = group),
  fill = NA, colour = "grey60") +
  geom_text(aes(label = subregion), data = centres,
  size = 2, angle = 45
  )


#5.8 revealing uncertainty
d <- subset(diamonds, carat < 2.5 &
rbinom(nrow(diamonds), 1, 0.2) == 1)
d$lcarat <- log10(d$carat)
d$lprice <- log10(d$price)

 # Remove overall linear trend
 detrend <- lm(lprice ~ lcarat, data = d)
 d$lprice2 <- resid(detrend)

mod <- lm(lprice2 ~ lcarat * color, data = d)

library(effects)
effectdf <- function(...) {
  suppressWarnings(as.data.frame(effect(...)))
  }
color <- effectdf("color", mod)
both1 <- effectdf("lcarat:color", mod)
 carat <- effectdf("lcarat", mod, default.levels = 50)
 both2 <- effectdf("lcarat:color", mod, default.levels = 3)

 ###The packages multcomp and multcompView are useful calculating and displaying these errors while correctly adjusting for
### multiple comparisons.

#5.9 Statistical Summaries-----This appears to not work.
midm <- function(x) mean(x, trim = 0.5)
ggplot(m2)
+
stat_summary(aes(colour = "trimmed"), fun.y = midm, geom = "point") + 
stat_summary(aes(colour = "raw"), fun.y = mean, eom = "point") +
scale_colour_hue("Mean")

####5.10 Annotating a plot ----This also appears to be buggy
(unemp <- qplot(date, unemploy, data=economics, geom="line",
     xlab = "", ylab = "No. unemployed (1000s)"))

presidential <- presidential[-(1:3), ]
yrng <- range(economics$unemploy)
xrng <- range(economics$date)
unemp + 
geom_vline(aes(xintercept = as.numeric(start)), data = presidential) +




unemp + 
geom_rect(aes(NULL, NULL, xmin = start, xmax = end, 
                 fill= party, ymin =yrng[1], ymax = yrng[2],
                 data = presidential)

  )
scale_fill_manual(values = c("blue", "red"))



###5.11 Weighted Data
qplot(percwhite, percbelowpoverty, data = midwest)
qplot(percwhite, percbelowpoverty, data = midwest,
size = poptotal / 1e6) + scale_size_area("Population\n(millions)",
breaks = c(0.5, 1, 2, 4))
qplot(percwhite, percbelowpoverty, data = midwest, size = area) +
scale_size_area()

lm_smooth <- geom_smooth(method = lm, size = 1)
qplot(percwhite, percbelowpoverty, data = midwest) + lm_smooth
qplot(percwhite, percbelowpoverty, data = midwest,
weight = popdensity, size = popdensity) + lm_smooth

qplot(percbelowpoverty, data = midwest, binwidth = 1)

########HOMEWORK REPLICATE PLOTS!!!!!!!

setwd("~/Documents/Classes/QTL Mapping")

#import data
data <- read.csv("TomatoR2CSHL.csv")
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

#Challenge 1- Completed
ggplot(movies, aes(x= year, y=rating)) + 
	scale_fill_gradientn(colours= rainbow(20)) +
    stat_binhex(colour= "black", aes(fill= ..density..)) 

#Challenge 2- Completed in class

head(movies)
movies.mpaa.new <- subset(movies, mpaa != "" & year >= 1990)

#set things in your geom and not in the base
base <- ggplot(movies.mpaa.new, 
			aes(x = year, 
				fill = mpaa))

density <- geom_density(alpha = 1/2) #make sure to apply this alpha to a given geom
label <- labs(title= "The Density Distribution of MPAA Rated Films from 1990-2005")
theme.custom <- theme(legend.position = "bottom")

base + density + label + theme.custom




#Challenge 3 - Completed
ggplot(data,aes(x= alt, y=hyp, colour=trt)) +
opts(title= "TOMATO DATA IN WORDS")+
xlab("ALTITUDE") +
ylab("HYPOCOTYL LENGTH") +
geom_point()+
geom_text(angle= 45, aes(label=species, size=leafnum)) +
facet_grid(.~who) +
theme(aspect.ratio = 2)

#####In Class-- Notice tilted legend
ggplot(tomato, aes(x     = alt,
                   y     = hyp,
                   size  = leafnum,
                   label = species,
                   color = trt)) +
  geom_text(angle = 30) +
  facet_grid(. ~ who) +
  labs(title = "TOMATO DATA IN WORDS",
       x     = "ALTITUDE",
       y     = "HYPOCOTYL LENGTH")


#Challenge 4 - Completed (mine)
ggplot(data, aes(x=leafleng, y=leafwid, colour=who)) + 
	xlab("Leaf Length (mm)") +
	ylab("Leaf Width (mm)") +
	geom_point() +
	geom_rug() +
	theme(aspect.ratio = 1)



#Challenge 5 -Completed
ggplot(mpg, aes(x=class, y=hwy, 
	colour= manufacturer, fill=manufacturer)) + 
	xlab("Class") +
	ylab("Highway (mpg)") +
	geom_dotplot(binaxis = "y", stackdir = "center", binpositions="all")+
	theme(aspect.ratio = 0.5, 
		axis.text.x = element_text(angle = 45, hjust = 1))

#Challenge 6 - Completed
library(ggmap)
map <- get_map(location = c(lon = -75, lat = -16),
               zoom = 5,
               maptype = 'satellite')

ggmap(map) + 
facet_grid(.~species) +
geom_point(aes(x=lon, y=lat, colour=alt), data=data) +
scale_colour_gradient(low="white", high="red")
theme(aspect.ratio = 2)


ggmap(map) +
  geom_point(aes(x      = lon,
                 y      = lat,
                 colour = alt),
             data = tomato,
             size = 2) +
  facet_grid(. ~ species) +
  scale_colour_continuous(low  = "white",
                          high = "red")




                          

#Challenge 7 - Not Completed
world <- map_data("world")
worldmap <- ggplot(world, aes(x     = long,
                              y     = lat,
                              group = group)) +
  geom_polygon(fill   = "white",
               colour = "black")
worldmap


worldmap +
  geom_point(aes(x     = lon,
                 y     = lat,
                 group = NULL),
             data  = data,
             size  = 2,
             color = "red")









































