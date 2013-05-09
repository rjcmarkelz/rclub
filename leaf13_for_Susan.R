#New script to analzye Daniele's leaf data from M96 lines
#Julin Maloof
#112705
#modified010906 for 3rd experiment
#modified 120710 to merge leaves together and pull out PCA


#not needed any more?
varcomp <- function(lme1) { #where lme1 is a lme model from a lmer call in the lme4 package
  #this function will return variance components for the random effects.
  lmeVC <- VarCorr(lme1)
  out <- sapply(lmeVC@reSumry,
            function(x,scal) scal*x@stdDev,
            scal = ifelse(lmeVC@useScale,lmeVC@scale,1))
  out <- c(out,lmeVC@scale)
  names(out)[length(names(out))] <- "Residual"
  unlist(out)^2
}


modelcheck <- function(model,h=8,w=10.5) {# because plot(lmer.obj) doesn't work
	rs <- residuals(model)
	fv <- fitted(model)
	quartz(h=h,w=w)
	plot(rs~fv)
	quartz(h=h,w=w)
	plot(sqrt(abs(rs))~fv)
	dev.new(h=h,w=w)
	qqnorm(rs)
	qqline(rs)
	}


plot.coefs <- function(coefs,base="coefs plot",highlight=10) { # plot coefs from rfr experiment
  #coefs should be a data frame or matrix with high response in first column, low response in second
  #column, and low total in third column
  #highlight = reaction norms to highlight
  high <- coefs[,1]
  low <- coefs[,3]
  breaks <- seq(floor(min(c(high,low))),
              ceiling(max(c(high,low)*1.1)),
                by=signif((ceiling(max(c(high,low)*1.1))-
                  floor(min(c(high,low))))/nclass.Sturges(c(high,low)),1))# breaks for histogram

  hist.mat <- rbind(hist(high,breaks,plot=F)$density,
                  hist(low,breaks,plot=F)$density)

  quartz()
  barplot(hist.mat,beside=T, legend=c("high R/FR","low R/FR"),
        names.arg=breaks[-length(breaks)],main=base)

  sorted.coefs <- coefs[order(coefs[,2]),]#sort based on reponse to low
  high.s <- sorted.coefs[,1]
  low.s <- sorted.coefs[,3]
  
  quartz()
  matplot(rbind(0,1),rbind(high.s,low.s),type="l",ylab=base,lty=1,col=rainbow(95))

  quartz()
  #print reaction norm plot of the top and bottom lines (most and least responsive).
  cutout <- (highlight/2+1):(length(high.s)-highlight/2) #which reaction norms not to print in color
  
  matplot(rbind(0,1),rbind(high.s[cutout],low.s[cutout]),
        xlim=c(0,1.5),type="l",ylab=base, ylim=range(c(high,low)),
        xlab="",col="lightgray",lty=3,xaxt="n",main=paste(base,"reaction norms"))

  matlines(rbind(0,1),rbind(high.s[-cutout],low.s[-cutout]),
        xlim=c(0,1.5),type="l",lwd=2,lty=1:(highlight),col=rainbow(highlight))

  legend("topright",inset=.05,legend=rownames(sorted.coefs)[-cutout],
       lty=1:highlight,lwd=2,col=rainbow(highlight))

  axis (side=1,at=(0:1),labels=c("high R/FR","low R/FR"))
}


plot.coefs.pdf <- function(coefs,base="coefs plot",highlight=10) { # plot coefs from rfr experiment
  #coefs should be a data frame or matrix with high response in first column, low response in second
  #column, and low total in third column
  #highlight = reaction norms to highlight
  pdf(file=paste(base,"_coef_plots.pdf",sep=""))
  high <- coefs[,1]
  low <- coefs[,3]
  breaks <- seq(floor(min(c(high,low))),
              ceiling(max(c(high,low)*1.1)),
                by=signif((ceiling(max(c(high,low)*1.1))-
                  floor(min(c(high,low))))/nclass.Sturges(c(high,low)),1))# breaks for histogram

  hist.mat <- rbind(hist(high,breaks,plot=F)$density,
                  hist(low,breaks,plot=F)$density)

  barplot(hist.mat,beside=T, legend=c("high R/FR","low R/FR"),
        names.arg=breaks[-length(breaks)],main=base)

  sorted.coefs <- coefs[order(coefs[,2]),]#sort based on reponse to low
  high.s <- sorted.coefs[,1]
  low.s <- sorted.coefs[,3]
  
  matplot(rbind(0,1),rbind(high.s,low.s),type="l",ylab=base,lty=1,col=rainbow(95))

  #print reaction norm plot of the top and bottom lines (most and least responsive).
  cutout <- (highlight/2+1):(length(high.s)-highlight/2) #which reaction norms not to print in color
  
  matplot(rbind(0,1),rbind(high.s[cutout],low.s[cutout]),
        xlim=c(0,1.5),type="l",ylab=base,
        xlab="",col="lightgray",lty=3,xaxt="n",main=paste(base,"reaction norms"))

  matlines(rbind(0,1),rbind(high.s[-cutout],low.s[-cutout]),
        xlim=c(0,1.5),type="l",lwd=2,lty=1:(highlight),col=rainbow(highlight))

  legend("topright",inset=.05,legend=rownames(sorted.coefs)[-cutout],
       lty=1:highlight,lwd=2,col=rainbow(highlight))

  axis (side=1,at=(0:1),labels=c("high R/FR","low R/FR"))
      dev.off()
}


library(lme4)
library(LMERConvenienceFunctions)

#####for experiments 1 and 3
leaf13 <- read.csv("M96leaf13 for Magnus.csv",row.names=1)
head(leaf13)
traits <- names(leaf13)[9:19]
traits

#################
#I am trying to figure out how to combine the leaf 4 and 6 data
#One possibility is to include them both in the same lme call and and used leaf number a a fixed effect.
#I would need to check for leaf number by acc interactions
#for the regression traits I could include them both and then include leaf numer in the regression, if needed.

#first create the dataset
#work with the leaf13 dataset only

attach(leaf13)

leafn <- rep(c(4,6),c(rep(length(plant),2)))
leaf.width <- c(leaf.width.4,leaf.width.6)
petiole <- c(petiole.length.4,petiole.length.6)
blade.length <- c(blade.length.4,blade.length.6)

leaf13comb <- rbind(leaf13[,c(1:7,33)],leaf13[,c(1:7,33)])
leaf13comb <- cbind(leaf13comb,leafn,leaf.width,petiole,blade.length)
leaf13comb$leafn <- factor(leaf13comb$leafn)

rownames(leaf13comb) <- 1:length(leafn)

detach(leaf13)

####12/07/2010
###try using leaf # as an effect

names(leaf13comb)

##Width
lmer.width1OLD <- lmer(leaf.width ~ rfr*leafn + (1|flat2) + (1|exp) + (rfr*leafn|acc),data=leaf13comb)

summary(lmer.width1OLD)
#the leafn random effect is highly correlated with acc and should be removed

lmer.width1 <- lmer(leaf.width ~ rfr*leafn + (1|flat2) + (1|exp) + (rfr|acc),data=leaf13comb)
summary(lmer.width1)

#test flat
lmer.width2 <- update(lmer.width1,.~ . - (1|flat2))
anova(lmer.width1,lmer.width2) #keep flat2 in p < 2.2 e -16

#test exp
lmer.width3 <- update(lmer.width1, .~. - (1|exp))
anova(lmer.width1,lmer.width3) #keep exp in 4.482e-11

#is there and rfr by acc interaction?
lmer.width4 <- update(lmer.width1,.~. - (rfr|acc) + (1|acc))
anova(lmer.width1,lmer.width4) #keep rfr|acc in p = .00012

#test accession on its own
lmer.width5 <- update(lmer.width4,.~. - (1|acc))
anova(lmer.width4, lmer.width5) #keep |acc in, p < 2.2e-16

#fixed effects.
lmer.width6 <- update(lmer.width1,.~.-rfr:leafn)
anova(lmer.width1,lmer.width6) #keep interaction p < 2.2e-16

lmer.width7 <- update(lmer.width1,.~.-rfr -rfr:leafn)
anova(lmer.width1,lmer.width7) #keep rfr p < 2.2e-16

lmer.width8 <- update(lmer.width1,.~.-leafn -rfr:leafn)
anova(lmer.width1,lmer.width7) #keep leafn p < 2.2e-16

lmer.width.final <- lmer.width1
modelcheck(lmer.width.final) #not great at the ends.


##length

lmer.length1OLD <- lmer(blade.length ~ rfr*leafn + (1|flat2) + (1|exp) + (rfr*leafn|acc),data=leaf13comb)

summary(lmer.length1OLD)
#the leafn random effect is highly correlated with acc and should be removed

lmer.length1 <- lmer(blade.length ~ rfr*leafn + (1|flat2) + (1|exp) + (rfr|acc),data=leaf13comb)
summary(lmer.length1)

#test flat
lmer.length2 <- update(lmer.length1,.~ . - (1|flat2))
anova(lmer.length1,lmer.length2) #keep flat2 in p < 2.2 e -16

#test exp
lmer.length3 <- update(lmer.length1, .~. - (1|exp))
anova(lmer.length1,lmer.length3) #keep exp in 2.012e-06

#is there and rfr by acc interaction?
lmer.length4 <- update(lmer.length1,.~. - (rfr|acc) + (1|acc))
anova(lmer.length1,lmer.length4) #keep rfr|acc in p = .01493

#test accession on its own
lmer.length5 <- update(lmer.length4,.~. - (1|acc))
anova(lmer.length4, lmer.length5) #keep |acc in, p < 2.2e-16

#fixed effects.
lmer.length6 <- update(lmer.length1,.~.-rfr:leafn)
anova(lmer.length1,lmer.length6) #keep interaction p < 2.2e-16

lmer.length7 <- update(lmer.length1,.~.-rfr -rfr:leafn)
anova(lmer.length1,lmer.length7) #keep rfr p < 2.2e-16

lmer.length8 <- update(lmer.length1,.~.-leafn -rfr:leafn)
anova(lmer.length1,lmer.length7) #keep leafn p < 2.2e-16

lmer.length.final <- lmer.length1
modelcheck(lmer.length.final) #not great at the ends.

##petiole
hist(leaf13comb$petiole)

lmer.petiole1OLD <- lmer(petiole ~ rfr*leafn + (1|flat2) + (1|exp) + (rfr*leafn|acc),data=leaf13comb)

summary(lmer.petiole1OLD)
#the leafn random effect is highly correlated with acc and should be removed

lmer.petiole1 <- lmer(petiole ~ rfr*leafn + (1|flat2) + (1|exp) + (rfr|acc),data=leaf13comb)
summary(lmer.petiole1)

#test flat
lmer.petiole2 <- update(lmer.petiole1,.~ . - (1|flat2))
anova(lmer.petiole1,lmer.petiole2) #keep flat2 in p < 2.2 e -16

#test exp
lmer.petiole3 <- update(lmer.petiole1, .~. - (1|exp))
anova(lmer.petiole1,lmer.petiole3) #keep exp in 0.039

#is there and rfr by acc interaction?
lmer.petiole4 <- update(lmer.petiole1,.~. - (rfr|acc) + (1|acc))
anova(lmer.petiole1,lmer.petiole4) #keep rfr|acc in p = 2.519e-06

#test accession on its own
lmer.petiole5 <- update(lmer.petiole4,.~. - (1|acc))
anova(lmer.petiole4, lmer.petiole5) #keep |acc in, p < 2.2e-16

#fixed effects.
lmer.petiole6 <- update(lmer.petiole1,.~.-rfr:leafn)
anova(lmer.petiole1,lmer.petiole6) #keep interaction p = 1.-24e-10

lmer.petiole7 <- update(lmer.petiole1,.~.-rfr -rfr:leafn)
anova(lmer.petiole1,lmer.petiole7) #keep rfr p < 2.2e-16

lmer.petiole8 <- update(lmer.petiole1,.~.-leafn -rfr:leafn)
anova(lmer.petiole1,lmer.petiole7) #keep leafn p < 2.2e-16

lmer.petiole.final <- lmer.petiole1
modelcheck(lmer.petiole.final) #not great at the ends.

##blade/petiole

leaf13comb$bp <- leaf13comb$blade/leaf13comb$petiole

hist(leaf13comb$bp) #long right tail
hist(log(leaf13comb$bp)) #nicer
library(car)

lmer.bp1 <- lmer(log(bp) ~ rfr*leafn + (1|flat2) + (1|exp) + (rfr*leafn|acc)  ,data=leaf13comb)

summary(lmer.bp1) # OK

lmer.bp2 <- update(lmer.bp1,.~ . - (1|flat2))
anova(lmer.bp1,lmer.bp2) #keep flat2 in p < 2.2 e-16

lmer.bp3 <- update(lmer.bp1, .~. - (1|exp))
anova(lmer.bp1,lmer.bp3) #experiment n.s. p = .996

lmer.bp4 <- update(lmer.bp3, .~. - (rfr*leafn|acc) + (rfr+leafn|acc))
anova(lmer.bp3,lmer.bp4) #do not need the interaction

lmer.bp5 <- update(lmer.bp4,.~. - (rfr+leafn|acc) + (leafn|acc))
anova(lmer.bp4,lmer.bp5) #keep rfr|acc in p = .00214

lmer.bp6 <- update(lmer.bp4,.~. - (rfr+leafn|acc) + (rfr|acc))
anova(lmer.bp4, lmer.bp6) #keep leafn|acc in, p = 5.62E -05

lmer.bp7 <- update(lmer.bp4, . ~ . - rfr:leafn)
anova(lmer.bp7,lmer.bp4) #lmer.bp4 not preferred p = .7177

lmer.bp8 <- update(lmer.bp7,.~. - (rfr+leafn|acc))
lmer.bp9 <- update(lmer.bp8,.~. + (1|acc))
anova(lmer.bp7, lmer.bp8) #keep |acc in, p < 2.2e-16
anova(lmer.bp8, lmer.bp9) #keep |acc in, p < 2.2e-16

lmer.bp.final <- lmer.bp7

modelcheck(lmer.bp.final) #QQ ugly at high end try boxcox

#calculate box cox paramter
library(car)
bpPower <- powerTransform(leaf13comb$bp~leaf13comb$rfr*leaf13comb$leafn+leaf13comb$acc+leaf13comb$exp+leaf13comb$flat2)
summary(bpPower)

leaf13comb$bpTrans <- bcPower(leaf13comb$bp,coef(bpPower))

hist(leaf13comb$bpTrans) #very good!

lmer.bpTrans1 <- lmer(bpTrans ~ rfr*leafn + (1|flat2) + (1|exp) + (rfr*leafn|acc)  ,data=leaf13comb)

summary(lmer.bpTrans1) # OK

lmer.bpTrans2 <- update(lmer.bpTrans1,.~ . - (1|flat2))
anova(lmer.bpTrans1,lmer.bpTrans2) #keep flat2 in p < 2.2 e-16

lmer.bpTrans3 <- update(lmer.bpTrans1, .~. - (1|exp))
anova(lmer.bpTrans1,lmer.bpTrans3) #experiment n.s. p = 1

lmer.bpTrans4 <- update(lmer.bpTrans3, .~. - (rfr*leafn|acc) + (rfr+leafn|acc))
anova(lmer.bpTrans3,lmer.bpTrans4) #do not need the interaction p = .5912

lmer.bpTrans5 <- update(lmer.bpTrans4,.~. - (rfr+leafn|acc) + (leafn|acc))
anova(lmer.bpTrans4,lmer.bpTrans5) #keep rfr|acc in p = .003

lmer.bpTrans6 <- update(lmer.bpTrans4,.~. - (rfr+leafn|acc) + (rfr|acc))
anova(lmer.bpTrans4, lmer.bpTrans6) #remove leafn|acc  p = .1098

lmer.bpTrans7 <- update(lmer.bpTrans6, . ~ . - rfr:leafn)
anova(lmer.bpTrans7,lmer.bpTrans6) #lmer.bpTrans6 not preferred p = .5754

lmer.bpTrans8 <- update(lmer.bpTrans7,.~. - (rfr|acc))
lmer.bpTrans9 <- update(lmer.bpTrans8,.~. + (1|acc))
anova(lmer.bpTrans7, lmer.bpTrans8) #keep |acc in, p < 2.2e-16

#fixed effects
lmer.bpTrans10 <- update(lmer.bpTrans7,.~.-rfr:leafn)
anova(lmer.bpTrans7,lmer.bpTrans10) #do not keep interaction

lmer.bpTrans11 <- update(lmer.bpTrans7,.~.-rfr -rfr:leafn)
anova(lmer.bpTrans7,lmer.bpTrans11) #keep rfr p < 3.201e-06

lmer.bpTrans12 <- update(lmer.bpTrans7,.~.-leafn -rfr:leafn)
anova(lmer.bpTrans7,lmer.bpTrans12) #keep leafn p < 2.2e-16


lmer.bpTrans.final <- lmer.bpTrans10

modelcheck(lmer.bpTrans.final) #QQ still not great

##leaf/width

leaf13comb$lw <- leaf13comb$blade.length/leaf13comb$leaf.width

hist(leaf13comb$lw) #long right tail
hist(log(leaf13comb$lw)) #nicer

lmer.lw1 <- lmer(log(lw) ~ rfr*leafn + (1|flat2) + (1|exp) + (rfr*leafn|acc)  ,data=leaf13comb)

summary(lmer.lw1) # OK

lmer.lw2 <- update(lmer.lw1,.~ . - (1|flat2))
anova(lmer.lw1,lmer.lw2) #keep flat2 in p = 3.071e-07

lmer.lw3 <- update(lmer.lw1, .~. - (1|exp))
anova(lmer.lw1,lmer.lw3) #experiment stays. p = 0.009741

lmer.lw4 <- update(lmer.lw3, .~. - (rfr*leafn|acc) + (rfr+leafn|acc))
anova(lmer.lw3,lmer.lw4) #do not need the interaction 0.9496

lmer.lw5 <- update(lmer.lw4,.~. - (rfr+leafn|acc) + (leafn|acc))
anova(lmer.lw4,lmer.lw5) #keep leafn|acc in p = .0001935

lmer.lw6 <- update(lmer.lw4,.~. - (rfr+leafn|acc) + (rfr|acc))
anova(lmer.lw4, lmer.lw6) #keep rfr|acc in, p = .002107

lmer.lw7 <- update(lmer.lw4, . ~ . - rfr:leafn)
anova(lmer.lw7,lmer.lw4) #lmer.lw4 not preferred p = .2592

lmer.lw8 <- update(lmer.lw7,.~. - (rfr+leafn|acc))
lmer.lw9 <- update(lmer.lw8,.~. + (1|acc))
anova(lmer.lw7, lmer.lw8) #keep |acc in, p < 2.2e-16
anova(lmer.lw8, lmer.lw9) #keep |acc in, p < 2.2e-16

#fixed effects

lmer.lw10 <- update(lmer.lw7,.~.-rfr:leafn)
anova(lmer.lw10,lmer.lw7) #no interaction p = 1

lmer.lw11 <- update(lmer.lw7,.~.-rfr -rfr:leafn)
anova(lmer.lw11,lmer.lw7) #keep rfr p < 2.168e-05

lmer.lw12 <- update(lmer.lw7,.~.-leafn -rfr:leafn)
anova(lmer.lw12,lmer.lw7) #keep leafn p < 2.2e-16

lmer.lw.final <- lmer.lw10

modelcheck(lmer.lw.final)

###Principal components
leaf13complete <- leaf13[complete.cases(leaf13[,c(9:11,13:15)]),]
#leaf13sub <- na.omit(leaf13[,c(9:11,13:15,17)])
leaf13sub <- leaf13complete[,c(9:11,13:15)]#no leaf angle


summary(leaf13sub)
pairs(leaf13sub)
dim(leaf13sub)
pc <- prcomp(leaf13sub,scale.=T,na.action=na.omit)
plot(pc)
biplot(pc,pch=1)
pc
summary(pc)

pc.values <- predict(pc)
dim(pc.values)

leaf13complete <- cbind(leaf13complete,pc.values)

names(leaf13complete)

library(ggplot2)

for(i in names(leaf13complete)[20:25]) {
  #comment next line for Rstudio
	#dev.new()
	print(qplot(x=PC1,y=get(i),colour=rfr,pch=rfr,data=leaf13complete,ylab=i) + scale_colour_manual(values=c("black","red")))
	}
	
for(i in names(leaf13complete)[20:25]) {
  #comment next line for Rstudio
	#dev.new()
	print(qplot(x=rfr,y=get(i),colour=rfr,pch=rfr,data=leaf13complete,ylab=i,geom="boxplot") + scale_colour_manual(values=c("black","red")))
	}
	
#which PCs are sig by RFR?
for(i in names(leaf13complete)[20:25]) {
	print(i)
	print(t.test(get(i) ~ rfr,data=leaf13complete))
	} #The first 4, but Only the first 2 have ~ large differences in the means...stick with those
	
##PC1

hist(leaf13complete$PC1)

lmer.pc1.1OLD <- lmer(PC1 ~ rfr + (1|flat2) + (1|exp) + (rfr|acc)  ,data=leaf13complete)

summary(lmer.pc1.1OLD) # rfr and acc random effects correlated

#why?  

pairs(leaf13complete[c(9:16,20:25)]) #strong correlation between PC1 and size

#also look at weighting of PC...it is very clear
pc

lmer.pc1.1 <- lmer(PC1 ~ rfr + (1|flat2) + (1|exp) + (1|acc)  ,data=leaf13complete)

summary(lmer.pc1.1)

lmer.pc1.2 <- update(lmer.pc1.1,.~ . - (1|flat2))
anova(lmer.pc1.1,lmer.pc1.2) #keep flat2 in p < 2.2 e-16

lmer.pc1.3 <- update(lmer.pc1.1, .~. - (1|exp))
anova(lmer.pc1.1,lmer.pc1.3) #experiment in;  p = 9.05e-07

lmer.pc1.4 <- update(lmer.pc1.3,.~. -  (1|acc))
anova(lmer.pc1.3, lmer.pc1.4) #keep 1|acc in, p < 2.2e-16

lmer.pc1.final <- lmer.pc1.1

modelcheck(lmer.pc1.final)

##PC2

hist(leaf13complete$PC2)

lmer.pc2.1 <- lmer(PC2 ~ rfr + (1|flat2) + (1|exp) + (rfr|acc)  ,data=leaf13complete)

summary(lmer.pc2.1) # OK

lmer.pc2.2 <- update(lmer.pc2.1,.~ . - (1|flat2))
anova(lmer.pc2.1,lmer.pc2.2) #keep flat2 in p < 2.2 e-16

lmer.pc2.3 <- update(lmer.pc2.1, .~. - (1|exp))
anova(lmer.pc2.1,lmer.pc2.3) #experiment out;  p = .28

lmer.pc2.4 <- update(lmer.pc2.3,.~. - (rfr|acc) + (1|acc))
anova(lmer.pc2.1,lmer.pc2.4) #keep rfr|acc in p = 0.005898

lmer.pc2.5 <- update(lmer.pc2.4,.~. -  (1|acc))
anova(lmer.pc2.5, lmer.pc2.4) #keep 1|acc in, p < 2.2e-16

lmer.pc2.final <- lmer.pc2.3

modelcheck(lmer.pc2.final)


##############################
#extract coefs
#for 13  models:



extract.list <- ls(pattern="final")



rm(coefs13)
for (fit in extract.list){
    coefs.tmp.orig <- coef(get(fit))$acc
    
    coefs.tmp <- cbind(coefs.tmp.orig[,1:2],apply(coefs.tmp.orig[,1:2],1,sum)) # high, low.response, and high + low
    colnames(coefs.tmp) <- paste(rep(fit,3),c("h","low.resp","h+l"),sep=".")
    
    if (length(coefs.tmp.orig)>2) { #there was a leaf6 * acc interaction
    		#columns 1 and 3 have leaf4 intercept + the leaf 6 effect = leaf6 in high
    		#sum all columns to get leaf6 in low
    	coefs.tmp <- cbind(coefs.tmp, apply(coefs.tmp.orig[,c(1,3)],1,sum), apply(coefs.tmp.orig,1,sum))
    	colnames(coefs.tmp)[1:3] <- paste(colnames(coefs.tmp)[1:3],rep("leaf4",3),sep=".")
    	colnames(coefs.tmp)[4:5] <- paste(rep(fit,2),c("h","h+l"),rep("leaf6",2),sep=".")
    	}
    if (fit==extract.list[1]) {         # first loop
      coefs13  <- coefs.tmp
    } else {                            #coefs13 exists
      coefs13 <- merge(coefs13,coefs.tmp,all=T,by="row.names")
      rownames(coefs13) <- coefs13[,1]
      coefs13 <- coefs13[,-1]
    }
  #  plot.coefs(coefs.tmp,fit)
    plot.coefs.pdf(coefs.tmp,fit) 
}#for

write.csv(coefs13,"coefs13.csv")

#######
##BROKEN NEEDS TO BE FIXED
##Feb 15 working again?
#fixed effect p.values
#take these from model1, which has full effects
model1.list <- ls(pattern=("[[:alpha:].]1{1}$"))
p.values <- matrix(ncol=length(model1.list)*2,nrow=3)

colnames(p.values) <- paste(rep(model1.list,each=2),
			rep(c("terms","p.value"),length(model1.list)),sep=".")
			
for (i in seq(2,length(model1.list)*2,by=2)){
	fit <- model1.list[i/2]
	tmp.p <- pamer.fnc(get(fit),ndigits=18)
	p.values[1:dim(tmp.p)[1],i-1] <- rownames(tmp.p)
	p.values[1:dim(tmp.p)[1],i] <- tmp.p$Upper #the p.value
	}

p.values[p.values==0] <- 2.20e-16
p.values

write.table(p.values,"fixedEffectPValues.csv",sep=",",row.names=F)

#extract variance components and calculate heritibilities
extract.list <- ls(pattern=glob2rx("lmer*final"))
for (fit in extract.list){
  vc.tmp <- data.frame(summary(get(fit))@REmat[,1:3],stringsAsFactors=F)
  h2.tmp <-round(sum(as.numeric(vc.tmp$Variance[grep("^acc$|^$",vc.tmp$Groups)]))/
  					sum(as.numeric(vc.tmp$Variance)),3)
  vc.tmp <- data.frame(Variance=vc.tmp[,3],row.names=paste(vc.tmp$Groups,vc.tmp$Name,sep="."))
  if (fit==extract.list[1]) {         # first loop
    varcomp13 <- as.matrix(vc.tmp)
    colnames(varcomp13) <- fit
    h2.13 <- vector()
    h2.13[fit] <- h2.tmp
  } else { #varcomp13 and h2.13 exist
    varcomp13 <- merge(varcomp13,vc.tmp,all=T,by="row.names")
    rownames(varcomp13) <- varcomp13[,1]
    varcomp13 <- varcomp13[,-1]
    colnames(varcomp13)[length(colnames(varcomp13))] <- fit
    h2.13[fit] <- h2.tmp
  } #else
}#for

write.csv(varcomp13,"varcomp13NEW.csv")

write.csv(h2.13,"h2.13NEW.csv")

#################################################




