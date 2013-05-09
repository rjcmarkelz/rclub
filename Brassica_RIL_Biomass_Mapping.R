setwd("~/Documents/Classes/QTL Mapping")
brassica <- read.table("brassica_biomass.csv", 
	header=TRUE, sep = ",", na.strings = ".")
head(brassica)
summary(brassica)

hist(brassica$Taproot)
library(lattice)
histogram(~brassica$Taproot | brassica$Daylength)
histogram(~brassica$Taproot | brassica$Treatment)
histogram(~brassica$Taproot | brassica$temp)
histogram(~brassica$Taproot | brassica$block)

library(lme4)

Brlme1 <- lmer(Taproot ~ Treatment + 
	(1|Line) + (1|block),data=brassica)
summary(Brlme1)

Brlme2 <- lmer(Taproot ~ Treatment + (1|block),data=brassica)
summary(Brlme2)

anova(Brlme1,Brlme2) 
                 #compare the two models with a likelihood ratio test (LRT)
                 #this result shows that the more complex model that includes
                 #the line effect fits better than the simple model, so we 
                 #keep the line effect in our model.  This is equivalent to
                 #saying that there is an important genotype effect

Brlme3 <- lmer(Taproot ~ Treatment + (1|Line),data=brassica)
summary(Brlme3)

anova(Brlme1,Brlme3)
####Sig block effect, leave it in the model

Brlme4 <- lmer(Taproot ~ (1|Line) + (1|block),data=brassica)
summary(Brlme4)

anova(Brlme1,Brlme4)
####Marginal treatment differences so we will leave it in the model


library(languageR) #the package that does the resampling
?pvals.fnc
Brlme1.pvals <- pvals.fnc(Brlme1) #takes a minute or two
Brlme1.pvals$fixed #pay attention to the pMCMC column for pvals.
           #we are also given 95% confidence intervals for the mean of the fixed effects
Brlme1.pvals$random

Brlme1.var <- summary(Brlme1)@REmat #REmat refers to the Random Effects matrix
Brlme1.var

head(Brlme1.var)



















#####A few general linear models just to look through the dataset
Brlm5 <- lm(Taproot ~ Treatment, data=brassica)
summary(Brlm5)

Brlm6 <- lm(Taproot ~ temp, data=brassica)
summary(Brlm6)

Brlm7 <- lm(Taproot ~ Line, data=brassica)
summary(Brlm7)

head(brassica)

Brlm8 <- lm(totalbiomass~Treatment, data=brassica)
summary(Brlm8)

Brlm9 <- lm(totalbiomass~Line, data=brassica)
summary(Brlm9)

Brlm10 <- lm(stemheight~Treatment, data=brassica)
summary(Brlm10)

Brlm11 <- lm(abbiomass~Treatment, data=brassica)
summary(Brlm11)

Brlm12 <- lm(m2leafarea~Treatment, data=brassica)
summary(Brlm12)

Brlm13 <- lm(leafarea~Treatment, data=brassica)
summary(Brlm13)
















