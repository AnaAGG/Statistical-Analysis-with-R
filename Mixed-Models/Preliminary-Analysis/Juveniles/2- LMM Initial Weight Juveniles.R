## PRELIMINARY ANALYSIS FOR JUVENILES WEIGHT USING MIXED LINEAR MODELS
## The goal of this analysis is check if there are differences on the juvenile weight before start the experiment
# If yes, when drawing conclusions, we have to consider that there were already differences between the treatments. 


library(car)        ## to get VIF's, use ANOVA, bootCase and Box-Cox transformation  
library(MASS)       ## to use  dropterm and rlm
library(MuMIn)      ## to calculate AICc
library(zoo)
library(lmtest)     ## To use `coeftest`and heterocedasticity corrections 
library(sandwich)   ## to correct heterocedasticity problem using vcovHC
library(heplots)    ## to calculate the eta2 partial values 
library(moments)    ## to get the skewness, kurtosis and its significances: kurtosis, anscombe.test, skewness, agostino.test
library(fit.models) ## to use leverage
library(psych)      ## to create descibe tables
library(phia)       ## to post-hoc analysis
library(Matrix)
library(lme4)     ## generalized mixed models
library(lmerTest) ## to MS, df, p ... usando type 3/type 1 hypotheses with "Satterthwaite" and "Kenward-Roger"
library(pbkrtest) ## needed for lmerTest
library(car)      ## to Anova(model, type=3)  equivalent to sum tyope III; to boxCox(model, lambda=seq(-2,2, 1/100))
library(MuMIn)    ## to AICc
library(lmtest)   ## to lrtest
library(phia)     ## to plots with interactions
library(lattice)  ## plot residuals
library(LMERConvenienceFunctions) ## to tests post hoc
library(blmeco)
library(nlme)

# Load data
wei<- read.table("clipboard", header=TRUE, sep = "\t")
names(wei)

# convert emersion and seawater temperature and salinity to categorical variables
wei$Salinity <- as.factor(wei$Salinity)
wei$Temperature <- as.factor(wei$Temperature)
wei$Emersion <- as.factor(wei$Emersion)
wei$Tank <- as.factor(wei$Tank)
wei$Initial <- as.numeric(wei$Initial)
str(wei)


options(contrasts=c(factor="contr.sum", ordered="contr.poly")) # to get the same results thath STATISTICA or SPSS using type III error
mi.control <- lmeControl(opt='optim',optimMethod = "SANN")

# fit different models to check with is the best combination of fixed and random factors for our data
m0 <- gls(Initial ~ Population, data=wei)
m1 <- lme(Initial ~ Population, random = ~1|Tank, data=wei, control=mi.control)
m2 <- lme(Initial ~ Population, random = ~ Population|Tank, data=wei, control=mi.control)

anova(m0, m1, m2)

#Reajustamos los p valores

0.5*(1-pchisq(0.055865,1))
0.5*((1-pchisq(27.674074,1)) + (1-pchisq(27.674074,10)))

#Check normality (residuals)
hist(residuals(m2), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.004))
curve(dnorm(x, mean=mean(residuals(m2)), sd=sd(residuals(m0))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m2), main="residuos del modelo")
qqline(residuals(m2), col="red", lwd=2)
shapiro.test(residuals(m2))# There is a lot of normality lack and I havent see any clear outlier


#Tranform the response variable to log
m3 <- lme (log(Initial) ~ Population, random = ~Population|Tank,  control=mi.control, data=wei)

#Check the normality before the transformation
hist(residuals(m3), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.004))
curve(dnorm(x, mean=mean(residuals(m3)), sd=sd(residuals(m3))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m3), main="residuos del modelo")
qqline(residuals(m3), col="red", lwd=2)
shapiro.test(residuals(m3))#NORMALIDAD OK


#Check heterocedasticity (residuals)
plot(fitted(m3), residuals(m3), main="�HAY HETEROCEDASTICIDAD?")
abline(h=0, col="red")
leveneTest(residuals(m3) ~ Population * Emersion * Temperature * Salinity, data=wei)# Variance homogeneity OK


#Model reduction: fixed part selection
m4 <- update(m3, method="ML")# We adjust the model with ML because then the model comparisons will be with lrtest
options(na.action = "na.fail")
sg<- dredge(m4, rank="AICc", fixed = ~Population|Tank) # I have assessed all the  possible models, with all the possible combinations of fixed factors, keeping the random part
subset(sg, delta < 2, recalc.weights = TRUE)
GermlingsSizeMuminresults<- write.csv(sg, "Initial Juvenile Weight.csv")
importance(subset(model.sel(sg), delta < 2))


#Post-hoc
require(emmeans)

# To the Temperature x Emersion interaction

lsm1 <- emmeans(m4, pairwise ~ Population, adjust = "Tukey")
summary(lsm1, type = "response")
summary(pairs(lsm1), type = "response")

library(multcompView)
CLD(lsm1, Letters=letters, alpha = 0.05, which = 1)

# To the Population x Emersion intereaction

lsm2 <- emmeans(m4, pairwise ~ Population:Emersion, adjust = "Tukey")
summary(lsm2, type = "response")
summary(pairs(lsm2), type = "response")

CLD(lsm2, Letters=letters, alpha = 0.05, which = 1)