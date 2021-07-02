# DENSITY DEPENDENCE ANALYSIS STRESS GREMLINGS USING LINEAR REGRESSION

# Load libraries
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
library(phia) ## para tests a posteriori y an�lisis de interacciones
library(Matrix)
library(lme4)     ## generalized mixed models
library(lmerTest) ## to MS, df, p ... usando type 3/type 1 hypotheses with "Satterthwaite" and "Kenward-Roger"
library(pbkrtest) ## needed for lmerTest
library(car)      ## to Anova(model, type=3)  equivalent to sum tyope III; to boxCox(model, lambda=seq(-2,2, 1/100))
library(MuMIn)    ## para AICc
library(lmtest)   ## para lrtest
library(phia)     ## to plots with interactions
library(lattice)  ## plot residuals
library(LMERConvenienceFunctions) ## to tests post hoc
library(blmeco)
library(nlme)

# Load data
Supr <- read.table("clipboard", header=TRUE, sep = "\t")
attach(Supr)
names(Supr) # returns the names of a data object

options(contrasts=c(factor="contr.sum", ordered="contr.poly")) # to get the sme results that STATISTICA and SPSS using type III
mi.control <- lmerControl(check.conv.grad=.makeCC(action ="ignore", tol=1e-6, relTol=NULL), optimizer="bobyqa", optCtrl=list(maxfun=100000))


#Performa a linear regression

m1<- lm(Supervivencia~Densidad.inicial, data= Supr)
m2<- lmer(Supervivencia~Densidad.inicial + (1|Disco), data= Supr)

AICc(m1,m2) # get the AICc values 

# Check the data normality(residuals)
hist(residuals(m1), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.05))
curve(dnorm(x, mean=mean(residuals(m1)), sd=sd(residuals(m1))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m1), main="residuos del modelo")
qqline(residuals(m1), col="red", lwd=2)
shapiro.test(residuals(m1))# There is a lot of lack of normality and I can see any defined outlier

# As the data are not normal I tranform the response variable to log and check again the normality
m3 <- lm (Log ~Densidad.inicial, data= Supr)

#Check the data normality(residuals)
hist(residuals(m3), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.05))
curve(dnorm(x, mean=mean(residuals(m3)), sd=sd(residuals(m3))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m3), main="residuos del modelo")
qqline(residuals(m3), col="red", lwd=2)
shapiro.test(residuals(m3))#no more problems of normality

# Check heterocedasticity
plot(fitted(m3), residuals(m3), main="�HAY HETEROCEDASTICIDAD?")# with the mixed models there are not the corrections that there are in the general/generalised ones
abline(h=0, col="red")
leveneTest(residuals(m3) ~ Densidad.inicial, data=Supr)# Variance homogeneity OK.



summary(m3)
Anova(m3, test="Chisq")
