# CHECK IF THE INITIAL LENGHT OF GERMLINGS DEPENDS ON THE POPULATION. 
# If yes, we have need to take this into account when interpreting the results

# load libraries
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


Lenght<- read.table("clipboard", header=TRUE, sep = "\t")
names(Lenght)

# to categorise the variables
Lenght$Disc<- as.factor(Lenght$Disc)
Lenght$Salinity <- as.factor(Lenght$Salinity)
Lenght$Temperature <- as.factor(Lenght$Temperature)
Lenght$Emersion <- as.factor(Lenght$Emersion)
Lenght$Tank <- as.factor(Lenght$Tank)
Lenght$Initial <- as.numeric(Lenght$Initial)
str(Lenght)


options(contrasts=c(factor="contr.sum", ordered="contr.poly")) # para obtener los mismos resultados que STATISTICA y SPSS utilizando type III SS.
mi.control <- lmerControl(check.conv.grad=.makeCC(action ="ignore", tol=1e-6, relTol=NULL), optimizer="bobyqa", optCtrl=list(maxfun=100000))

# create all the possible models with all the possible combinations
m0 <- lm (Initial ~ Population * Emersion , data=Lenght)
m1 <- lmer(Initial ~ Population * Emersion  + (1|Tank), control=mi.control, data=Lenght)
m2 <- lmer(Initial ~ Population * Emersion  + (Population|Tank), control=mi.control, data=Lenght)

AICc(m0, m1, m2)

#Check the normality (residuals)
hist(residuals(m2), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.004))
curve(dnorm(x, mean=mean(residuals(m2)), sd=sd(residuals(m1))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m2), main="residuos del modelo")
qqline(residuals(m2), col="red", lwd=2)
shapiro.test(residuals(m2)) # There is a lot of non-normality and you see some well-defined outliers, so what we do is remove them and re-run the models. 

# MODEL WITHOUT OUTLIERS
Lenght2<- read.table("clipboard", header=TRUE, sep = "\t")
m3 <- lm (Initial ~ Population * Emersion , data=Lenght2)
m4 <- lmer(Initial ~ Population * Emersion  + (1|Tank), control=mi.control, data=Lenght2)
m5 <- lmer(Initial ~ Population * Emersion  + (Population|Tank), control=mi.control, data=Lenght2)

AICc(m3, m4,m5)

hist(residuals(m5), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.004))
curve(dnorm(x, mean=mean(residuals(m5)), sd=sd(residuals(m5))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m5), main="residuos del modelo")
qqline(residuals(m5), col="red", lwd=2)
shapiro.test(residuals(m5)) # I still have problems of normality, so I ttransform the response variable to log

# Transform the response variable

m6 <- lmer (log(Initial) ~ Population * Emersion + (Population|Tank), control=mi.control, data=Lenght2)

# Check the normality (residuals)
hist(residuals(m6), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.004))
curve(dnorm(x, mean=mean(residuals(m6)), sd=sd(residuals(m2))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m6), main="residuos del modelo")
qqline(residuals(m6), col="red", lwd=2)
shapiro.test(residuals(m6)) #It has not been fully corrected, but we leave it as it is, it is close to 0.05.


# Check the heterocedasticity (residuals)
plot(fitted(m6), residuals(m6), main="�HAY HETEROCEDASTICIDAD?")# with the mixed ones there are not the corrections that there are in the general/generalised ones.
abline(h=0, col="red")
leveneTest(residuals(m6) ~ Population * Emersion, data=Lenght2)# Variance homogeneity OK


# Reduce the model 
m7 <- update(m6, method="ML")
m7 <- update(m6, REML = FALSE)# We adjust the model with ML because then the model comparisons are going to be with Irtest.
m8 <- update(m6, ~. -Population : Emersion)
summary(m8)
Anova(m8, type = 3, test="Chisq")
lrtest(m7,m8) # It comes out significant, so we cannot reduce the model any further. 

summary(m7)
Anova(m7, type = 3, test="Chisq")
r.squaredGLMM(m7)
# Post-hoc comparisons, of the factors found to be significant

require(lsmeans)
lsm <- lsmeans(m7, pairwise ~ Population:Emersion, adjust = "Tukey")
summary(lsm, type = "response")
plot(lsm, by = "Population", intervals = TRUE, type = "response")
lsmip(lsm, Population ~ Emersion, type = "response")
summary(pairs(lsm), type = "response")

library(multcompView)
cld(lsm, Letters=letters, alpha = 0.05, which = 1)
