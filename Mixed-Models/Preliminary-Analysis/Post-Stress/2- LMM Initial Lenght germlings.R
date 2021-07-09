# CHECK IF THE INITIAL LENGHT OF GERMLINGS DEPENDS ON THE POPULATION ON THE POST-STRESS EXPERIMENT. 
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



growth <- read.table("clipboard", header = TRUE, sep = "\t", dec=".")

# Re-asign variables to categorical or discrete variables
growth$Disc<- as.factor(growth$Disc)
growth$Initial <- as.numeric(growth$Initial)
growth$Final <- as.numeric(growth$Final)
str(growth)

options(contrasts=c(factor="contr.sum", ordered="contr.poly")) # to get the same results that STATISTICA or SPSS using SS type III

# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# ANALYSIS TO EVALUATE IF THERE ARE SIGNIFICANTIVE DIFFERENCES BETWEEN TREATMENTS AT THE START OF THE EXPERIMENT (RECOVERY EXPERIMENT)


m0 <- lm(Initial ~ Population, data=growth)

boxplot(Initial~Population, data=growth)

# Check the normality (residuals)

hist(residuals(m0), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.0030))
curve(dnorm(x, mean=mean(residuals(m1)), sd=sd(residuals(m1))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m0), main="residuos del modelo")
qqline(residuals(m0), col="red", lwd=2)
shapiro.test(residuals(m0))#Normalidad OK.

# Check the heterocedasticity (residuals)
plot(fitted(m0), residuals(m0), main="�HAY HETEROCEDASTICIDAD?") # with the mixed models there are not corrections that we have in the generalized models
abline(h=0, col="red")
leveneTest(residuals(m0) ~ Population, data=growth)# Homogeneity OK

summary(m0)
Anova(m0, type=3, test="F")
# Nothing significative


# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# ANALYSIS TO EVALUATE IF THERE ARE SIGNIFICANTIVE DIFFERENCES BETWEEN TREATMENTS AT THE END OF THE EXPERIMENT (RECOVERY EXPERIMENT)

m1 <- lm(Final ~ Population, data=growth)

boxplot(Final~Population, data=growth)

# Check normality (residuals)
hist(residuals(m1), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.002))
curve(dnorm(x, mean=mean(residuals(m1)), sd=sd(residuals(m1))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m1), main="residuos del modelo")
qqline(residuals(m1), col="red", lwd=2)
shapiro.test(residuals(m1))# Normality OK.

# Check heterocedasticity (residuals)
plot(fitted(m1), residuals(m1), main="�HAY HETEROCEDASTICIDAD?")# with the mixed models there are not corrections that we have in the generalized models
abline(h=0, col="red")
leveneTest(residuals(m1) ~ Population, data = growth)# Variance homogeneity OK

summary(m1)
Anova(m1, type=3, test="F")
# Nothing significative
