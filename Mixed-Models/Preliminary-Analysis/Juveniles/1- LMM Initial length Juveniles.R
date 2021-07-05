## PRELIMINARY ANALYSIS FOR JUVENILES USING MIXED LINEAR MODELS
## The goal of this analysis is check if there are differences on the juvenile lenght before start the experiment
# If yes, when drawing conclusions, we have to consider that there were already differences between the treatments. 

library(car)        ## para obtener VIF's y usar Anova, bootCase, y transformaci�n Box-Cox
library(MASS)       ## para usar dropterm y rlm
library(MuMIn)      ## para calcular AICc
library(zoo)
library(lmtest)     ## para usar coeftest y correcciones por heterocedasticidad
library(sandwich)   ## para correcciones de heterocedasticidad usando vcovHC
library(heplots)    ## para la estima de los valores de partial eta2
library(moments)    ## para obtener el sesgo, kurtosis y sus significaciones: kurtosis, anscombe.test, skewness, agostino.test
library(fit.models) ## para usar el comando leverage
library(psych)      ## para construir tablas con describe
library(DAAG)       ## para hacer validaciones cruzadas del modelo usando cv.lm
library(robust)     ## para hacer estimas robustas usando rlm
library(robustbase) ## para hacer estimas robustas usando lmrob
library(phia) ## para tests a posteriori y an�lisis de interacciones
library(Matrix)
library(lme4)     ## generalized mixed models
library(lmerTest) ## para MS, df, p ... usando type 3/type 1 hypotheses with "Satterthwaite" and "Kenward-Roger"
library(pbkrtest) ## necesario para lmerTest
library(car)      ## para Anova(modelo, type=3) para equivalente a suma de tipo III; para boxCox(modelo, lambda=seq(-2,2, 1/100))
library(MuMIn)    ## para AICc
library(lmtest)   ## para lrtest
library(psych)    ## para tabla de promedios con describe y describeBy
library(arm)      ## para correr simulaciones con modelos lm, glm, polr o merMod
library(phia)     ## para plots de interacciones
library(lattice)  ## para plots de residuos
library(LMERConvenienceFunctions) ## para tests post hoc
library(blmeco)
library(nlme)

# Load data, 2 possible options
lenjuv <- read.table("clipboard", header = TRUE, sep = "\t") # sep = "\t" because the data is in an Excel
lenjuv2 <- read.table("FILENAME.csv", header = True, sep = "\t")

# convert emersion and seawater temperature and salinity to categorical variables
lenjuv$Tank<- as.factor(lenjuv$Tank)
lenjuv$Population<- as.factor(lenjuv$Population)
lenjuv$Salinity<- as.factor(lenjuv$Salinity)
lenjuv$Temperature<- as.factor(lenjuv$Temperature)
lenjuv$Emersion<- as.factor(lenjuv$Emersion)
lenjuv$Weight <- as.numeric(lenjuv$Weight)
str(lenjuv)

options(contrasts=c(factor="contr.sum", ordered="contr.poly")) # to get the same results thath STATISTICA or SPSS using type III error
mi.control <- lmeControl(opt='optim')

# fit different models to check with is the best combination of fixed and random factors for our data
m0 <- gls(Weight ~ Population * Emersion * Salinity * Temperature, data=lenjuv)
m1 <- lme(Weight~ Population * Emersion * Salinity * Temperature, random = ~1|Tank, data=lenjuv, control=mi.control) # tank as random
m2 <- lme(Weight~ Population * Emersion * Salinity * Temperature, random = ~Population|Tank, data=lenjuv, control=mi.control) # interaction of population and tank as random


anova(m0,m1,m2)
0.5*(1-pchisq(0.056382,1))
0.5*((1-pchisq(18.071757 ,1)) + (1-pchisq(18.071757 ,9)))


# weights of the compared models with AICc
Weights(AICc(m0, m1))

## How often is one model better than another
## the relevant comparisons could be, placing the model with the lowest AICc first:
## the best model is the m2 which is not the one we talked about designed, so let's see how much better it is:
exp(-0.5*(AICc(m0)-AICc(m1)))

#Aunque el modelo 2 resulta ser 7160 veces mejor que el modelo 3 (de nuestro dise�o) 
#vamos a comparar los dos modelos:

boxplot(Weight~Population, data=lenjuv)
boxplot(Weight~Emersion, data=lenjuv)
boxplot(Weight~Salinity, data=lenjuv)
boxplot(Weight~Temperature, data=lenjuv)


#Exploraci�n de la normalidad (residuos)
hist(residuals(m2), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,7))
curve(dnorm(x, mean=mean(residuals(m2)), sd=sd(residuals(m2))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m2), main="residuos del modelo")
qqline(residuals(m2), col="red", lwd=2)
shapiro.test(residuals(m2))# There is many lack of normality and I can't see any outlier, so the next step is transform the response variable (log transformation)


# Transform the response variable 
m3 <- lme(log(Weight) ~ Population * Emersion * Salinity * Temperature, random = ~Population|Tank, control = mi.control, data=lenjuv)

# Check the normality (residuals) to see if the normality problems have been solved
hist(residuals(m3), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,7))
curve(dnorm(x, mean=mean(residuals(m3)), sd=sd(residuals(m0))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m3), main="residuos del modelo")
qqline(residuals(m3), col="red", lwd=2)
shapiro.test(residuals(m3)) # Normality OK

#Check heterocedasticity
plot(fitted(m3), residuals(m3), main="�HAY HETEROCEDASTICIDAD?")#con los mixtos no hay las correcciones que hay en los generales/generalizados.
abline(h=0, col="red")
leveneTest(residuals(m3) ~ Population*Emersion*Salinity*Temperature, data=lenjuv) #Hay un poco de heterocedasticidad, pero lo dejamos asi. 

summary(m3)
Anova(m3, type=3, test="Chisq")

# Reduce the model

m4 <- update(m3, method = "ML")# we adjust the model with ML because then the model comparison are going to be with lrtest
options(na.action = "na.fail")
sg<- dredge(m3, rank="AICc", fixed = ~Population|Tank) # I evaluate all the possible models, with all the possible combinations of fixed factos, keep constant the random factor
subset(sg, delta < 2, recalc.weights = TRUE)
importance(subset(model.sel(sg), delta <= 2))
GermlingsSizeMuminresults<- write.csv(sg, "Juvenile Size-MuMIn Preliminar results(all pop).csv") # to save the MuMin results in a csv


#Post-hoc test:
require(emmeans)

# To the Emersion x Temperature interaction

lsm1 <- emmeans(m4, pairwise ~ Temperature:Emersion, adjust = "Tukey")
summary(lsm1, type = "response")
summary(pairs(lsm1), type = "response")

CLD(lsm1, Letters=letters, alpha = 0.05, which = 1)

# To the Population x Emersion interation
lsm2 <- emmeans(m4, pairwise ~ Population:Emersion, adjust = "Tukey")
summary(lsm2, type = "response")
summary(pairs(lsm2), type = "response")

# make a compact letter display for pai-wise comparison
CLD(lsm2, Letters=letters, alpha = 0.05, which = 1)

