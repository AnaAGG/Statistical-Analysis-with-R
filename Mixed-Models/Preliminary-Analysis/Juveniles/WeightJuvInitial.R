#Análisis preliminares Juvenile weight


library(car)        ## para obtener VIF's y usar Anova, bootCase, y transformación Box-Cox
library(MASS)       ## para usar dropterm y rlm
library(MuMIn)      ## para calcular AICc
library(lmtest)     ## para usar coeftest y correcciones por heterocedasticidad
library(sandwich)   ## para correcciones de heterocedasticidad usando vcovHC
library(heplots)    ## para la estima de los valores de partial eta2
library(moments)    ## para obtener el sesgo, kurtosis y sus significaciones: kurtosis, anscombe.test, skewness, agostino.test
library(fit.models) ## para usar el comando leverage
library(psych)      ## para construir tablas con describe
library(DAAG)       ## para hacer validaciones cruzadas del modelo usando cv.lm
library(robust)     ## para hacer estimas robustas usando rlm
library(robustbase) ## para hacer estimas robustas usando lmrob
library(phia)       ## para tests a posteriori y análisis de interacciones
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

wei<- read.table("clipboard", header=TRUE, sep = "\t")
names(wei)


wei$Salinity <- as.factor(wei$Salinity)
wei$Temperature <- as.factor(wei$Temperature)
wei$Emersion <- as.factor(wei$Emersion)
wei$Tank <- as.factor(wei$Tank)
wei$Initial <- as.numeric(wei$Initial)
str(wei)


options(contrasts=c(factor="contr.sum", ordered="contr.poly")) # para obtener los mismos resultados que STATISTICA y SPSS utilizando type III SS.
mi.control <- lmerControl(check.conv.grad=.makeCC(action ="ignore", tol=1e-6, relTol=NULL), optimizer="bobyqa", optCtrl=list(maxfun=100000))

mi.control <- lmeControl(opt='optim',optimMethod = "SANN")# este es el bueno para que todos los modelos puedan correr, sino el modelo 2 no converge

m0 <- gls(Initial ~ Population, data=wei)
m1 <- lme(Initial ~ Population, random = ~1|Tank, data=wei, control=mi.control)
m2 <- lme(Initial ~ Population, random = ~ Population|Tank, data=wei, control=mi.control)

anova(m0, m1, m2)

#Reajustamos los p valores

0.5*(1-pchisq(0.055865,1))
0.5*((1-pchisq(27.674074,1)) + (1-pchisq(27.674074,10)))

#Exploración de la normalidad (residuos)
hist(residuals(m2), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.004))
curve(dnorm(x, mean=mean(residuals(m2)), sd=sd(residuals(m0))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m2), main="residuos del modelo")
qqline(residuals(m2), col="red", lwd=2)
shapiro.test(residuals(m2))#Hay mucha falta de normalidad y no se ve ningún outlier bien definido.

#Transformamos la variable respuesta:

m3 <- lme (log(Initial) ~ Population, random = ~Population|Tank,  control=mi.control, data=wei)

#Exploración de la normalidad (residuos)
hist(residuals(m3), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.004))
curve(dnorm(x, mean=mean(residuals(m3)), sd=sd(residuals(m3))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m3), main="residuos del modelo")
qqline(residuals(m3), col="red", lwd=2)
shapiro.test(residuals(m3))#NORMALIDAD OK


#Exploración de la heterocedasticidad (residuos)
plot(fitted(m3), residuals(m3), main="¿HAY HETEROCEDASTICIDAD?")#con los mixtos no hay las correcciones que hay en los generales/generalizados.
abline(h=0, col="red")
leveneTest(residuals(m3) ~ Population * Emersion * Temperature * Salinity, data=wei)#Homogeneidad de varianzas OK.


#Reducimos el modelo: seleccion de la parte fija

m4 <- update(m3, method="ML")#Ajustamos el modelo con ML porque luego las comparaciones de modelos van a ser con lrtest
options(na.action = "na.fail")
sg<- dredge(m4, rank="AICc", fixed = ~Population|Tank) #Evaluamos todos los posibles modelos, con todas las posibles combinaciones de factores fijos, fijando el factor aleatorio. 
subset(sg, delta < 2, recalc.weights = TRUE)
GermlingsSizeMuminresults<- write.csv(sg, "Initial Juvenile Weight.csv")
importance(subset(model.sel(sg), delta < 2))


#Pot-hoc
require(emmeans)

#para la interaccion de Temperature x Emersion

lsm1 <- emmeans(m4, pairwise ~ Population, adjust = "Tukey")
summary(lsm1, type = "response")
summary(pairs(lsm1), type = "response")

library(multcompView)
CLD(lsm1, Letters=letters, alpha = 0.05, which = 1)

#para la interaccion de Population x Emersion

lsm2 <- emmeans(m4, pairwise ~ Population:Emersion, adjust = "Tukey")
summary(lsm2, type = "response")
summary(pairs(lsm2), type = "response")

CLD(lsm2, Letters=letters, alpha = 0.05, which = 1)