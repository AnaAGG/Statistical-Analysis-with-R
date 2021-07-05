
growth <- read.table("clipboard", header = TRUE, sep = "\t", dec=".")
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

growth$Disc<- as.factor(growth$Disc)
growth$Initial <- as.numeric(growth$Initial)
growth$Final <- as.numeric(growth$Final)
str(growth)

options(contrasts=c(factor="contr.sum", ordered="contr.poly")) # para obtener los mismos resultados que STATISTICA y SPSS utilizando type III SS.

#Análisis para saber si hay diferencias significativas entre tratamientos al inicio
#del experimento de "recovery":

m0 <- lm(Initial ~ Population, data=growth)

boxplot(Initial~Population, data=growth)

#Exploración de la normalidad (residuos)
hist(residuals(m0), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.0030))
curve(dnorm(x, mean=mean(residuals(m1)), sd=sd(residuals(m1))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m0), main="residuos del modelo")
qqline(residuals(m0), col="red", lwd=2)
shapiro.test(residuals(m0))#Normalidad OK.

#Exploración de la heterocedasticidad (residuos)
plot(fitted(m0), residuals(m0), main="¿HAY HETEROCEDASTICIDAD?")#con los mixtos no hay las correcciones que hay en los generales/generalizados.
abline(h=0, col="red")
leveneTest(residuals(m0) ~ Population, data=growth)#Homogeneidad de varianzas OK.

summary(m0)
Anova(m0, type=3, test="F")
#Nada significativo.

#Análisis para saber si hay diferencias significativas entre tratamientos al final
#del experimento de "recovery":

m1 <- lm(Final ~ Population, data=growth)

boxplot(Final~Population, data=growth)

#Exploración de la normalidad (residuos)
hist(residuals(m1), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.002))
curve(dnorm(x, mean=mean(residuals(m1)), sd=sd(residuals(m1))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m1), main="residuos del modelo")
qqline(residuals(m1), col="red", lwd=2)
shapiro.test(residuals(m1))#Normalidad OK.

#Exploración de la heterocedasticidad (residuos)
plot(fitted(m1), residuals(m1), main="¿HAY HETEROCEDASTICIDAD?")#con los mixtos no hay las correcciones que hay en los generales/generalizados.
abline(h=0, col="red")
leveneTest(residuals(m1) ~ Population, data = growth)#Homogeneidad de varianzas OK.

summary(m1)
Anova(m1, type=3, test="F")
#Nada significativo.

