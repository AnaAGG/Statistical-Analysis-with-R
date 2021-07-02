# Análisis de densodependencia en reclutas, en estres. 

Supr <- read.table("clipboard", header=TRUE, sep = "\t")
attach(Supr)
names(Supr)

library(car)        ## para obtener VIF's y usar Anova, bootCase, y transformación Box-Cox
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
library(phia) ## para tests a posteriori y análisis de interacciones
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


options(contrasts=c(factor="contr.sum", ordered="contr.poly")) # para obtener los mismos resultados que STATISTICA y SPSS utilizando type III SS.
mi.control <- lmerControl(check.conv.grad=.makeCC(action ="ignore", tol=1e-6, relTol=NULL), optimizer="bobyqa", optCtrl=list(maxfun=100000))


#Hacemos una regresion lineal:

m1<- lm(Supervivencia~Densidad.inicial, data= Supr)
m2<- lmer(Supervivencia~Densidad.inicial + (1|Disco), data= Supr)

AICc(m1,m2)

#Exploración de la normalidad (residuos)
hist(residuals(m1), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.05))
curve(dnorm(x, mean=mean(residuals(m1)), sd=sd(residuals(m1))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m1), main="residuos del modelo")
qqline(residuals(m1), col="red", lwd=2)
shapiro.test(residuals(m1))#Hay mucha falta de normalidad y no se ve ningún outlier bien definido.

m3 <- lm (Log ~Densidad.inicial, data= Supr)

#Exploración de la normalidad (residuos)
hist(residuals(m3), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.05))
curve(dnorm(x, mean=mean(residuals(m3)), sd=sd(residuals(m3))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m3), main="residuos del modelo")
qqline(residuals(m3), col="red", lwd=2)
shapiro.test(residuals(m3))#Hay mucha falta de normalidad y no se ve ningún outlier bien definido.

#Exploración de la heterocedasticidad (residuos)
plot(fitted(m3), residuals(m3), main="¿HAY HETEROCEDASTICIDAD?")#con los mixtos no hay las correcciones que hay en los generales/generalizados.
abline(h=0, col="red")
leveneTest(residuals(m3) ~ Densidad.inicial, data=Supr)#Homogeneidad de varianzas OK.



summary(m3)
Anova(m3, test="Chisq")
