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


lenjuv <- read.table("clipboard", header = TRUE, sep = "\t")
#Convierto salinity y temperature y Emersion  en factores.
lenjuv$Tank<- as.factor(lenjuv$Tank)
lenjuv$Population<- as.factor(lenjuv$Population)
lenjuv$Salinity<- as.factor(lenjuv$Salinity)
lenjuv$Temperature<- as.factor(lenjuv$Temperature)
lenjuv$Emersion<- as.factor(lenjuv$Emersion)
lenjuv$Weight <- as.numeric(lenjuv$Weight)
str(lenjuv)

options(contrasts=c(factor="contr.sum", ordered="contr.poly")) # para obtener los mismos resultados que STATISTICA y SPSS utilizando type III SS.
mi.control <- lmerControl(check.conv.grad=.makeCC(action ="ignore", tol=1e-6, relTol=NULL), optimizer="bobyqa", optCtrl=list(maxfun=100000))
mi.control <- lmeControl(opt='optim') # este es el bueno

m0 <- gls(Weight ~ Population * Emersion * Salinity * Temperature, data=lenjuv)
m1 <- lme(Weight~ Population * Emersion * Salinity * Temperature, random = ~1|Tank, data=lenjuv, control=mi.control)
m2 <- lme(Weight~ Population * Emersion * Salinity * Temperature, random = ~Population|Tank, data=lenjuv, control=mi.control)

anova(m0,m1,m2)

0.5*(1-pchisq(0.056382,1))
0.5*((1-pchisq(18.071757 ,1)) + (1-pchisq(18.071757 ,9)))


# pesos de los modelos comparados
Weights(AICc(m0, m1))
## ¿cuántas veces es mejor un modelo que otro?
## las comparaciones relevantes podrían ser, colocando primero el modelo con menor AICc:
#El mejor modelo es el m2 que no es el que nosotros habíamos diseñado, por lo que vamos a ver cuanto más bueno es:
exp(-0.5*(AICc(m0)-AICc(m1)))
#Aunque el modelo 2 resulta ser 7160 veces mejor que el modelo 3 (de nuestro diseño) 
#vamos a comparar los dos modelos:

boxplot(Weight~Population, data=lenjuv)
boxplot(Weight~Emersion, data=lenjuv)
boxplot(Weight~Salinity, data=lenjuv)
boxplot(Weight~Temperature, data=lenjuv)


#Exploración de la normalidad (residuos)
hist(residuals(m0), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,7))
curve(dnorm(x, mean=mean(residuals(m0)), sd=sd(residuals(m0))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m0), main="residuos del modelo")
qqline(residuals(m0), col="red", lwd=2)
shapiro.test(residuals(m2))#Hay mucha falta de normalidad y no se ve ningún outlier bien definido.


#Transformamos la variable respuesta:

m3 <- lme(log(Weight) ~ Population * Emersion * Salinity * Temperature, random = ~Population|Tank, control = mi.control, data=lenjuv)

#Exploración de la normalidad (residuos) para ver si se soluciona la falta de normalidad
hist(residuals(m3), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,7))
curve(dnorm(x, mean=mean(residuals(m3)), sd=sd(residuals(m0))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m3), main="residuos del modelo")
qqline(residuals(m3), col="red", lwd=2)
shapiro.test(residuals(m3)) #Normalidad OK

#Exploración de la heterocedasticidad (residuos)
plot(fitted(m3), residuals(m3), main="¿HAY HETEROCEDASTICIDAD?")#con los mixtos no hay las correcciones que hay en los generales/generalizados.
abline(h=0, col="red")
leveneTest(residuals(m3) ~ Population*Emersion*Salinity*Temperature, data=lenjuv) #Hay un poco de heterocedasticidad, pero lo dejamos asi. 

summary(m3)
Anova(m3, type=3, test="Chisq")

#Reducimos modelo:
library(MuMIn)
m4 <- update(m3, method = "ML")#Ajustamos el modelo con ML porque luego las comparaciones de modelos van a ser con lrtest.
options(na.action = "na.fail")
sg<- dredge(m3, rank="AICc", fixed = ~Population|Tank) #Evaluamos todos los posibles modelos, con todas las posibles combinaciones de factores fijos, fijando el factor aleatorio. 
subset(sg, delta < 2, recalc.weights = TRUE)
importance(subset(model.sel(sg), delta <= 2))
GermlingsSizeMuminresults<- write.csv(sg, "Juvenile Size-MuMIn Preliminar results(all pop).csv")


#Post-hoc test:
require(emmeans)

#para la interaccion de Temperature x Emersion

lsm1 <- emmeans(m4, pairwise ~ Temperature:Emersion, adjust = "Tukey")
summary(lsm1, type = "response")
summary(pairs(lsm1), type = "response")

CLD(lsm1, Letters=letters, alpha = 0.05, which = 1)

#para la interaccion de Population x Emersion

lsm2 <- emmeans(m4, pairwise ~ Population:Emersion, adjust = "Tukey")
summary(lsm2, type = "response")
summary(pairs(lsm2), type = "response")

CLD(lsm2, Letters=letters, alpha = 0.05, which = 1)

