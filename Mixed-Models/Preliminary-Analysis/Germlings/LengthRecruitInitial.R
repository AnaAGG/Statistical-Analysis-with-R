#Análisis preliminares longitud inicial de reclutas.


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


Lenght<- read.table("clipboard", header=TRUE, sep = "\t")
names(Lenght)


Lenght$Disc<- as.factor(Lenght$Disc)
Lenght$Salinity <- as.factor(Lenght$Salinity)
Lenght$Temperature <- as.factor(Lenght$Temperature)
Lenght$Emersion <- as.factor(Lenght$Emersion)
Lenght$Tank <- as.factor(Lenght$Tank)
Lenght$Initial <- as.numeric(Lenght$Initial)
str(Lenght)


options(contrasts=c(factor="contr.sum", ordered="contr.poly")) # para obtener los mismos resultados que STATISTICA y SPSS utilizando type III SS.
mi.control <- lmerControl(check.conv.grad=.makeCC(action ="ignore", tol=1e-6, relTol=NULL), optimizer="bobyqa", optCtrl=list(maxfun=100000))

m0 <- lm (Initial ~ Population * Emersion , data=Lenght)
m1 <- lmer(Initial ~ Population * Emersion  + (1|Tank), control=mi.control, data=Lenght)
m2 <- lmer(Initial ~ Population * Emersion  + (Population|Tank), control=mi.control, data=Lenght)

AICc(m0, m1, m2)

#Exploración de la normalidad (residuos)
hist(residuals(m2), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.004))
curve(dnorm(x, mean=mean(residuals(m2)), sd=sd(residuals(m1))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m2), main="residuos del modelo")
qqline(residuals(m2), col="red", lwd=2)
shapiro.test(residuals(m2))#Hay mucha falta de normalidad y se ven algunos outliers bien definidos, lo que hacemos es quitarlos y volver a correr los modelos. 

#modelos sin outliers
Lenght2<- read.table("clipboard", header=TRUE, sep = "\t")
m3 <- lm (Initial ~ Population * Emersion , data=Lenght2)
m4 <- lmer(Initial ~ Population * Emersion  + (1|Tank), control=mi.control, data=Lenght2)
m5 <- lmer(Initial ~ Population * Emersion  + (Population|Tank), control=mi.control, data=Lenght2)

AICc(m3, m4,m5)

hist(residuals(m5), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.004))
curve(dnorm(x, mean=mean(residuals(m5)), sd=sd(residuals(m5))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m5), main="residuos del modelo")
qqline(residuals(m5), col="red", lwd=2)
shapiro.test(residuals(m5))

#Transformamos la variable respuesta:

m6 <- lmer (log(Initial) ~ Population * Emersion + (Population|Tank), control=mi.control, data=Lenght2)

#Exploración de la normalidad (residuos)
hist(residuals(m6), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.004))
curve(dnorm(x, mean=mean(residuals(m6)), sd=sd(residuals(m2))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m6), main="residuos del modelo")
qqline(residuals(m6), col="red", lwd=2)
shapiro.test(residuals(m6))#No se ha corregido del todo, pero lo dejamos asi, esta cerca de 0.05


#Exploración de la heterocedasticidad (residuos)
plot(fitted(m6), residuals(m6), main="¿HAY HETEROCEDASTICIDAD?")#con los mixtos no hay las correcciones que hay en los generales/generalizados.
abline(h=0, col="red")
leveneTest(residuals(m6) ~ Population * Emersion, data=Lenght2)#Homogeneidad de varianzas OK.


#Reducimos el modelo: 

#Reducimos modelo:
m7 <- update(m6, method="ML")
m7 <- update(m6, REML = FALSE)#Ajustamos el modelo con ML porque luego las comparaciones de modelos van a ser con lrtest.
m8 <- update(m6, ~. -Population : Emersion)
summary(m8)
Anova(m8, type = 3, test="Chisq")
lrtest(m7,m8) # Sale significativo, por lo que no podemos seguir reduciendo el modelo. 

summary(m7)
Anova(m7, type = 3, test="Chisq")
r.squaredGLMM(m7)
#Comparaciones posthoc, de los factores que salen significativos

require(lsmeans)
lsm <- lsmeans(m7, pairwise ~ Population:Emersion, adjust = "Tukey")
summary(lsm, type = "response")
plot(lsm, by = "Population", intervals = TRUE, type = "response")
lsmip(lsm, Population ~ Emersion, type = "response")
summary(pairs(lsm), type = "response")

library(multcompView)
cld(lsm, Letters=letters, alpha = 0.05, which = 1)
