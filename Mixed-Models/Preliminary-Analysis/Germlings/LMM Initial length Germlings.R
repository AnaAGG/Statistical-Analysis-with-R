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

##Análisis PARA AROSA-MUROS

lenger <- read.table("clipboard", header = TRUE, sep = "\t")
#Convierto salinity y temperature en factores: Population y Tank con cuatro niveles, el reto con dos niveles.
lenger$Tank<- as.factor(lenger$Tank)
lenger$Salinity<- as.factor(lenger$Salinity)
lenger$Temperature<- as.factor(lenger$Temperature)
lenger$Emersion<- as.factor(lenger$Emersion)
lenger$Initial <- as.numeric(lenger$Initial)
str(lenger)

options(contrasts=c(factor="contr.sum", ordered="contr.poly")) # para obtener los mismos resultados que STATISTICA y SPSS utilizando type III SS.
mi.control <- lmerControl(check.conv.grad=.makeCC(action ="ignore", tol=1e-6, relTol=NULL), optimizer="bobyqa", optCtrl=list(maxfun=100000))

m0 <- gls(Initial ~Population * Emersion * Temperature * Salinity, data=lenger)
m1 <- lme(Initial~ Population * Emersion * Temperature * Salinity, random = ~1|Tank, data=lenger, control=mi.control)
m2 <- lme(Initial~ Population * Emersion * Temperature * Salinity, random =  ~Population|Tank, data=lenger, control=mi.control) 


anova(m2,m1,m0)

0.5*(1-pchisq(159.06943,2))
0.5*((1-pchisq(91.30448 ,2)) + (1-pchisq(91.30448 ,1)))

# pesos de los modelos comparados
Weights(AICc(m0, m1))
## ¿cuántas veces es mejor un modelo que otro?
## las comparaciones relevantes podrían ser, colocando primero el modelo con menor AICc:
#El mejor modelo es el m2 que no es el que nosotros habíamos diseñado, por lo que vamos a ver cuanto más bueno es:
exp(-0.5*(AICc(m0)-AICc(m1)))
#Aunque el modelo 2 resulta ser 7160 veces mejor que el modelo 3 (de nuestro diseño) 
#vamos a comparar los dos modelos:

boxplot(Initial~Population, data=lenger)
boxplot(Initial~Emersion, data=lenger)
boxplot(Initial~Population * Emersion, data=lenger)

#Exploración de la normalidad (residuos)
hist(residuals(m2), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.002))
curve(dnorm(x, mean=mean(residuals(m2)), sd=sd(residuals(m2))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m2), main="residuos del modelo")
qqline(residuals(m2), col="red", lwd=2)
shapiro.test(residuals(m2))#Hay mucha falta de normalidad y no se ve ningún outlier bien definido. Vamos a dejar los datos con falta de normalidad, xq como son


#Exploración de la heterocedasticidad (residuos)
plot(fitted(m2), residuals(m2), main="¿HAY HETEROCEDASTICIDAD?")#con los mixtos no hay las correcciones que hay en los generales/generalizados.
abline(h=0, col="red")
leveneTest(residuals(m2) ~ Population*Emersion* Temperature* Salinity, data=lenger)#Homogeneidad de varianzas OK.


m3 <- lmer(log(Initial)~ Population * Temperature * Emersion * Salinity + (Population|Tank), data=lenger, control=mi.control)

#Exploración de la normalidad (residuos)
hist(residuals(m3), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.002))
curve(dnorm(x, mean=mean(residuals(m3)), sd=sd(residuals(m3))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m3), main="residuos del modelo")
qqline(residuals(m3), col="red", lwd=2)
shapiro.test(residuals(m3))#Hay mucha falta de normalidad y no se ve ningún outlier bien definido. Vamos a dejar los datos con falta de normalidad, xq como son


#Exploración de la heterocedasticidad (residuos)
plot(fitted(m3), residuals(m3), main="¿HAY HETEROCEDASTICIDAD?")#con los mixtos no hay las correcciones que hay en los generales/generalizados.
abline(h=0, col="red")
leveneTest(residuals(m3) ~ Population*Emersion* Temperature* salinity, data=lenger)#Homogeneidad de varianzas OK.


summary(m3)
Anova(m3, type=3, test="Chisq")

#Reducimos modelo:
library(MuMIn)
m4 <- update(m3, REML = FALSE)#Ajustamos el modelo con ML porque luego las comparaciones de modelos van a ser con lrtest.
options(na.action = "na.fail")
sg<- dredge(m4, rank="AICc", fixed = ~Population|Tank) #Evaluamos todos los posibles modelos, con todas las posibles combinaciones de factores fijos, fijando el factor aleatorio. 
subset(sg, delta < 2, recalc.weights = TRUE)
importance(subset(model.sel(sg), delta <= 2))
GermlingsSizeMuminresults<- write.csv(sg, "Germlings Size-MuMIn Preliminar results (a-m).csv")




##Análisis PARA TODAS LAS POBLACIONES
lenger <- read.table("clipboard", header = TRUE, sep = "\t")
#Convierto salinity y temperature en factores: Population y Tank con cuatro niveles, el reto con dos niveles.
lenger$Tank<- as.factor(lenger$Tank)
lenger$Salinity<- as.factor(lenger$Salinity)
lenger$Temperature<- as.factor(lenger$Temperature)
lenger$Emersion<- as.factor(lenger$Emersion)
lenger$Initial <- as.numeric(lenger$Initial)
str(lenger)

options(contrasts=c(factor="contr.sum", ordered="contr.poly")) # para obtener los mismos resultados que STATISTICA y SPSS utilizando type III SS.
mi.control <- lmerControl(check.conv.grad=.makeCC(action ="ignore", tol=1e-6, relTol=NULL), optimizer="bobyqa", optCtrl=list(maxfun=100000))
mi.control <- lmeControl(opt='optim') # este es el bueno

m0 <- lm(Initial ~ Population * Emersion , na.action = na.omit,  data=lenger)
m1 <- lme(Initial~ Population * Emersion, random = ~1|Tank, data=lenger, control=mi.control)
m2 <- lme(Initial~ Population * Emersion, random = ~Population|Tank, data=lenger, control=mi.control) 


anova(m2, m1,m0)

0.5*(1-pchisq(231.8191,9))
0.5*((1-pchisq(125.3764 ,2)) + (1-pchisq(125.3764 ,1)))


# pesos de los modelos comparados
Weights(AICc(m0, m1))
## ¿cuántas veces es mejor un modelo que otro?
## las comparaciones relevantes podrían ser, colocando primero el modelo con menor AICc:
#El mejor modelo es el m2 que no es el que nosotros habíamos diseñado, por lo que vamos a ver cuanto más bueno es:
exp(-0.5*(AICc(m0)-AICc(m1)))
#Aunque el modelo 2 resulta ser 7160 veces mejor que el modelo 3 (de nuestro diseño) 
#vamos a comparar los dos modelos:

boxplot(Initial~Population, data=lenger)
boxplot(Initial~Emersion, data=lenger)
boxplot(Initial~Population * Emersion, data=lenger)

#Exploración de la normalidad (residuos)
hist(residuals(m2), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.002))
curve(dnorm(x, mean=mean(residuals(m2)), sd=sd(residuals(m2))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m2), main="residuos del modelo")
qqline(residuals(m2), col="red", lwd=2)
shapiro.test(residuals(m2))#Hay mucha falta de normalidad y no se ve ningún outlier bien definido. Vamos a dejar los datos con falta de normalidad, xq como son


#Exploración de la heterocedasticidad (residuos)
plot(fitted(m2), residuals(m2), main="¿HAY HETEROCEDASTICIDAD?")#con los mixtos no hay las correcciones que hay en los generales/generalizados.
abline(h=0, col="red")
leveneTest(residuals(m2) ~ Population*Emersion, data=lenger)#Homogeneidad de varianzas OK.


m3 <- lme(log(Initial)~ Population * Emersion, random = ~Population|Tank, data=lenger, control=mi.control)

#Exploración de la normalidad (residuos)
hist(residuals(m3), density=5, freq=FALSE, main="residuos del modelo", ylim=c(0,0.002))
curve(dnorm(x, mean=mean(residuals(m3)), sd=sd(residuals(m3))), col="red", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(m3), main="residuos del modelo")
qqline(residuals(m3), col="red", lwd=2)
shapiro.test(residuals(m3))#Hay mucha falta de normalidad y no se ve ningún outlier bien definido. Vamos a dejar los datos con falta de normalidad, xq como son


#Exploración de la heterocedasticidad (residuos)
plot(fitted(m3), residuals(m3), main="¿HAY HETEROCEDASTICIDAD?")#con los mixtos no hay las correcciones que hay en los generales/generalizados.
abline(h=0, col="red")
leveneTest(residuals(m3) ~ Population*Emersion* Temperature, data=lenger)#Homogeneidad de varianzas OK.


summary(m3)
Anova(m3, type=3, test="Chisq")

#Reducimos modelo:
library(MuMIn)
m4 <- update(m3, method = "ML")#Ajustamos el modelo con ML porque luego las comparaciones de modelos van a ser con lrtest.
options(na.action = "na.fail")
sg<- dredge(m4, rank="AICc", fixed = ~Population|Tank) #Evaluamos todos los posibles modelos, con todas las posibles combinaciones de factores fijos, fijando el factor aleatorio. 
subset(sg, delta < 2, recalc.weights = TRUE)
importance(subset(model.sel(sg), delta <= 2))
GermlingsSizeMuminresults<- write.csv(sg, "Germlings Size-MuMIn Preliminar results(all pop).csv")

#Reajustamos el mejor modelo (m4) con REML:
m6 <- update(m4, method="REML")
summary(m6)
Anova(m6, type=3, test="Chisq")

#Comparaciones posthoc, de los factores que salen significativos

require(lsmeans)
lsm <- lsmeans(m4, pairwise ~ Population * Emersion, adjust = "Tukey")
summary(lsm, type = "response")
plot(lsm, by = "Population", intervals = TRUE, type = "response")
lsmip(lsm, Population ~ Emersion, type = "response")
summary(pairs(lsm), type = "response")

library(multcompView)
CLD(lsm, Letters=letters, alpha = 0.05, which = 1)

#Para la figura
initialger <- read.table("clipboard", header = TRUE, sep="\t")

ggplot(initialger, aes(x= Population, y= Mean, fill= Emersion)) + geom_bar(position = "dodge", stat = "identity", width = 0.5, colour ="Black") + 
  scale_fill_grey(start = .5, end= .9)

p <- ggplot(initialger, aes(x= Population, y= Mean, fill= Emersion)) + 
  geom_bar(position = "dodge", stat = "identity", width = 0.5, colour="black", size =0.2) + #???size = X para determinar el grosor de las lineas de las barras
  scale_fill_grey(start = .9, end= 1.0) + #para determinar los colores de las barras(Blanco y gris)
  geom_errorbar(aes(ymin= Mean -SE, ymax = Mean + SE),
                position = position_dodge(0.5), width = .0, size = 0.2) + 
  scale_y_continuous(expand = c(0.0,0.0),limits = c(0.0, 1000), breaks = seq(0.0,1000, by = 100)) + 
  geom_text(aes(label = Letters, y = Mean),hjust = "center", vjust = -2, fontface = "bold") + 
  xlab("") + ylab("Germling size (µm)") +
  theme_bw() + #quitamos el fondo gris 
  theme(panel.border= element_blank(),
        panel.grid.major = element_blank(), # para quitar las lineas internas del plot
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black", size = 0.20), #para ver las lineas a lo largo de los ejes, size= 1, el grosor de los ejes
        axis.text.x = element_text(face="plain", color = "black", 
                                   size=10, angle = 0), 
        axis.text.y = element_text(face = "plain", color = "black", 
                                   size= 10, angle = 0)) +
  theme(legend.position = "none")

p 

#para guardar las figuras en pdf:
ggsave("Survival Germlings Initial (PxE).png", p,  width = 8, height = 8, units = "cm", dpi = 600)
ggsave("Survival Germlings Initial (PXE).pdf", p,  width = 8, height = 8, units = "cm", dpi = 600)




