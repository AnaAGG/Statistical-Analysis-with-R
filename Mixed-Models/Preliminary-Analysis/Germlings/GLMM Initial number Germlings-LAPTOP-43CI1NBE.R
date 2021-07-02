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
library(blmeco)   ## para AICweights
library(MASS)
library(nlme)

##Para las poblaciones de rias##
numger<- read.table("clipboard", header = TRUE, sep="\t")

numger$Tank<- as.factor(numger$Tank)
numger$Salinity<- as.factor(numger$Salinity)
numger$Population<- as.factor(numger$Population)
numger$Temperature<- as.factor(numger$Temperature)
str(numger)

mi.control <- glmerControl(check.conv.grad=.makeCC(action ="ignore", tol=1e-6, relTol=NULL), optimizer="bobyqa", optCtrl=list(maxfun=100000))
options(contrasts=c(factor="contr.sum", ordered="contr.poly"))

##Al hacer promedios de los conteos de los cuadrados, disco no se puede meter como factor ya 
##que solo tiene una medida.


m1 <- glm(Initial ~ Population , family=poisson(link="log"), data=numger)
m2 <- glmer(Initial ~ Population + (1|Tank), family=poisson(link="log"), data=numger, control=mi.control)
m3 <- glmer(Initial ~ Population + (Population|Tank), family=poisson(link="log"), data=numger, control=mi.control) # No se incluye este modelo por problemas de convergencia
#Los tres modelos están ajustados con el método de ML por lo que son comparables con AICc.

anova(m2,m1)



# pesos de los modelos comparados
Weights(AICc(m1,m2))
## ¿cuántas veces es mejor un modelo que otro?
## las comparaciones relevantes podrían ser, colocando primero el modelo con menor AICc:
exp(-0.5*(AICc(m2)-AICc(m1)))

dispersion_glmer(m2)
summary(m2)
Anova(m2, type=3, test="Chisq")

#Otra forma de saber si existe sobredispersión, definida por el ratio entre la devianza 
#residual y los grados de libertad residuales, si es mayor que 1 hay sobredispersión:
rdev <- sum(residuals(m2)^2)
mdf <- length(fixef(m2))
rdf <- nrow(numger)-mdf
rdev/rdf #Si es mayor que 1 hay sobredispersión

#Una solución para abordar el tema de la sobredispersión es crear una variable aleatoria
#a nivel individual:
numger$obs <- 1:nrow(numger) #create individual level random variable
modelBQ<-glmer(Initial ~ Population + (Population|Tank) + (1|obs), data=numger, family=poisson(link="log"), control=mi.control)
#Se analiza otra vez la dispersión:
rdev <- sum(residuals(modelBQ)^2)
mdf <- length(fixef(modelBQ))
rdf <- nrow(numger)-mdf
rdev/rdf 
dispersion_glmer(modelBQ)



#Reducimos el modelo:
#Selección de la parte fija:

options(na.action = "na.fail")
sg<- dredge(modelBQ, rank="AICc", fixed = ~Population|Tank) #Evaluamos todos los posibles modelos, con todas las posibles combinaciones de factores fijos, fijando el factor aleatorio. 
subset(sg, delta < 2, recalc.weights = TRUE)
importance(subset(model.sel(sg), delta <= 2))

Muminresults<- write.csv(sg, "MuMIn Preliminary results SurvivalGermlings_(A-M).csv")



##Para todas las poblaciones##
numger<- read.table("clipboard", header = TRUE, sep="\t")

numger$Tank<- as.factor(numger$Tank)
numger$Salinity<- as.factor(numger$Salinity)
numger$Temperature<- as.factor(numger$Temperature)
str(numger)

mi.control <- glmerControl(check.conv.grad=.makeCC(action ="ignore", tol=1e-6, relTol=NULL), optimizer="bobyqa", optCtrl=list(maxfun=100000))
options(contrasts=c(factor="contr.sum", ordered="contr.poly"))

##Al hacer promedios de los conteos de los cuadrados, disco no se puede meter como factor ya 
##que solo tiene una medida.


m1 <- glm(Initial ~ Population * Emersion, family=poisson(link="log"), data=numger)
m2 <- glmer(Initial ~ Population * Emersion + (1|Tank), family=poisson(link="log"), data=numger, control=mi.control)
m3 <- glmer(Initial ~ Population * Emersion + (Population|Tank), family=poisson(link="log"), data=numger, control=mi.control)
#Los tres modelos están ajustados con el método de ML por lo que son comparables con AICc.

anova(m3,m2,m1)
AICc(m1,m2,m3)


# pesos de los modelos comparados
Weights(AICc(m1,m2,m3))
## ¿cuántas veces es mejor un modelo que otro?
## las comparaciones relevantes podrían ser, colocando primero el modelo con menor AICc:
exp(-0.5*(AICc(m3)-AICc(m2)))

dispersion_glmer(m2)
summary(m2)
Anova(m2, type=3, test="Chisq")

#Otra forma de saber si existe sobredispersión, definida por el ratio entre la devianza 
#residual y los grados de libertad residuales, si es mayor que 1 hay sobredispersión:
rdev <- sum(residuals(m2)^2)
mdf <- length(fixef(m2))
rdf <- nrow(numger)-mdf
rdev/rdf #Si es mayor que 1 hay sobredispersión

#Una solución para abordar el tema de la sobredispersión es crear una variable aleatoria
#a nivel individual:
numger$obs <- 1:nrow(numger) #create individual level random variable
modelBQ<-glmer(Initial ~ Population * Emersion  + (Population|Tank) + (1|obs), data=numger, family=poisson(link="log"), control=mi.control)
#Se analiza otra vez la dispersión:
rdev <- sum(residuals(modelBQ)^2)
mdf <- length(fixef(modelBQ))
rdf <- nrow(numger)-mdf
rdev/rdf 
dispersion_glmer(modelBQ)

Anova(modelBQ, type=3, test="Chisq")
summary(modelBQ)
r.squaredGLMM(modelBQ)

#Reducimos el modelo:
#Selección de la parte fija:

m6<- update(m4, REML =FALSE)#lo primero que tenemos que hacer es ajustar el modelo con la parte aleatorio óptima a ML. 
options(na.action = "na.fail")
sg<- dredge(modelBQ, rank="AICc", fixed = ~Population|Tank) #Evaluamos todos los posibles modelos, con todas las posibles combinaciones de factores fijos, fijando el factor aleatorio. 
subset(sg, delta < 2, recalc.weights = TRUE)
importance(subset(model.sel(sg), delta <= 2))

Muminresults<- write.csv(sg, "MuMIn Preliminary results SurvivalGermlings_(A-M).csv")

#ahora hacemos el analisis post-hoc para la interaccion Population x Emersion
library(emmeans)

lsm1 <- emmeans(modelBQ, pairwise ~ Population:Emersion, adjust = "Tukey")
summary(lsm1, type = "response")

CLD(lsm1, Letters=letters, alpha = 0.95, which = 1)

plot(lsm1)

# Para la grafica
numger <- read.table("clipboard", header = TRUE, sep="\t")

ggplot(initialger, aes(x= Population, y= Mean)) + geom_bar(position = "dodge", stat = "identity", width = 0.5, colour ="Black") + 
  scale_fill_grey(start = .5, end= .9)

p <- ggplot(initialger, aes(x= Population, y= Mean)) + 
  geom_bar(position = "dodge", stat = "identity", width = 0.3, colour="black", fill= "grey", size =0.2) + #???size = X para determinar el grosor de las lineas de las barras
  geom_errorbar(aes(ymin= Mean -SE, ymax = Mean + SE),
                position = position_dodge(0.5), width = .0, size = 0.2) + 
  geom_text(aes(label = Letters, y = Mean),hjust = "center", vjust = -2, fontface = "bold") +
  scale_y_continuous(expand = c(0.0,0.0),limits = c(0.0, 100), breaks = seq(0.0,100, by = 20)) + 
  xlab("") + ylab("Initial number of germlings/disc") +
  theme_bw() + #quitamos el fondo gris 
  theme(panel.border= element_blank(),
        panel.grid.major = element_blank(), # para quitar las lineas internas del plot
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black", size = 0.20), #para ver las lineas a lo largo de los ejes, size= 1, el grosor de los ejes
        axis.text.x = element_text(face="plain", color = "black", 
                                   size=10, angle = 0), 
        axis.text.y = element_text(face = "plain", color = "black", 
                                   size= 10, angle = 0))

p

#para guardar las figuras en pdf:
pdf("myplotArosa.pdf",p, width = 4, height = 4) # las medidas vienen por defecto en inches, para converstirlo a cm habrá que hacerlo manualmente

#Para una grafica de 8x8 cm 
pdf("myplotArosa.pdf", p,  width = 8/2.54, height = 8/2.54)
dev.off()

#???otra forma de hacerlo:
ggsave("InitialGermlingM.png", p,  width = 8, height = 8, units = "cm", dpi = 600)
ggsave("InitialGermlingM.pdf", p,  width = 8, height = 8, units = "cm", dpi = 600)

