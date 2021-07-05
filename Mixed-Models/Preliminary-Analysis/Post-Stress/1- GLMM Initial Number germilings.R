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
numrec<- read.table("clipboard", header = TRUE, sep="\t")

str(numrec)

mi.control <- glmerControl(check.conv.grad=.makeCC(action ="ignore", tol=1e-6, relTol=NULL), optimizer="bobyqa", optCtrl=list(maxfun=100000))
options(contrasts=c(factor="contr.sum", ordered="contr.poly"))

##Al hacer promedios de los conteos de los cuadrados, disco no se puede meter como factor ya 
##que solo tiene una medida.


m1 <- glm(Initial ~ Population  , family=poisson(link="log"), data=numrec)



# pesos de los modelos comparados
Weights(AICc(m1,m2,m3))
## ¿cuántas veces es mejor un modelo que otro?
## las comparaciones relevantes podrían ser, colocando primero el modelo con menor AICc:
exp(-0.5*(AICc(m3)-AICc(m2)))

dispersion_glmer(m3)
summary(m3)
Anova(m3, type=3, test="Chisq")


#Reducimos el modelo:
options(na.action = "na.fail")
sg<- dredge(m1, rank="AICc") #Evaluamos todos los posibles modelos, con todas las posibles combinaciones de factores fijos, fijando el factor aleatorio. 
subset(sg, delta < 2, recalc.weights = TRUE)
importance(subset(model.sel(sg), delta <= 2))
Muminresults<- write.csv(sg, "MuMIn Preliminary results NumberRecruits.csv")

