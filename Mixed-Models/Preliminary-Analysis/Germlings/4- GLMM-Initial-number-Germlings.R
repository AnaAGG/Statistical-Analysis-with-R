
# CHECK IF THE INITIAL NUMBER OF GERMLINGS DEPENDS ON THE POPULATION. 
# If yes, we have need to take this into account when interpreting the results

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
library(emmeans)  ## to post-hoc analysis

# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# TO RIA POPULATIONS

numger<- read.table("clipboard", header = TRUE, sep="\t")

# to categorise the variables
numger$Tank<- as.factor(numger$Tank)
numger$Salinity<- as.factor(numger$Salinity)
numger$Population<- as.factor(numger$Population)
numger$Temperature<- as.factor(numger$Temperature)
str(numger) # get the type of the objects (as dtypes in python)

mi.control <- glmerControl(check.conv.grad=.makeCC(action ="ignore", tol=1e-6, relTol=NULL), optimizer="bobyqa", optCtrl=list(maxfun=100000))
options(contrasts=c(factor="contr.sum", ordered="contr.poly"))

##When averaging the counts of the squares, disco cannot be factored in as it only has one measure. 
##it only has one measure.


m1 <- glm(Initial ~ Population , family=poisson(link="log"), data=numger)
m2 <- glmer(Initial ~ Population + (1|Tank), family=poisson(link="log"), data=numger, control=mi.control)
m3 <- glmer(Initial ~ Population + (Population|Tank), family=poisson(link="log"), data=numger, control=mi.control) # I dont include this model due to convergence problems
# All three models are fitted with the ML method, thus are comparable with AICc
anova(m2,m1)



# model weights
Weights(AICc(m1,m2))

## how many times is one model better than the other?
## relevant comparisons could be, placing the model with the lowest AICc first:
exp(-0.5*(AICc(m2)-AICc(m1)))

dispersion_glmer(m2)
summary(m2)
Anova(m2, type=3, test="Chisq")

#Another way to know if there is overdispersion, defined by the ratio between the residual deviancy and the residual degrees of freedom, if it is greater than 1 there is overdispersion. 
#residual degrees of freedom, if it is greater than 1 there is overdispersion:
rdev <- sum(residuals(m2)^2)
mdf <- length(fixef(m2))
rdf <- nrow(numger)-mdf
rdev/rdf #if > 1 ==> OVERDISPERSION

#One solution to address the issue of overdispersion is to create a random variable.
#at the individual level:
numger$obs <- 1:nrow(numger) #create individual level random variable
modelBQ<-glmer(Initial ~ Population + (Population|Tank) + (1|obs), data=numger, family=poisson(link="log"), control=mi.control)
#I check the overdispersion again
rdev <- sum(residuals(modelBQ)^2)
mdf <- length(fixef(modelBQ))
rdf <- nrow(numger)-mdf
rdev/rdf 
dispersion_glmer(modelBQ)



#Reduce the model:
#Select the fixed part:

options(na.action = "na.fail")
sg<- dredge(modelBQ, rank="AICc", fixed = ~Population|Tank) # Evaluating all the possible models, with all the possible combination, fixing the random part
subset(sg, delta < 2, recalc.weights = TRUE)
importance(subset(model.sel(sg), delta <= 2))
Muminresults<- write.csv(sg, "MuMIn Preliminary results SurvivalGermlings_(A-M).csv")



# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# ALL POPULATIONS

numger<- read.table("clipboard", header = TRUE, sep="\t")
numger$Tank<- as.factor(numger$Tank)
numger$Salinity<- as.factor(numger$Salinity)
numger$Temperature<- as.factor(numger$Temperature)
str(numger)

mi.control <- glmerControl(check.conv.grad=.makeCC(action ="ignore", tol=1e-6, relTol=NULL), optimizer="bobyqa", optCtrl=list(maxfun=100000))
options(contrasts=c(factor="contr.sum", ordered="contr.poly"))

m1 <- glm(Initial ~ Population * Emersion, family=poisson(link="log"), data=numger)
m2 <- glmer(Initial ~ Population * Emersion + (1|Tank), family=poisson(link="log"), data=numger, control=mi.control)
m3 <- glmer(Initial ~ Population * Emersion + (Population|Tank), family=poisson(link="log"), data=numger, control=mi.control)

anova(m3,m2,m1)
AICc(m1,m2,m3)


# Model weights
Weights(AICc(m1,m2,m3))

## how many times is one model better than the other?
## relevant comparisons could be, placing the model with the lowest AICc first:
exp(-0.5*(AICc(m3)-AICc(m2)))

dispersion_glmer(m2)
summary(m2)
Anova(m2, type=3, test="Chisq")

#Another way to know if there is overdispersion, defined by the ratio between the residual deviancy and the residual degrees of freedom, if it is greater than 1 there is overdispersion. 
#residual degrees of freedom, if it is greater than 1 there is overdispersion:
rdev <- sum(residuals(m2)^2)
mdf <- length(fixef(m2))
rdf <- nrow(numger)-mdf
rdev/rdf #Si es mayor que 1 hay sobredispersi�n

#One solution to address the issue of overdispersion is to create a random variable.
#at the individual level:
numger$obs <- 1:nrow(numger) #create individual level random variable
modelBQ<-glmer(Initial ~ Population * Emersion  + (Population|Tank) + (1|obs), data=numger, family=poisson(link="log"), control=mi.control)

#Check the overdispersion again
rdev <- sum(residuals(modelBQ)^2)
mdf <- length(fixef(modelBQ))
rdf <- nrow(numger)-mdf
rdev/rdf 
dispersion_glmer(modelBQ)

Anova(modelBQ, type=3, test="Chisq")
summary(modelBQ)
r.squaredGLMM(modelBQ)

#Reduce the model:
#Select the fixed part:

m6<- update(m4, REML =FALSE) #The first thing we have to do is to fit the model with the optimal random part to ML. 
options(na.action = "na.fail")
sg<- dredge(modelBQ, rank="AICc", fixed = ~Population|Tank) #We evaluate all possible models, with all possible combinations of fixed factors, fixing the random factor. 
subset(sg, delta < 2, recalc.weights = TRUE)
importance(subset(model.sel(sg), delta <= 2))

Muminresults<- write.csv(sg, "MuMIn Preliminary results SurvivalGermlings_(A-M).csv")

# Now we do the POST-HOC analysis for the Population x Emersion interaction.

lsm1 <- emmeans(modelBQ, pairwise ~ Population:Emersion, adjust = "Tukey")
summary(lsm1, type = "response")

CLD(lsm1, Letters=letters, alpha = 0.95, which = 1)

plot(lsm1)

# To the plot
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
  theme_bw() + # to remove background
  theme(panel.border= element_blank(),
        panel.grid.major = element_blank(), # to remove plot grid
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black", size = 0.20), # to see the lines along the axes, size= 1, the thickness of the axes
        axis.text.x = element_text(face="plain", color = "black", 
                                   size=10, angle = 0), 
        axis.text.y = element_text(face = "plain", color = "black", 
                                   size= 10, angle = 0))

p

# Save figures as pdf
pdf("myplotArosa.pdf",p, width = 4, height = 4) # las medidas vienen por defecto en inches, para converstirlo a cm habr� que hacerlo manualmente

# To get figure 8x8 cm 
pdf("myplotArosa.pdf", p,  width = 8/2.54, height = 8/2.54)
dev.off()

# Another to save the figure
ggsave("InitialGermlingM.png", p,  width = 8, height = 8, units = "cm", dpi = 600)
ggsave("InitialGermlingM.pdf", p,  width = 8, height = 8, units = "cm", dpi = 600)

