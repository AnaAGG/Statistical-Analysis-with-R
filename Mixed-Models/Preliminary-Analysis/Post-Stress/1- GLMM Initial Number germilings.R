# CHECK IF THE INITIAL NUMBER OF GERMLINGS DEPENDS ON THE POPULATION IN THE POST-STRESS PHASE
# If yes, we have need to take this into account when interpreting the results

# Load libraries
library(lme4)     ## generalized mixed models
library(lmerTest) ## to MS, df, p ... using type 3/type 1 hypotheses with "Satterthwaite" and "Kenward-Roger"
library(pbkrtest) ## needed to lmerTest
library(car)      ## to fit Anova (model, type = 3) equivalent to Type III sum; or boxCox (model, lambda = seq(-2,2, 2/100))
library(MuMIn)    ## to AICc
library(lmtest)   ## to lrtest
library(psych)    ## to get average table with describe and describeBy
library(arm)      ## to execute models simulations for lm, glm, polr or merMod
library(phia)     ## to plot interactions 
library(lattice)  ## to plot residuals
library(LMERConvenienceFunctions) ## to post-hoc test
library(blmeco)   ## to AICweights
library(MASS)
library(nlme)

# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# TO RIAS POPULATIONS
numrec<- read.table("clipboard", header = TRUE, sep="\t")

str(numrec)

mi.control <- glmerControl(check.conv.grad=.makeCC(action ="ignore", tol=1e-6, relTol=NULL), optimizer="bobyqa", optCtrl=list(maxfun=100000))
options(contrasts=c(factor="contr.sum", ordered="contr.poly"))

## When averaging the counts of the squares, disco cannot be factored in as it only has one measure. 
## it only has one measure.

m1 <- glm(Initial ~ Population  , family=poisson(link="log"), data=numrec)

# Weight of the compared models
Weights(AICc(m1,m2,m3))
## How often is one model better than another?
## the relevant comparisons could be, placing the model with the lowest AICc first:
exp(-0.5*(AICc(m3)-AICc(m2)))

# Check the dispersion
dispersion_glmer(m3)
summary(m3)
Anova(m3, type=3, test="Chisq")


#Reduce the model
options(na.action = "na.fail")
sg<- dredge(m1, rank="AICc") # I assessed all the possible models, with all the possible combinations of fixed factos, fixing th random factor
subset(sg, delta < 2, recalc.weights = TRUE)
importance(subset(model.sel(sg), delta <= 2))
Muminresults<- write.csv(sg, "MuMIn Preliminary results NumberRecruits.csv")

