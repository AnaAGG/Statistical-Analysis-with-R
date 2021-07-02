# DENSITY DEPENDENCE ANALYSIS USING QUANTILE REGRESSION
##  https://data.library.virginia.edu/getting-started-with-quantile-regression/

# Load libraries
library(SparseM)
library(quantreg)
library(rlang)
library(ggplot2)

# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# WITHOUT CONSIDERING TREATMENTS AND POPULATIONS 

qtden <- read.table ("clipboard", header = TRUE, sep = "\t")
attach(qtden)

qplot(Initial, Supervivencia)

ggplot(qtden, aes(x=Initial, y=Supervivencia)) #plot the axis
ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() # plot the data
ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_smooth(method = "lm") # plot linear regression
ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_smooth(method = "lm") + geom_quantile() #plot the quantiles, 0.90. 

# Now we are going to do the same using the quantreg package to do the quantile regression.
qr1<- rq(Supervivencia ~Initial, data= qtden, tau = 0.9) # this is the same that a linear model
summary(qr1) #in the results: upper and lower bd are the confidence intervals

ggplot(qtden, aes(x=Initial, y =Supervivencia)) + geom_point() + geom_abline(intercept = coef(qr1)[1], slope = coef(qr1)[2])

#The rq function allows regressions with more than one quantile. For that we first have to make a vestor with the quantiles from 0.10-0.9.


# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# CONSIDERING INITIAL NUMBER OF GERMLINGS

qs <- 1:9/10
qs <- c(0.9,0.7)
qr3 <- rq(Supervivencia ~Initial, data = qtden, tau = qs)
coef(qr3)

plot1<- ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_quantile(quantiles = qs) 
plot1 + xlab("Initial density(Disc)") + ylab ("Survival (%)") + theme( 
  panel.background = element_rect(fill = "white", color="black"),
  panel.grid.major = element_line(colour = "white"),
  panel.grid.minor = element_line(colour = "white")) + 
  xlim(0,300)+ylim(0, 100)


summary(qr3, se= "boot") # to get the p-values 

#To calculate the determination coefficients(R1) 

fit0 <- rq (Supervivencia ~ 1, tau = 0.95, data = qtden)
fit1 <- rq (Supervivencia ~Initial, tau= 0.95, data = qtden)
rho <- function(u,tau=.5)u*(tau - (u < 0))
R0.95 <- 1 - fit1$rho/fit0$rho
R0.95

fit0 <- rq (Supervivencia ~ 1, data = qtden, tau = 0.9)
fit1 <- rq (Supervivencia ~Initial, tau= 0.9, data = qtden)
rho <- function(u,tau=.5)u*(tau - (u < 0))
R0.9 <- 1 - fit1$rho/fit0$rho
R0.9

fit0 <- rq (Supervivencia ~ 1, data = qtden, tau = 0.7)
fit1 <- rq (Supervivencia ~Initial, tau= 0.7, data = qtden)
rho <- function(u,tau=.5)u*(tau - (u < 0))
R0.7 <- 1 - fit1$rho/fit0$rho
R0.7

fit0 <- rq (Supervivencia ~ 1, data = qtden, tau = 0.5)
fit1 <- rq (Supervivencia ~Initial, tau= 0.5, data = qtden)
rho <- function(u,tau=.5)u*(tau - (u < 0))
R0.5 <- 1 - fit1$rho/fit0$rho
R0.5

# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# CONSIDERING POPULATION X EMERSION

qtden <- read.table ("clipboard", header = TRUE, sep = "\t")
attach(qtden2)

qtden$Pop <- as.factor(qtden2$Pop)

ggplot(qtden, aes(x=Initial, y=Supervivencia)) # to plot the axis
ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() # plot the data

ggplot(qtden, aes(x=Initial, y=Supervivencia)) +   geom_point(color='darkblue') # to change the color
qs <- 1:9/10
qr3 <- rq(Supervivencia ~Initial * Pop *  Emersion, data = qtden, tau = qs)
qr3.1 <- rq(Supervivencia ~Initial * Pop *  Emersion, data = qtden, tau = taus)
coef(qr3)
ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_quantile(quantiles = qs)
ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_quantile(quantiles = taus)

summary(qr3, se= "ker") # to get the regression p-values

# To calculate the determination coefficients(R1) 

fit0 <- rq (Supervivencia ~ 1, tau = 0.95, data = qtden)
fit1 <- rq (Supervivencia ~Initial * Pop * Emersion, tau= 0.95, data = qtden)
rho <- function(u,tau=.5)u*(tau - (u < 0))
R0.95 <- 1 - fit1$rho/fit0$rho
R0.95

fit0 <- rq (Supervivencia ~ 1, data = qtden, tau = 0.9)
fit1 <- rq (Supervivencia ~Initial * Pop * Emersion, tau= 0.9, data = qtden)
rho <- function(u,tau=.5)u*(tau - (u < 0))
R0.9 <- 1 - fit1$rho/fit0$rho
R0.9

fit0 <- rq (Supervivencia ~ 1, data = qtden, tau = 0.7)
fit1 <- rq (Supervivencia ~Initial * Pop * Emersion, tau= 0.7, data = qtden)
rho <- function(u,tau=.5)u*(tau - (u < 0))
R0.7 <- 1 - fit1$rho/fit0$rho
R0.7

fit0 <- rq (Supervivencia ~ 1, data = qtden, tau = 0.5)
fit1 <- rq (Supervivencia ~Initial * Pop * Emersion, tau= 0.5, data = qtden)
rho <- function(u,tau=.5)u*(tau - (u < 0))
R0.5 <- 1 - fit1$rho/fit0$rho
R0.5


#In case we want to include dots from different origins, i.e. from different treatments with different colours:
sp<- ggplot(qtden2, aes(x=Initial, y=Supervivencia, col=Pop)) + geom_point()
sp
sp + scale_color_manual(breaks = c("Arosa", "Muros", "Peiz�s", "San Pedro"),values=c("red", "blue", "green", "black")) # si queremos cambiar el color de los puntos

sp2<- ggplot(qtden2, aes(x=Initial, y=Supervivencia, col=Pop: Emersion)) + geom_point()
sp2