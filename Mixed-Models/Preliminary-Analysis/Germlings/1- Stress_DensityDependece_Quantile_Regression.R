## DENSITY DEPENDENCE ANALYSIS USING QUANTILE REGRESSION
## https://data.library.virginia.edu/getting-started-with-quantile-regression/

# THE OBJETIVE OF THIS CODE IS TO TEST IF THERE ARE ANY DIFFERENCE BETWEEN TREATMENT BEFORE START THE EXPERIMENT TO 
# SO THAT WHEN ANALYSING THE DATA WE CAN TAKE INTO ACCOUNT INITIAL DIFFERENCES TO TAKE CONCLUSIONS


# Load required libraries
library(SparseM)
library(quantreg)
library(rlang)
library(ggplot2)

# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# WWITHOUT CONSIDERING TREATMENTS AND POPULATIONS

# Load the data
qtden <- read.table ("clipboard", header = TRUE, sep = "\t")
attach(qtden)


# Preliminary plot
qplot(Initial, Supervivencia)

ggplot(qtden, aes(x=Initial, y=Supervivencia)) # to draw the plot axes
ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() # plot the data
ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_smooth(method = "lm") # plot the linear regression
ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_smooth(method = "lm") + geom_quantile() # plot the 0.9 quantiles 

#Ahora vamos a hacer lo mismo usando el paquete quantreg para hacer la "quantile regression"
qr1<- rq(Supervivencia ~Initial, data= qtden, tau = 0.9) # this is the same that fit a linear regression
summary(qr1) #from results: upper and lower bd are the confidence intervals 

ggplot(qtden, aes(x=Initial, y =Supervivencia)) + geom_point() + geom_abline(intercept = coef(qr1)[1], slope = coef(qr1)[2])

# The function `rq` allows us create regression with more than one quartile. For do this we need to create a vector with the wanted quuartiles ( qs variable)

# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# CONSIDERING INITIAL NUMBER OF GERMLINGS

qs <- 1:9/10
qs <- c(0.9,0.7)
qr3 <- rq(Supervivencia ~Initial, data = qtden, tau = qs)
coef(qr3)

summary(qr3, se= "ker") # to get the regression p-values


# To calculate the coefficient of determination (R1)

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

# To pretify the plot

plot1<- ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_quantile(quantiles = qs) 
p <-plot1 + xlab("Initial density(Disc)") + ylab ("Survival (%)") + theme( 
  panel.background = element_rect(fill = "white", color="black"),
  panel.grid.major = element_line(colour = "white"),
  panel.grid.minor = element_line(colour = "white")) + 
  xlim(0,900)+ylim(0, 100)

m <- ggplot(qtden, aes(Initial, Supervivencia)) + geom_point( colour = "black", size = 1) # plot the data
m1<- m + geom_quantile() # plot the different quantiles lines

# But... I want thicker lines and change the color
m1<- p + geom_quantile(colour = "blue", size= 0.5, alpha= 0.5) + 
  xlab("Initial density (Disc)") + ylab ("Survival (%)") + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), # to remove the grid inside the plot
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  xlim(0,900)+ylim(0, 100) +
  theme(axis.line = element_line(colour = "black", size = 0.20), # to see the lines along the xed , size= 1, the axis thickness 
        axis.text.x = element_text(face="plain", color = "black", 
                                   size=10, angle = 0), 
        axis.text.y = element_text(face = "plain", color = "black", 
                                   size= 10, angle = 0))

m1

# Save the plot:
ggsave("QuantileRegressionMesocosm.png", m1,  width = 12, height = 8, units = "cm", dpi = 600)
ggsave("QuantileRegressionMesocosm.pdf", m1,  width = 8, height = 8, units = "cm", dpi = 600)


# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #

# CONSIDERING POPULATION X EMERSION

qtden <- read.table ("clipboard", header = TRUE, sep = "\t")
attach(qtden)

qtden$Pop <- as.factor(qtden$Pop)

ggplot(qtden, aes(x=Initial, y=Supervivencia)) # to plot the plot axis
ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() # plot data

ggplot(qtden, aes(x=Initial, y=Supervivencia)) +   geom_point(color='darkblue') # to change the points color
qs <- 1:9/10
qr3 <- rq(Supervivencia ~Initial * Pop *  Emersion, data = qtden, tau = qs)
#qr3.1 <- rq(Supervivencia ~Initial * Pop *  Emersion, data = qtden, tau = taus)
coef(qr3)
ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_quantile(quantiles = qs)
#ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_quantile(quantiles = taus)

summary(qr3, se= "ker") # to get the p-values

# To calculate the determination coefficients (R1)

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


# In case we want to introduce multiple points from different origins with different points, i.e. from different treatments or populations 
sp<- ggplot(qtden2, aes(x=Initial, y=Supervivencia, col=Pop)) + geom_point()
sp
sp + scale_color_manual(breaks = c("Arosa", "Muros", "Peiz�s", "San Pedro"),values=c("red", "blue", "green", "black")) # to change the points colors

sp2<- ggplot(qtden2, aes(x=Initial, y=Supervivencia, col=Pop: Emersion)) + geom_point()
sp2