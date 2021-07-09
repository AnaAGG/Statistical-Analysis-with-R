## DENSITY DEPENDENCE ANALYSIS USING QUANTILE REGRESSION
## https://data.library.virginia.edu/getting-started-with-quantile-regression/

# THE OBJETIVE OF THIS CODE IS TO TEST IF THERE ARE ANY DIFFERENCE BETWEEN TREATMENT BEFORE START THE RECOVERY EXPERIMENT TO 
# SO THAT WHEN ANALYSING THE DATA WE CAN TAKE INTO ACCOUNT INITIAL DIFFERENCES TO TAKE CONCLUSIONS

# load libraries
library(SparseM)
library(quantreg)
library(rlang)
library(ggplot2)

# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# --------------------- # --------------------- # --------------------- # --------------------- # --------------------- # --------------------- #
# DENSITY DEPENDENDE ANALYSIS NO CONSIDERING POPULATIONS AND TREATMENTS

qs <- 1:9/10
qs <- c(0.9,0.7)
qr3 <- rq(Supervivencia ~Initial, data = qtden, tau = qs)
coef(qr3)

plot1<- ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_quantile(quantiles = qs) 
plot1 + xlab("Initial density(Disc)") + ylab ("Survival (%)") + theme( 
  panel.background = element_rect(fill = "white", color="black"),
  panel.grid.major = element_line(colour = "white"),
  panel.grid.minor = element_line(colour = "white")) + 
  xlim(0,250)+ylim(0, 100)

# To get pretty plot

plot1<- ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_quantile(quantiles = qs) 
plot1 + xlab("Initial density(Disc)") + ylab ("Survival (%)") + theme( 
  panel.background = element_rect(fill = "white", color="black"),
  panel.grid.major = element_line(colour = "white"),
  panel.grid.minor = element_line(colour = "white")) + 
  xlim(0,900)+ylim(0, 100)

m <- ggplot(qtden, aes(Initial, Supervivencia)) + geom_point( colour = "black", size = 1) # plot the data
m1<- m + geom_quantile() # plot the quartiles

# But we want the lines thicker and in a different colour:
m1<- m + geom_quantile(colour = "blue", size= 0.5, alpha= 0.5) + 
  xlab("Initial density (Disc)") + ylab ("Survival (%)") + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), # to remove background lines
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  xlim(0,250)+ylim(0, 100) +
  theme(axis.line = element_line(colour = "black", size = 0.20), 
        axis.text.x = element_text(face="plain", color = "black", 
                                   size=10, angle = 0), 
        axis.text.y = element_text(face = "plain", color = "black", 
                                   size= 10, angle = 0))

m1

ggsave("QuantileRegressionPost-estres.png", m1,  width = 12, height = 8, units = "cm", dpi = 600)
ggsave("QuantileRegressionPost-estres.pdf", m1,  width = 12, height = 8, units = "cm", dpi = 600)

summary(qr3, se= "boot") # to get quantile regression p-values 

# To calculate the determination coeficients (R1)

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
# DENSITY DEPENDENDE ANALYSIS CONSIDERING POPULATION X EMERSION


qtden <- read.table ("clipboard", header = TRUE, sep = "\t")
attach(qtden2)

qtden$Pop <- as.factor(qtden2$Pop)

ggplot(qtden, aes(x=Initial, y=Supervivencia)) # to plot the axis
ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() # plot the data

ggplot(qtden, aes(x=Initial, y=Supervivencia)) +   geom_point(color='darkblue') #to change the colors
qs <- 1:9/10
qr3 <- rq(Supervivencia ~Initial * Pop *  Emersion, data = qtden, tau = qs)
qr3.1 <- rq(Supervivencia ~Initial * Pop *  Emersion, data = qtden, tau = taus)
coef(qr3)
ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_quantile(quantiles = qs)
ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_quantile(quantiles = taus)

summary(qr3, se= "ker") # to get the regression p-values

# To calculate the determation coeficients (R1)

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
sp + scale_color_manual(breaks = c("Arosa", "Muros", "Peiz�s", "San Pedro"),values=c("red", "blue", "green", "black")) # change the color

sp2<- ggplot(qtden2, aes(x=Initial, y=Supervivencia, col=Pop: Emersion)) + geom_point()
sp2


# To plot the figure of the initial number do recruits per population

recps <- read.table("clipboard", header = TRUE, sep="\t")

ggplot(recps, aes(x= Population, y= Survival)) + geom_bar(position = "dodge", stat = "identity", width = 0.5, colour ="Black") + 
  scale_fill_grey(start = .5, end= .9)

p <- ggplot(recps, aes(x= Population, y= Survival)) + 
  geom_bar(position = "dodge", stat = "identity", width = 0.5, colour="black", size =0.2) + #???size = X to determine the bars weight
  
  scale_fill_grey(start = .9, end= 1.0) + # to specify the color of the bars (balck and white)
  geom_errorbar(aes(ymin=Survival -SE, ymax = Survival + SE),
                position = position_dodge(0.5), width = .0, size = 0.2) +
  scale_y_continuous(expand = c(0.0,0.0),limits = c(0.0, 100), breaks = seq(0.0,100, by = 20)) + 
  xlab("") + ylab("Initial number of recruits") +
  theme_bw() + # removing the background 
  theme(panel.border= element_blank(),
        panel.grid.major = element_blank(), # to remove the background lines
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black", size = 0.20), 
        axis.text.x = element_text(face="plain", color = "black", 
                                   size=10, angle = 0), 
        axis.text.y = element_text(face = "plain", color = "black", 
                                   size= 10, angle = 0))

p

ggsave("InitialRecruitsPost-estres.png", p,  width = 12, height = 8, units = "cm", dpi = 600)
ggsave("InitialRecruitsPost-estres.pdf", p,  width = 12, height = 8, units = "cm", dpi = 600)
