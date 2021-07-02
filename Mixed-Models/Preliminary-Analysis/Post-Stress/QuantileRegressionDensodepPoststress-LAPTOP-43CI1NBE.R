# Análisis de densodependencia en reclutas, en post-estres. 
# Quantile regression ##  https://data.library.virginia.edu/getting-started-with-quantile-regression/


library(SparseM)
library(quantreg)
library(rlang)
library(ggplot2)

#Sin considerar poblaciones ni trtamientos. 

qtden <- read.table ("clipboard", header = TRUE, sep = "\t")
attach(qtden)

qplot(Initial, Supervivencia)

ggplot(qtden, aes(x=Initial, y=Supervivencia)) #para dibujar los ejes de la gráfica
ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() # dibujamos los puntos de los datos



ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_smooth(method = "lm") # dibuja la regresión linear

ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_smooth(method = "lm") + geom_quantile() #Dibuja los los cuartiles, 0.90. 

#Ahora vamos a hacer lo mismo usando el paquete quantreg para hacer la "quantile regression"
qr1<- rq(Supervivencia ~Initial, data= qtden, tau = 0.9) # esto es lo mismo que hacer un lm normal. 
summary(qr1)
#upper and lower bd son los intervalos de confianza 

ggplot(qtden, aes(x=Initial, y =Supervivencia)) + geom_point() + geom_abline(intercept = coef(qr1)[1], slope = coef(qr1)[2])

#La funcion rq permite realizar regresiones con mas de un cuantil. Para eso tenemos que hacer primero un vestor con los cuantiles desde 0.10-0.9
#Considerando Poblacion x Emersión

qs <- 1:9/10
qs <- c(0.9,0.7, 0.5)
qr3 <- rq(Supervivencia ~Initial, data = qtden, tau = qs)
coef(qr3)

summary(qr3)

plot1<- ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_quantile(quantiles = qs) 
plot1 + xlab("Initial density(Disc)") + ylab ("Survival (%)") + theme( 
  panel.background = element_rect(fill = "white", color="black"),
  panel.grid.major = element_line(colour = "white"),
  panel.grid.minor = element_line(colour = "white")) + 
  xlim(0,250)+ylim(0, 100)

#Si queremos dejar la grafica bonita

plot1<- ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_quantile(quantiles = qs) 
plot1 + xlab("Initial density(Disc)") + ylab ("Survival (%)") + theme( 
  panel.background = element_rect(fill = "white", color="black"),
  panel.grid.major = element_line(colour = "white"),
  panel.grid.minor = element_line(colour = "white")) + 
  xlim(0,900)+ylim(0, 100)

m <- ggplot(qtden, aes(Initial, Supervivencia)) + geom_point( colour = "black", size = 1) #con esto ploteo los puntos
m1<- m + geom_quantile() # con esto ploteo las rectas de los quantiles

#pero nosostras queremos las lineas mas gruesas y de otro color:
m1<- m + geom_quantile(colour = "blue", size= 0.5, alpha= 0.5) + 
  xlab("Initial density (Disc)") + ylab ("Survival (%)") + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), # para quitar las lineas internas del plot
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  xlim(0,250)+ylim(0, 100) +
  theme(axis.line = element_line(colour = "black", size = 0.20), #para ver las lineas a lo largo de los ejes, size= 1, el grosor de los ejes
        axis.text.x = element_text(face="plain", color = "black", 
                                   size=10, angle = 0), 
        axis.text.y = element_text(face = "plain", color = "black", 
                                   size= 10, angle = 0))

m1

ggsave("QuantileRegressionPost-estres.png", m1,  width = 12, height = 8, units = "cm", dpi = 600)
ggsave("QuantileRegressionPost-estres.pdf", m1,  width = 12, height = 8, units = "cm", dpi = 600)

summary(qr3, se= "boot") # para sacar los p valores de la regresion.

#Para calcular los coeficientes de determinacion (R1) 

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

#Considerando Poblacion x Emersión

qtden <- read.table ("clipboard", header = TRUE, sep = "\t")
attach(qtden2)

qtden$Pop <- as.factor(qtden2$Pop)

ggplot(qtden, aes(x=Initial, y=Supervivencia)) #para dibujar los ejes de la gráfica
ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() # dibujamos los puntos de los datos

ggplot(qtden, aes(x=Initial, y=Supervivencia)) +   geom_point(color='darkblue') #si quiero cambiarle el color a los puntos
qs <- 1:9/10
qr3 <- rq(Supervivencia ~Initial * Pop *  Emersion, data = qtden, tau = qs)
qr3.1 <- rq(Supervivencia ~Initial * Pop *  Emersion, data = qtden, tau = taus)
coef(qr3)
ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_quantile(quantiles = qs)
ggplot(qtden, aes(x=Initial, y=Supervivencia)) + geom_point() + geom_quantile(quantiles = taus)

summary(qr3, se= "ker") # para sacar los p valores de la regresion.

#Para calcular los coeficientes de determinacion (R1) 

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


#En caso de que queramos meter puntos de distintos origenes, es decir, de distintos tratamientos con distintos colores:
sp<- ggplot(qtden2, aes(x=Initial, y=Supervivencia, col=Pop)) + geom_point()
sp
sp + scale_color_manual(breaks = c("Arosa", "Muros", "Peizás", "San Pedro"),values=c("red", "blue", "green", "black")) # si queremos cambiar el color de los puntos

sp2<- ggplot(qtden2, aes(x=Initial, y=Supervivencia, col=Pop: Emersion)) + geom_point()
sp2


#???Para plotear la figura de numero inicial de reclutas por poblacion

recps <- read.table("clipboard", header = TRUE, sep="\t")

ggplot(recps, aes(x= Population, y= Survival)) + geom_bar(position = "dodge", stat = "identity", width = 0.5, colour ="Black") + 
  scale_fill_grey(start = .5, end= .9)

p <- ggplot(recps, aes(x= Population, y= Survival)) + 
  geom_bar(position = "dodge", stat = "identity", width = 0.5, colour="black", size =0.2) + #???size = X para determinar el grosor de las lineas de las barras
  scale_fill_grey(start = .9, end= 1.0) + #para determinar los colores de las barras(Blanco y gris)
  geom_errorbar(aes(ymin=Survival -SE, ymax = Survival + SE),
                position = position_dodge(0.5), width = .0, size = 0.2) +
  scale_y_continuous(expand = c(0.0,0.0),limits = c(0.0, 100), breaks = seq(0.0,100, by = 20)) + 
  xlab("") + ylab("Initial number of recruits") +
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

ggsave("InitialRecruitsPost-estres.png", p,  width = 12, height = 8, units = "cm", dpi = 600)
ggsave("InitialRecruitsPost-estres.pdf", p,  width = 12, height = 8, units = "cm", dpi = 600)
