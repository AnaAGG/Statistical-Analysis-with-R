##MODELO DE FUCUS SERRATUS##

library(sp)
library(raster)
library(rgdal)
library(dismo)
library(rJava)

#PASO 1## Preparacion de los datos

#Lo primero que hacemos es cargar los datos de presencia

presences<- read.table("clipboard", header=TRUE, sep="\t")
head(presences)

#Como solo nos hace falta la información de lon y lat, lo que hacemos ahora es crear un nuevo objeto que solo tenga esas dos columnas, mantenemos el mismo nombre

presences <- presences[,2:3]
head(presences)

#Para darle a las coordenas un sistema de referencia
coordinates(presences) <- ~Longitude+Latitude
projection(presences) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')


#Para meter los raster en R
admax <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/ad_max.asc")
admin <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/ad_min.asc")
calcita <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/calcita_mean.asc")
clormax <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/clor_max.asc")
clormin <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/clor_min.asc")
cobmax <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/cob_max.asc")
cobmin <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/cob_min.asc")
espmax <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/esp_max.asc")
espmin <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/esp_min.asc")
fitomax <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/fito_max.asc")
fitomin <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/fito_min.asc")
fosfmax <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/fosf_max.asc")
fosfmin <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/fosf_min.asc")
hierromax <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/hierro_max.asc")
hierromin <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/hierro_min.asc")
nitrmax <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/nitr_max.asc")
nitrmin <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/nitr_min.asc")
cloudmax <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/cloud_max.asc")
cloudmin <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/cloud_min.asc")
oxigmax <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/oxig_max.asc")
oxigmin <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/oxig_min.asc")
parmax <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/par_max.asc")
ph <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/ph.asc")
ppmax <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/pp_max.asc")
ppmin <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/pp_min.asc")
salmax <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/salin_max.asc")
salmin <- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/salin_min.asc")
silimax<- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/silic_max.asc")
silimin<- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/silic_min.asc")
tempmax<- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/temp_max.asc")
tempmin<- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/temp_min.asc")
velmax<- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/veloc_max.asc")
velmin<- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/veloc_min.asc")
airmax<- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/airtemp_max.asc")
airmin<- raster ("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/airtemp_min.asc")


varamb <- c(admax, admin, calcita, clormax,clormin, cobmax, espmax, fitomax, fitomin, fosfmax, fosfmin, hierromax, hierromin, nitrmax, nitrmin, cloudmax, cloudmin, oxigmax, oxigmin, parmax, ph, ppmax, ppmin, salmax, salmin, silimax, silimin, tempmax, tempmin, velmax, velmin, airmax, airmin)
predictors <- stack(varamb)

crs(predictors) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #Para darle una extension geografica a mi RasterStack
predictors
plot(predictors)# Para plotear todas las variables predictoras

#Ahora podemos plotear las presecias con las variables. Si lo hacemos de uno en una: 
plot(predictors, 1) #Al poner el 1 estoy ploteando ad_max (es la primera en nuestra lista)
points(presences, col='blue') # para añadir los datos de presencia

#Para extraer la informacion de los raster de cada una de nuestras presencias: 
presvals <- extract(predictors, presences)
presvals

#AQUI HABRA QUE HACER UNA CORRELACION DE SPEARMAN
install_github("danlwarren/ENMTools")
library(devtools)
library(backports)
library(ecospat)
library(dismo)
library(ENMTools)

#Correlacion de Spearman
corspearman <- cor(presvals, method = "spearman")
write.csv(corspearman, "Spearman Correlation Fucus")

#Correlacion de Pearson
corpearson <- cor(presvals, method = "pearson")
write.csv(corpearson, "Pearson Correlation Fucus")

#Para ver la relacion que existe entre dos variables en una gráfica
plot(cloudmin, airmax, maxpixels = 10000000) #maxpixels es para que nos plotee todos los puntos que tenemos en cada variable. 
plot(cloudmin, tempmax, maxpixels = 10000000)
plot(airmax, parmax, maxpixels = 10000000)
plot(airmax, tempmax, maxpixels = 10000000)
plot(airmin, tempmin, maxpixels = 10000000)

#Ahora vamos a generar un nuevo fichero con un background de 250
set.seed(0)
backgr <- randomPoints(predictors, 250)
absvals <- extract(predictors, backgr) # nos da los valores de los predictores para los 500 puntos de background que hemos generado

pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))  # creamos un nuevo objeto donde vamos a juntar nuestros datos de presencia con los del background
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
head(sdmdata)

###MAXENT Y BIOMOD### https://gist.github.com/hannahlowens/974066848f8f85554ff7

library(rpart)
library(MASS)
library(gbm)
library(splines2)
library(foreach)
library(gam)
library(nnet)
library(class)
library(mda)
library(randomForest)
#library(Design)
library(lattice)
library(survival)
library(Formula)
library(Rcpp)
library(rlang)
library(tibble)
library(ggplot2)
library(Hmisc)
library(reshape)
library(plyr)
library(parallel)
library(raster)
library(sp)
library(dismo)
library(rJava) #Si sale el siguiente error "Error: package or namespace load failed for 'rJava':.onLoad failed in loadNamespace() for 'rJava', details:call: fun(libname, pkgname)", el problema es que hay que instalar la version de 64bits de java, por defencto se instala la de 32. La pagina de donde podemos descargar la version correcta es: https://www.java.com/en/download/manual.jsp  
library(biomod2)

#Lo primero que hacemos es cargar los datos de presencia.

presences<- read.table("clipboard", header=TRUE, sep="\t")
head(presences)

presences <- cbind(presences, rep.int(1, length(nrow(presences)))) # lo que hacemos con esto es añadir una columna indicando que son datos de presencia
colnames(presences) <- c("Name", "Longitude", "Latitude", "Response")
head(presences)

#Siguiente paso cargar las variables ambientales
#En vez de meter las variables de una en una se puede hacer de una unica vez de la siguiente forma: 

setwd("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/ExplicativeVariables")
envtList <- list.files(pattern = ".asc")
envt.st <- stack(envtList)

#Siguiente cargar las proyecciones
setwd("")
projectionList <- list.files(pattern = ".asc")
proj.st <- stack(projectionList)

#Configuración de archivo de datos para Biomod2
bmData<- BIOMOD_FormatingData(resp.var = presences[,4],
                              resp.xy = presences[,2:3],
                              resp.name = as.character(presences[1,1]),
                              expl.var = envt.st,
                              PA.nb.rep = 1) #Mirar que significaba esto

#Configuracion de archivo de datos para MaxEnt
#Aqui tambien habra que poner el background
myBiomodOption <- Print_Default_ModelingOptions()
myBiomodOption@MAXENT.Phillips$path_to_maxent.jar = paste(system.file(package="dismo"), "/java/maxent.jar", sep='') #importante! meter maxent.java en la carpeta del paquete dismo de R
myBiomodOption@MAXENT.Phillips$memory_allocated = 10048  #Asigna 2 gigas en la memoria para modelar
myBiomodOption@MAXENT.Phillips$maximumiterations = 5000
myBiomodOption@MAXENT.Phillips$threshold = F
myBiomodOption@MAXENT.Phillips$visible = F
myBiomodOption@MAXENT.Phillips$beta_lqp = 0.95

#Corremos MaxEnt
myMaxentModel <- BIOMOD_Modeling(data = bmData,
                                 models = c('MAXENT.Phillips'),
                                 models.options = myBiomodOption,
                                 NbRunEval = 10, #numero de veces que queremos correr los modelos. 
                                 do.full.models = F, # Si es T, el calibrado y la evaluacion del modelo se hacer con todo el set de datos. 
                                 DataSplit = 70, # % de datos que se usan para calibrar el modelo, el resto de los datos se usan para testar
                                 models.eval.meth = c('KAPPA', 'TSS', 'ROC'),
                                 SaveObj = T)

#Corremos BIOMOD
presences <- read.table("clipboard", header=TRUE, sep="\t") #DAtos de presencia
presences <- presences[,2:3]

setwd("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/ExplicativeVariables")
envtList <- list.files(pattern = ".asc")
envt.st <- stack(envtList)

presvals <- extract(envt.st, presences)
write.csv(presvals, "DataSpecies")

DataSpecies<- read.table("clipboard", header=TRUE, sep="\t")
head(DataSpecies)

myRespName <- 'F.serratus'

#Las coordenas x e y
myRespXY <- DataSpecies[,2:3]

#os datos de presencia/ausencia
myResp <- as.numeric(DataSpecies[,1])

myBiomodData<- BIOMOD_FormatingData(myResp, envt.st, myRespXY, myRespName,PA.nb.rep = 1, 
                    PA.nb.absences = 600,
                    PA.strategy = 'random', 
                    PA.dist.min = 0.2, 
                    PA.dist.max = NULL, 
                    PA.sre.quant = 0.025,
                    PA.table = NULL,
                    na.rm = TRUE)

myBiomodOptions <- BIOMOD_ModelingOptions(GLM =list( type = 'simple',interaction.level = 0,
                                                     myFormula = NULL, test = 'BIC', family = 'binomial', 
                                                     control = glm.control(epsilon = 1e-08, maxit = 1000,
                                                                           trace = FALSE)))
                                          
myBiomodOptions  

# 3. Computing the models

#help(BIOMOD_ModelingOptions)

myBiomodModelOut <- BIOMOD_Modeling(
                                myBiomodData, 	
                                models = c('GLM'),  #########, 'GAM' ######
                                models.options = myBiomodOptions, 
                                NbRunEval=10, 
                                DataSplit=70, 
                                VarImport=3, 
                                models.eval.meth = c('KAPPA','TSS','ROC', 'ACCURACY'),
                                SaveObj = TRUE, 
                                rescal.all.models = FALSE, 
                                do.full.models = TRUE, 
                                modeling.id = paste(myRespName,"FirstModeling",sep=""))

myBiomodModelOut

myGLMs<-BIOMOD_LoadModels(myBiomodModelOut) 
ls()  
get_formal_model(F.serratus_PA1_Full_GLM)
summary(F.serratus_PA1_Full_GLM@model)
myRespPlot2D <- response.plot2(models = myGLMs, Data = get_formal_data(myBiomodModelOut,'expl.var'),show.variables= get_formal_data(myBiomodModelOut,'expl.var.names'),do.bivariate = FALSE,fixed.var.metric = 'median',col = c("blue", "red"),legend = TRUE,data_species = get_formal_data(myBiomodModelOut,'resp.var'))


# get all models evaluation
myBiomodModelEval <- get_evaluations(myBiomodModelOut)
myBiomodModelEval

# print the dimnames of this object
dimnames(myBiomodModelEval)

# let's print the TSS scores of all selected models   #--TSS y ROC evaluan poder predictivo del modelo
myBiomodModelEval["TSS","Testing.data",,,]

# let's print the ROC scores of all selected models
myBiomodModelEval["ROC","Testing.data",,,]

# print variable importances
get_variables_importance(myBiomodModelOut)

## outputformat- cambio '.grd' por '.img'
myBiomodProj <- BIOMOD_Projection(
                        modeling.output = myBiomodModelOut,
                        new.env = envt.st,
                        xy.new.env = myRespXY,
                        proj.name = 'current', 
                        selected.models = 'all',
                        binary.meth = 'TSS',
                        compress = 'gzip',
                        clamping.mask = F,
                        output.format = '.grd')

