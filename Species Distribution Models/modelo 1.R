
library(foreach)
library(iterators)
library(parallel)
library(doParallel)
library(utils)
library(graphics)
library(stats)
library(grDevices)
library(sp)
library(raster)
library(maxnet)
library(rJava)
library(knitr)
library(rmarkdown)
library(spocc)
library(maptools)
library(rgeos)
library(dismo)
library(ENMeval)

maxentJARversion ()

presences <- read.table("clipboard", header=TRUE, sep="\t") #DAtos de presencia
presences <- presences[,2:3]
coordinates(presences) <- ~Longitude+Latitude
projection(presences) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')



# Put the rasters into a RasterStack:
setwd("C:/Users/Ana G/OneDrive - Universidad Rey Juan Carlos/Predoctoral/DatosFucus/Modelos/ModeloFucus/R/RastersPresente/Presente/ParaR/ExplicativeVariables")
envtList <- list.files(pattern = ".asc")
envs <- stack(envtList)

# Plot first raster in the stack, bio1
plot(envs[[1]], main=names(envs)[1])
plot(envs[[2]], main=names(envs)[2])
plot(envs[[3]], main=names(envs)[3])

# Add points for all the occurrence points onto the raster
points(presences)

# There are some points all the way to the south-east, far from all others. Let's say we know that this represents a subpopulation that we don't want to include, and want to remove these points from the analysis. We can find them by first sorting the occs table by latitude.
head(presences[order(presences$latitude),])

# Make a SpatialPoints object
occs.sp <- SpatialPoints(presences)

# Get the bounding box of the points
bb <- bbox(occs.sp)

# Add 5 degrees to each bound by stretching each bound by 10, as the resolution is 0.5 degree.
bb.buf <- extent(bb[1]-10, bb[3]+10, bb[2]-10, bb[4]+10)

# Crop environmental layers to match the study extent
envs.backg <- crop(envs, bb.buf)

# Randomly sample 600 background points from one background extent raster (only one per cell without replacement). Note: Since the raster has <10,000 pixels, you'll get a warning and all pixels will be used for background. We will be sampling from the biome variable because it is missing some grid cells, and we are trying to avoid getting background points with NA.
bg <- randomPoints(envs.backg[[9]], n= 600)
bg <- as.data.frame(bg)

# Notice how we have pretty good coverage (every cell).
plot(envs.backg[[1]], legend=FALSE)
points(bg, col='red')


#Jacknife
jack <- get.jackknife(presences, bg)

plot(envs.backg[[1]], col='gray', legend=FALSE)
points(occs, pch=21, bg=jack$occ.grp)  # note that colors are repeated here

#Running ENMeval
eval1 <- ENMevaluate(presences, envs, bg, method='jackknife', RMvalues=c(1,2), fc=c('Q'), algorithm='maxent.jar')

eval2 <- ENMevaluate(presences, envs, bg, method='jackknife', fc=c('Q'), algorithm='maxent.jar')

#https://github.com/xavi-rp/enm_eval/blob/master/enm_eval 
eval1 <- ENMevaluate(occ= presences, env = envs, 
                     bg.coords = bg, # matriz o data. frame de dos columnas de la long y lat de los puntos de background
                     categoricals = NULL, method = "jackknife", #categoricals = indicar si hay alguna variable categorica
                     occ.grp = NULL, bg.grp = NULL, 
                     fc = "Q", #El ajuste de nuestras regresiones, L = Linear, Q = cuadratico, H = Hinge, T = Threshol<d 
                     kfolds = 10, #numero de interacciones que queremos en el modelo.  
                     overlap = TRUE, 
                     bin.output = TRUE, clamp = TRUE, 
                     rasterPreds = TRUE, parallel = TRUE, numCores = 5) 

#Para exportar los resultados en un tabla: 
write.csv(eval1@results, file= "Maxent_Results")

#Exploring the results:
eval1@results
eval1@models
eval1@predictions
eval1@overlap
