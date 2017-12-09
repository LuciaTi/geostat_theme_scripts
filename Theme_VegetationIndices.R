###############################
## Theme_ Vegetation Indices ##
###############################

setwd("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_theme_scripts")
library(raster)
library(RStoolbox)

## 1) 3 Possibilities to calculate the NDVI "by hand" ####

lsat <- brick("lsat.tif")

# 1- type it by hand
lsat_ndvi <- (lsat$lsat.4 - lsat$lsat.3)/(lsat$lsat.4 + lsat$lsat.3)
plot(lsat_ndvi)

# 2- overlay: normally for single-layer objects (RasterLayer)
lsat_ndvi2 <- overlay(lsat$lsat.4, lsat$lsat.3, fun = function(nir, red){(nir-red)/(nir+red)})
plot(lsat_ndvi2)

# 3- calc: normally for mulitylayer-objects, like RasterStack or RasterBrick
lsat_ndvi3 <- calc(lsat, fun = function(x){(x[, 4] - x[, 3])/(x[, 4] + x[, 3])})
plot(lsat_ndvi3)

## 2) Calculate VIs with RStoolbox::spectralIndices() ####

lsat_ndvi <- spectralIndices(lsat, red = "lsat.3", nir = "lsat.4", indices = "NDVI")
plot(lsat_ndvi, main="ndvi")

lsat_DVI <- spectralIndices(lsat, red = "lsat.3", nir = "lsat.4", indices = "DVI")
plot(lsat_DVI, main="DVI")

lsat_slavi <- spectralIndices(lsat, red = "lsat.3", nir = "lsat.4", indices = "SAVI")
plot(lsat_slavi, main="slavi")

lsat_MSAVI <- spectralIndices(lsat, red = "lsat.3", nir = "lsat.4", indices = "MSAVI")
plot(lsat_slavi, main="msavi")

## 3) Calculate the standard deviation between several VIs ####
vi_stack <- stack(lsat_DVI, lsat_ndvi, lsat_MSAVI) # produce a stack with the Vis to compare
lsat_VI_sd <- calc(vi_stack, fun = sd) # calculate the sd
plot(lsat_VI_sd) # plot the sd



