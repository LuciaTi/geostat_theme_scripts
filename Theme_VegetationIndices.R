###############################
## Theme_ Vegetation Indices ##
###############################

setwd("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_theme_scripts")
library(raster)
library(RStoolbox)
library(ggplot2)

lsat <- brick("raster/lsat.tif")
p224r63 <- brick("raster/p224r63.tif")
p224r63m_ndvi <- raster("results/p224r63_2011m_ndvi.tif")

## 1) 3 Possibilities to calculate the NDVI "by hand" ####

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

# plot the standard deviation
plot <- ggR(lsat_VI_sd, geom_raster=TRUE) +
  ggtitle("Comparison of DVI, NDVI and MSAVI") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold")) +
  scale_fill_gradient2(low="darkgreen", mid="yellow", high="red", 
                       midpoint=30,
                       name="Standard\nDeviation\n")
plot



## 4) Comparison via Moving Window Approach (with raster::focal()) ####

# calculate and plot the NDVI if not calculated before
#p224r63_ndvi <- spectralIndices(p224r63, red = "p224r63.3", nir = "p224r63.4", indices = "NDVI")
#ggR(p224r63_ndvi, geom_raster=TRUE) +
#  ggtitle("NDVI (p224r63-image") +
#  theme(plot.title = element_text(size = 12, colour = "black", face="bold"), 
#        legend.title=element_text(size=12, colour="black", face="bold")) +
#  scale_fill_gradient(low="white", high="darkgreen",
#                       name="Values\n")





########## 1. Window with 3x3 cells

# define the matrix for the moving window
window <- matrix(1, nrow=3, ncol=3) # all pixels around center shell contribute equally --> set all matrix-values to 1


# calculate and plot the variance within the window (per center pixel while moving)
p224r63m_ndvi_3x3_var <- focal(p224r63m_ndvi, w=window, fun=var)
ggR(p224r63m_ndvi_3x3_var, geom_raster=TRUE) +
  ggtitle("NDVI-variation (p224r63m-image") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold"), 
        legend.title=element_text(size=12, colour="black", face="bold"), 
        plot.background = element_rect(fill="white")) +
  scale_fill_gradientn(colours=rev(rainbow(4)),
                       na.value="white",
                         name="Variation\n")



########## 2. Compare windows of different sizes

# calculate the sd over windows of different sizes
focals <- lapply(c(3, 7, 11, 15), function(winDim){ # lapply applys the same FUN argument to all elements, returns result as a list
  
                                                   # define the FUN argument
                                                   window <- matrix(data = 1, ncol=winDim, nrow=winDim) # define windwo size
                                                   focal(p224r63m_ndvi, w=window, fun=sd) # apply the moving window
                                                   
                                                   })
focal_sd_winSizes <- stack(focals)
names(focal_sd_winSizes) <- c("w3x3", "w7x7", "w11x11", "w15x15")

#(!!! Plot is not working yet!)
ggR(focal_sd_winSizes, geom_raster=TRUE) +
  ggtitle("NDVI-variation ~ window size (p224r63m-image") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold"), 
        legend.title=element_text(size=12, colour="black", face="bold")) +
  facet_wrap(~ names(focal_sd_winSizes)) +
  scale_fill_gradientn(colours=rev(rainbow(4)),
                       na.value="white",
                       name="Variation\n")




