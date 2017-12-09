#######################################
## Theme _ Land Cover Classification ##
#######################################

setwd("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_theme_scripts")

library(RStoolbox)
library(raster)

lsat <- brick("lsat.tif")


## 1) Unsupervised L.C.Classification - First Possibility ####
uc <- unsuperClass(lsat, nClasses = 3)
plot(uc$map)

ggR(uc$map, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle("Unsupervised classification 1") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold"))

# + scale_colour_manual(values = c("white", "blue", "red"),  ## didn´t work yet...
#                      labels = c("water", "urban", "forest"))


## 2) Unsupervised L.C.Classification - Second Possibility ####
# run an unsupervised classification 2
plotRGB(lsat, stretch = "lin")
ext <- drawExtent()
lsat_ext <- crop(lsat, ext)

lsat_ext.kmeans <- kmeans(lsat_ext[], 3) # read in as matrix (kmeans can´t read raster)
lsat.kmeans <- kmeans(lsat[], 3)
kmeansraster <- raster(lsat_ext) # re-create a raster from the result
kmeansraster.big <- raster(lsat)
kmeansraster[] <- lsat_ext.kmeans$cluster # fill up the empty raster with values
kmeansraster.big[] <- lsat.kmeans$cluster

plot(kmeansraster)
plot(kmeansraster.big)

ggR(kmeansraster.big, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle("Unsupervised classification 2") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold"))

plotRGB(lsat, stretch = "lin")
plot(kmeansraster.big) # plot with pre-defined colours
click(kmeansraster.big, n = 3)
arg <- list(at = seq(1,3,1), labels = c("forest", "water", "non-forest"))
col <- c("darkgreen", "blue", "red")

plot(kmeansraster.big, col = col, axis.arg = arg) 
