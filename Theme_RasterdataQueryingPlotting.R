###############################################
## Theme _ RasterData, Querying and Plotting ##
###############################################

setwd("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_theme_scripts")
library(raster)
library(RStoolbox)
library(sp)
library(ggplot2)

## 1) Creating Raster Layer ####
library(raster)
r1 <- raster(nrows =10, ncols =10) # create a raser with 10 rows and 10 columns
r1 # get information about the raster
plot(r1) # plot the empty raster --> no values associated.
r1[] <- rnorm(100) # fill the empty raster with 100 random values.
r2 <- raster(nrows =10, ncols =10)  # same for second raster
r2[] <- rnorm(100)
raster1 <- stack(r1, r2)            # stack the two rasters together
raster1[[3]] <- rnorm(100)          # add third raster directly
plot(r1)    
plot(raster1) # and plot these values.

## 2) Creating SpatialPoints Objects ####
# create a vector (spatial points)
library(sp)
poi1 <- cbind(c(rnorm(10)), c(rnorm(10))) # create 2x10 random coordinates. store them in table (2 columns)
poi1 # show the coordinates
poi1.sp <- SpatialPoints(poi1) # convert the list of coordinates to a spatial object
plot(poi1.sp) # plot this object.

## 3) Creating SpatialPointsDataFrames ####
# see also: 2) SpatialPoints Objects
df <- data.frame(attr1 = c("a", "b", "z", "d", "e", "q", "w", "r", "z", "y"), attr2 =c(101:110))
poi1.spdf <- SpatialPointsDataFrame(poi1.sp, df)
plot(poi1.spdf)

## 4A) Querying Raster Data ####
## example data set: lsat (package RStoolbox)
library(RStoolbox)
lsat # information about the data set.
data(lsat) # load/crate the example data from the package.
x <- lsat[1:10, ] # values of rows 1:10
x <- lsat[] # all values
x <- getValues(lsat) # all values
x <- lsat[lsat$B2_dn < 20] # only values, where value from band 2 is < 20.

# plot only band 1
plot(lsat$B1_dn)
plot(lsat[[1]])

# extraxt band 2 and 3 and save them in a new object.
lsat_extract <- lsat[[2:3]]
lsat_extract <- lsat[[c(2,3)]]
lsat_extract <- c(lsat$B2_dn, lsat$B3_dn)

# query or mask values from lsat.
lsat_sub <- lsat[8, 10] # pixel of 8th row and 10th column per band
lsat_sub <- lsat[1:10, ] # pixels from rows 1:10 in each column per band
lsat_sub <- lsat[500] # value per band with the cell-ID 500
queryRaster <- (lsat[[(2)]] < 20) # create a query raster: in band 2 show only values which are < 20
lsat_sub <- lsat[queryRaster]
head(lsat_sub)

# get the corner pixels of the raster data
nr <- nrow(lsat)
nc <- ncol(lsat)
lsat[c(1, nr), c(1, nc)] # (for each band)
lsat$B4_dn[c(1, nr), c(1, nc)] # (for band 4)

# some information about the data.
length(lsat[[2]]) # number of values in band 2
cellStats(lsat, max) # maximum value of each band
cellStats(lsat$B2_dn, min) # minimum value of band 2
lsat[] <- rnorm(ncell(lsat)) # cells are filled with normal distributed data. ncell is number of entries.
lsat[lsat < 0] <- NA # set all values in lsat <0 to NA

## 4B) Querying Raster Data ####
# load example-vector data from the RStoolbox package:
poly <- readRDS(system.file("external/trainingPolygons.rds", package="RStoolbox"))
poly # --> Spdf with 36 features

# create a new raster layer with proberties of poly and 100 values
env <- raster(poly, vals = rnorm(100))  # poly defines the properties for the new raster Layer (same as for object poly)
# vals: values for the new raster layer.

# extract values from env at the location of another spdf, here poly
extract(env, poly)

## 5) Writing/Safing RasterData (on harddisk) ####
data(lsat) # load raster example data from package RStoolbox
writeRaster(lsat,
            "C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_theme_scripts/lsat.grd")
writeRaster(lsat,
            "C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_theme_scripts/lsat.tif")

## 6) Loading Raster Data, Adding/deleting layers (from harddisk) ####

# loading raster data from working directory
lsat_1 <-  raster("lsat.tif", band=1) # / loading layers or singel bands
lsat_2 <-  raster("lsat.tif", band=2)
lsat_1.2 <- stack(band_1, band_2) # stack single bands from "several sources" together
all_lsat <- brick("lsat.tif") # load several bands from the same source at once

# adding or deleting bands
lsat_Plus <- stack(all_lsat, all_lsat[[1]]) # stack another additional band to the rasters
lsat_Plus_minus <- dropLayer(lsat_Plus, 8) # delete the same layer again (it´s the eigth layer)


## 7A) Plotting Raster Data - plotRGB() ####

lsat <- brick("lsat.tif") # load the data
plotRGB(lsat, 3,2,1, 
        stretch="hist",
        axes=TRUE,
        main = "lsat - true color", 
        xlab="x-values", 
        ylab="y-values",
        colNA="black")
# (!!!) labels and title work only, if plot window got cleared before!


## 7B) Plotting Raster Data - ggR() ####
lsat <- brick("lsat.tif") # load the data

## plotting a colour scale
p <- ggR(lsat, layer = 1, stretch = "lin", geom_raster = TRUE) +
  scale_fill_gradient(low = "white", high = "blue", 
                      name = "values\n") +
  ggtitle("Lsat - Layer 1") +
  xlab("x-values") +
  ylab("y-values") +
  theme(plot.title = element_text(color="blue", size=15, face="bold.italic"),
        axis.title.x = element_text(color="darkblue", size=12, face="bold"),
        axis.title.y = element_text(color="darkblue", size=12, face="bold"), 
        legend.text = element_text(size = 11, colour = "darkred"),
        legend.title = element_text(color="darkred", size=12, face="bold"),
        legend.justification = c("right", "bottom"),
        legend.direction = "vertical",
        legend.spacing = unit(2,"lines"),
        legend.key.width = unit(3, "lines"),
        legend.key.height = unit(2, "lines"))
p

p + guides(fill=guide_legend(keywidth=1, keyheight=1, default.unit="lines")) # change the legend



# plotting in grey colors
ggR(lsat, layer = 4, maxpixels = 1e6, stretch = "hist") 


## 7C) Plotting Raster Data - ggRGB ####
lsat <- brick("lsat.tif") # load the data
ggRGB(lsat, 3,2,1, stretch = "lin") +
  ggtitle("lsat - true color plotting") +
  xlab("x-values") +
  ylab("y-values") +
  theme(
    plot.title = element_text(color="blue", size=15, face="bold.italic"),
    axis.title.x = element_text(color="darkblue", size=12, face="bold"),
    axis.title.y = element_text(color="darkblue", size=12, face="bold"))


## 8) Plotting smaller Extents ####
lsat <- brick("lsat.tif") # load the data
plotRGB(lsat, 3,2,1, stretch="lin")
ext <- drawExtent()
lsat_ext <- crop(lsat, ext)
plotRGB(lsat_ext, 3,2,1, stretch="lin")
