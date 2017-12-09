#############################
## Theme_Functions_If_etc. ##
#############################

setwd("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_theme_scripts")

## 1) IF -Statements ####

a <-  sqrt(2) 
if(a*a != 2) # define the condition.
{
  print("R is great!")
}

## 2) WHILE -Statements ####

j <- 0
while(j < 1)
{
  j <- j+0.1 ; print(j)
}

## 3) Generell Definition of Functions ####

myfunction <- function(x, y){
  z <- x+y
  return(c("z is:", z))
}
myfunction(4,3)


############## or


myfunction <- function(x, y){   
  x+y
}
myfunction(4,3)
# --> same function!
# return() only needed, if you create many values in the function which are needed later on.


## 4) Functions for Remote Sensing ####

lsat <- brick("lsat.tif") # load the data

# calculate the standard defiation between bands
lsat_sd <- calc(lsat, fun = sd) # calc works for all bands, not for single  rasters!
plot(lsat_sd)

# define a function to divide the layer values of one layer by 10
fun <- function(x){x/10}
raster_output <-  calc(lsat$lsat.4, fun)
plot(raster_output)

# define a function which changes NA values to -999
fun2 <- function(x){x[is.na(x)] <- -999; return(x)}
raster_output2 <- calc(lsat$lsat.4, fun2)
plot(raster_output2)

# define a function which sums up values
raster_output3 <- calc(lsat, fun = function(x){x[1]+x[2]*x[3]})
plot(raster_output3)

# define a function to calculate the NDVI
fun_ndvi <- function(nir, red){(nir-red)/(nir+red)}
# more on Vegetagtion indices: --> see also Theme_script "Theme_VegetationIndices"


# calculate and plot a linear model:
# Check, how pixel values from sepparate layers are linked.
# Here: compute a linear model, bands[1:5] are depending variable, bands[6:10] are explaining.
# band1/band6, band2/band7 and so on.
# coefficients: 1 is intercept, 2 is slope
### the following example doesn´t make sense, but one could compare the occurrence of certain Species
### (stored on one layer) with the productivity of the vegetation (stored on the other layer)
plotRGB(lsat, stretch = "lin") # plot the image to create a smaller extent
ext = drawExtent() # choose the extent by clicking
lsat_crop <-crop(lsat, ext) # crop the image to smaller extent
lsat_double <- stack(lsat_crop, lsat_crop) # produce a stack of layers to compare
fun <- function(x){lm(x[1:5] ~ x[6:10])$coefficients[2]} # define the function for the calculation
raster_output4 <- calc(lsat_double, fun) # calculate the linear model
plot(raster_output4) # plot the model
#calc(raster12, fun = fun, filename ="raster12_lm")

