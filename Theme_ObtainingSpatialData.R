############################
## Theme _ Obtaining Spatial Data ##
############################

setwd("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_theme_scripts")

## 1) Precipitation Data of Germany ####

library(raster)

# check the ISO3-codes for the country
getData("ISO3")

# save all the country codes as variable
# and query the code of the country of interest
x <- getData("ISO3")
names(x)
x$ISO3[x$NAME == "Germany"]

# get the surface/shape/map of country borders of germany
germany <- getData("GADM", country = "DEU", level = 2)
plot(germany)

# get the precipitation data of germany
prec_germany <- getData("worldclim", var = "prec", res = .5, lon = 10, lat = 51)
plot(prec_germany)

# crop the precipitation data to the extend of germany`s country borders
prec_germany1 <- crop(prec_germany, germany) # crop "reduces" image to extent of germany.
spplot(prec_germany1)

# mask "reduces" image to surface of germany.
prec_germany2 <- mask(prec_germany1, germany)
spplot(prec_germany2)

# calculate and plot the mean precipitation values:
prec__germany_avg <- cellStats(prec_germany2, stat = "mean")
plot(prec_avg, 
     pch = 19, 
     col = "red", 
     cex = 1.5, 
     ylab = "average precipitation", 
     xlab = "month", 
     cex.lab = 1.5)
lines(lowess(prec__germany_avg, f = .2)) # add a line connecting the points. f: smoothness-factor, larger values give more smoothness


## 2) Precipitation of Bavaria #### 
bavaria = germany[germany$NAME_1 == "Bayern", ] # define the part of the germany-data for bavaria
plot(bavaria)
prec_bavaria = mask(crop(prec_germany, bavaria), bavaria)
plot(prec_bavaria)

# plot the precipitation of Bavaria in December compared to July
par(mfrow = c(1,2), mar = c(4,4,5,5), mgp = c(2, 1, 0))
plot(prec_bavaria$prec12_16, 
     xlab = "December", 
     legend.args=list(text="precipitation [ml]", side = 2, font=2, line = 0.5, cex=1.1))
plot(prec_bavaria$prec7_16, 
     xlab = "July", 
     legend.args=list(text="precipitation [ml]", side = 2, font=2, line = 0.5, cex=1.1))
# mtext("Precipitation Bavaria\nDecember vs. July", side = 3, line = 3, outer = T)  --> metext() didn?t work ...?



## 3) Precipitation of Germany vs. South Africa #### 

# plot the precipitation of South Africa (compared to Germany - boxplot)

library(raster) # load the raster package

# save all the country codes as variable
# and query the code of the country of interest
x <- getData("ISO3")
names(x)
x$ISO3[x$NAME == "South Africa"]

s_africa = getData("GADM", country = "ZAF", level = 2) # get the country boarders
plot(s_africa)
# get long(x) and lat(y) values
mean(c(xmin(s_africa), xmax(s_africa))) # long value: 24
mean(c(ymin(s_africa), ymax(s_africa))) # lat value: - 29
prec_s_africa.1 = getData("worldclim", var = "prec", res = .5, lon = 24, lat = -29) # get the precipitation data --> not all is covered
prec_s_africa.2 = getData("worldclim", var = "prec", res = .5, lon = 23, lat = -33)
prec_s_africa.3 = getData("worldclim", var = "prec", res = .5, lon = 31, lat = -28)
prec_s_africa = mosaic(prec_s_africa.1, prec_s_africa.2, prec_s_africa.3, fun = mean) # connect the datasets
prec_s_africa2 = mask(crop(prec_s_africa, s_africa), s_africa) # reduce the data to the country borders of south Africa.
plot(prec_s_africa2) # check of data is correct

prec_s_africa_avg <- cellStats(prec_s_africa2, stat = "mean") # calculate mean vlaues for south africa

prec__germany_avg <- cellStats(prec_germany2, stat = "mean")


par(mgp = c(3, 1.5, 0), mfrow = c(1,1)) # move labels further from the plot
prec_total_avg = cbind(prec__germany_avg, prec_s_africa_avg) # connect the precipitation from germany and s_africa to one dataframe
# and plot it comparing the countries in a boxplot
boxplot(prec_total_avg, 
        xlab = "Countries",
        ylab = "Mean Precipitation [ml]", 
        names = c("mean\nGermany", "mean\nSouth Africa"), 
        main = "Mean Precipitation\nGermany vs. South Africa", 
        cex.lab = 1.3, 
        cex.names = 1.3, 
        cex.axis = 1.2, 
        las = 1)

## 4) Elevation Data of Germany ####

library(raster)

germany_0 <- getData("GADM", country = "DEU", level = 0) # get the country borders around Germany
germany_1 <- getData("GADM", country = "DEU", level = 1) # get the county borders within Germany
round(coordinates(germany_0), digits = 0) # check the mid-coordinates of Germany. round them for the getData command
elev_germany <- getData('alt', country = "DEU") # download elevation data for Germany
plot(elev_germany)

elev_germany_crop <- crop(elev_germany, germany_0) # reduce the extent of the object
elev_germany_cropmask <- mask(elev_germany_crop, germany_0) # reduce to shape of germany

# plot the elevation data and the country borders
plot(elev_germany_cropmask)
plot(germany_0, add = TRUE)
plot(germany_1, add = TRUE)


