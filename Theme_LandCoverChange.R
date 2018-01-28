######################################
## Theme Land Cover Change Analysis ##
######################################

setwd("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_theme_scripts")
library(raster)
library(RStoolbox)
library(rgdal)
library(ggplot2)


## 1) Post-Classification Comparison + Spatial Comparison with rasterEntropy() ####

########### 1. load the data and run a classification for each year/data set
p224r63_2011 <- brick("raster/p224r63_2011.grd")
p224r63_1988 <- brick("raster/p224r63_1988.grd")

# draw a smaller extent, crop images and safe the results for working in QGIS
ex <-  drawExtent()
p224r63_2011_crop <- crop(p224r63_2011, ex)
writeRaster(p224r63_2011_crop, "results/p224r63_2011_crop.grd")
p224r63_1988_crop <- crop(p224r63_1988, ex)
writeRaster(p224r63_1988_crop, "results/p224r63_1988_crop.grd")
# > ex
#class       : Extent 
#xmin        : 603015 
#xmax        : 626925 
#ymin        : -498165 
#ymax        : -482175

# --> take vector data (training and validation) for each year in QGIS to run a supervised LandCoverClassification
# read in the training-/validation data and the scenes
td_2011 <- readOGR("vector", "ChangeAnalysis1_td_2011")
vd_2011 <- readOGR("vector", "ChangeAnalysis1_vd_2011")
td_1988 <- readOGR("vector", "ChangeAnalysis1_td_1988")
vd_1988 <- readOGR("vector", "ChangeAnalysis1_vd_1988")
p224r63_1988_crop <- brick("results/p224r63_1988_crop.grd")
p224r63_2011_crop <- brick("results/p224r63_2011_crop.grd")


# Classify the images from both years
# _1 with validation data, _2 with partition=0.6
set.seed(6) # fix the random number
sc_2011_1 <- superClass(p224r63_2011_crop, 
                      trainData=td_2011, 
                      responseCol="id", 
                      valData=vd_2011)
set.seed(6) # fix the random number
sc_2011_2 <- superClass(p224r63_2011_crop, 
                        trainData=td_2011, 
                        responseCol="id", 
                        trainPartition=0.6)

plot(sc_2011_1$map)
cols <- c("1"="darkgreen", "2"="yellow", "3"="blue")
ggR(sc_2011_1$map, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle("p224r63_crop: Supervisedclassification_2011") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold")) +
  scale_fill_manual(values = cols, 
                    labels=c("Class: forest", "Class2_non-forest", "Class3: water"), 
                    name = "Classes\n")

set.seed(6)
sc_1988_1 <- superClass(p224r63_1988_crop, 
                      trainData=td_1988, 
                      responseCol="id", 
                      valData=vd_1988)
set.seed(6)
sc_1988_2 <- superClass(p224r63_1988_crop, 
                      trainData=td_1988, 
                      responseCol="id", 
                      trainPartition=0.6)

plot(sc_1988_1$map)
cols <- c("1"="darkgreen", "2"="yellow", "3"="blue")
ggR(sc_1988_1$map, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle("p224r63_crop: Supervisedclassification_1988") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold")) +
  scale_fill_manual(values = cols, 
                    labels=c("Class: forest", "Class2_non-forest", "Class3: water"), 
                    name = "Classes\n")



########### 2. Run the Change Analysis

# safe the Classification information as raster files
# also multiply one of the raster by 10, then summ both files to reach the Change classes:
# Class1 * 10 = 10. Plus Class1 = 11.  
# --> new Classes will 11, 12, 13, 21, 22, 23, 31, 32, 33
# --> 11 means Class1 to Class1, 32 means: Class3 to Class 2, etc....
sc_1988_1_map_multiplied <- sc_1988_1$map * 10
sc_1988_2_map_multiplied <- sc_1988_2$map * 10
sc_2011_1_map <- sc_2011_1$map
sc_2011_2_map <- sc_2011_2$map

changeClasses_1988_2011_1 <- sc_1988_1_map_multiplied + sc_2011_1_map
changeClasses_1988_2011_2 <- sc_1988_2_map_multiplied + sc_2011_2_map

plot(changeClasses_1988_2011_1)
ggR(changeClasses_1988_2011_1, forceCat=TRUE, geom_raster=TRUE) +
  ggtitle("p224r63_crop: Change Classes\nPost-Classification-Comparison_1") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold")) +
  scale_fill_manual(values=rev(rainbow(9)), 
                    labels=c("forest : forest", "forest : non-forest", "forest : water",
                             "non-forest : forest", "non-forest : non-forest", "non-forest : water",
                             "water : forest", "water : non-forest", "water : water"), 
                    name = "Change Classes\n")



########### 3. Validate the Change Classification

# create validation data (vector) in QGIS containing the examples for the ChangeClasses and load it
vd_change_1988_2011 <- readOGR("vector", "ChangeClasses_1988_2011")

val_change_1988_2011_1 <- validateMap(map=changeClasses_1988_2011_1, valData=vd_change_1988_2011, responseCol="id")
val_change_1988_2011_1 # the total accuracy is 0.8658

val_change_1988_2011_2 <- validateMap(map=changeClasses_1988_2011_2, valData=vd_change_1988_2011, responseCol="id")
val_change_1988_2011_2 # the total accuracy is 0.8613



########## 4. Spatial Comparison with rasterEntropy()
comparison_stack_1988_2011 <- stack(changeClasses_1988_2011_1, changeClasses_1988_2011_2)
comparison_change_1988_2011_1_2 <- rasterEntropy(comparison_stack_1988_2011)

ggR(comparison_change_1988_2011_1_2, geom_raster=TRUE) +
  ggtitle("p224r63_crop: Post-Classification-Comparison\nComparison of valData and trainPartition Approach") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold"), 
        legend.title = element_text(size=11, colour="black", face="bold")) +
  scale_fill_gradient(low="white", high="darkred", 
                      name="Entropy\n")






## 2) Spectral Change Vector Analysis ####

# tow outputs are calculated: the magnitude and the direction of the change between two datasets
# following: the Tasseled Cap Components are calculated: brightness and greenness

# read in the raster data of two years
p224r63_1988 <- brick("C:/Users/Ltischer/Documents/Studium/A Master/HIWI_GIS/raster_data/final/p224r63_1988.gri")
p224r63_2011 <- brick("C:/Users/Ltischer/Documents/Studium/A Master/HIWI_GIS/raster_data/final/p224r63_2011.gri")

# calculate greeness, wetness and brightness for each of the tow years
# only sre-layers are used --> without layer 6
tc_1988 <- tasseledCap(dropLayer(p224r63_1988, "B6_bt"), sat="Landsat5TM") # calculate without layer 6
tc_2011 <- tasseledCap(dropLayer(p224r63_2011, "B6_bt"), sat="Landsat5TM")

# calculate magnitude for the change between rasters
# magnitude: calculated as the euclidean distance between two corresponding pixels in 2D-space (spanned by the two axes brightness/x-axis and greenness/y-axis)
# direction: angle between pixels realtive to first axis (brightness)
changeVector_1988_2011 <- rasterCVA(tc_2011[[1:2]], tc_1988[[1:2]])


# plot the result
plot(changeVector_1988_2011[[1]])
plot(changeVector_1988_2011[[2]])

ggR(changeVector_1988_2011[[1]], geom_raster=TRUE) +
  ggtitle("p224r63: Spectral Change Vector Analysis - angle") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold")) +
  scale_fill_gradient2(low="darkgreen", mid="white", high="red",
                      midpoint=180,
                      name = "Degrees\nof Change\n")


ggR(changeVector_1988_2011[[2]], geom_raster=TRUE) +
  ggtitle("p224r63: Spectral Change Vector Analysis - magnitude") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold")) +
  scale_fill_gradient2(low="darkgreen", mid="white", high="red",
                       midpoint=0.575,
                       name = "Magnitude\n")






## 3) Multi-data-Classification (with data from the book) ####

# not "LandCover"-Classes will be created, but "LandCoverCHANGE"-Classes

# read in the data from the years to compare
p224r63_1988 <- brick("C:/Users/Ltischer/Documents/Studium/A Master/HIWI_GIS/raster_data/final/p224r63_1988.gri")
p224r63_2011 <- brick("C:/Users/Ltischer/Documents/Studium/A Master/HIWI_GIS/raster_data/final/p224r63_2011.gri")

# merge the two datasets to one
# !! if the resolution is not identical: resampling is needed!
p224r63_1988_2011 <- stack(p224r63_1988, p224r63_2011)

# create vector data with the change Classes in QGIS and import it
# 6 change Classes: forest-forest, noforest-noforest, forest-noforest, noforest-forest, water-water, nowater-water
change_classes_1988_2011 <- readOGR("C:/Users/Ltischer/Documents/Studium/A Master/HIWI_GIS/vector_data", "change_classes_1988_2011")
change_classes_1988_2011_df <- fortify(change_classes_1988_2011, region="id") # transform for Plotting
change_classes_1988_2011_df_final <- merge(change_classes_1988_2011_df, change_classes_1988_2011@data, by="id")

# plot the two raster datasets each together with the vector data to check correct overlay
cols=c("1"="yellow", "2"="orange", "3"="red", "4"="blue", "5"="white", "6"="green")
ggRGB(p224r63_2011, stretch="lin", geom_raster = TRUE) +
  geom_polygon(data=change_classes_1988_2011_df_final,
               mapping=aes(x=long, y=lat, group=group, col=factor(id)), 
               alpha=0.5, 
               fill=NA) +
  ggtitle("p224r63_2011 with Change Classes 1988_2011") +
  theme(plot.title = element_text(color="black", size=15, face="bold"),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"), 
        legend.text = element_text(size = 11, colour = "darkred"),
        legend.title = element_text(color="darkred", size=12, face="bold")) +
  scale_colour_manual(values = cols, 
                      labels=c("forest_forest", "forest_nonforest", "nonforest_forest", 
                               "nonforest_nonforest", "nowater_water", "water_water"), 
                      name="Change Classes\n")

cols=c("1"="yellow", "2"="orange", "3"="red", "4"="blue", "5"="white", "6"="green")
ggRGB(p224r63_1988, stretch="lin", geom_raster = TRUE) +
  geom_polygon(data=change_classes_1988_2011_df_final,
               mapping=aes(x=long, y=lat, group=group, col=factor(id)), 
               alpha=0.5, 
               fill=NA) +
  ggtitle("p224r63_1988 with Change Classes 1988_2011") +
  theme(plot.title = element_text(color="black", size=15, face="bold"),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"), 
        legend.text = element_text(size = 11, colour = "darkred"),
        legend.title = element_text(color="darkred", size=12, face="bold")) +
  scale_colour_manual(values = cols, 
                      labels=c("forest_forest", "forest_nonforest", "nonforest_forest", 
                               "nonforest_nonforest", "nowater_water", "water_water"), 
                      name="Change Classes\n")




# calculate the actual Change Classification
p224r63_1988_2011_change <- superClass(img=p224r63_1988_2011, nSamples=1000, 
                                       trainData=change_classes_1988_2011, 
                                       responseCol = "class")

# display the Class - ID mapping
p224r63_1988_2011_change$classMapping

cols=c("forest_forest"="darkgreen", "forest_noforest"="red", "noforest_forest"="yellow",
       "noforest_noforest"="grey", "nowater_water"="darkgrey", "water_water"="blue")
ggR(p224r63_1988_2011_change$map, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle("p224r63: Change Classes\nMulti-Date Classifcation 1988_2011") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold")) +
  scale_fill_manual(values=cols, 
                    name="Change Classes")

## 4) Spatial Comparison with rasterEntropy() ####

