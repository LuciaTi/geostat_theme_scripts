#######################################
## Theme _ Land Cover Classification ##
#######################################

setwd("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_theme_scripts")

library(RStoolbox)
library(raster)
library(ggplot2)
library(rgdal)
library(wrspathrow)

lsat <- brick("raster/lsat.tif")
p224r63 <- brick("raster/crop_p224r63_all_bands.tif")


## 1) Unsupervised L.C.Classification - unsuperClass() ####
uc <- unsuperClass(lsat, nClasses = 3)
plot(uc$map)

cols <- c("1"="darkgreen", "2"="blue", "3"="darkred")
ggR(uc$map, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle("Unsupervised classification 1") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold")) +
  scale_fill_manual(values = cols, 
                    labels=c("Class1: Forest", "Class2: Water", "Class3: non-Forest"), 
                    name = "Classes\n")


## 2A) Unsupervised L.C.Classification - kmeans() and correction for NAs ####
plotRGB(lsat, stretch = "lin") # plot the original image
lsat.kmeans <- kmeans(lsat[], 3) # read in as matrix (kmeans can?t read raster)
kmeansraster <- raster(lsat) # re-create a raster from the result
kmeansraster[] <- lsat.kmeans$cluster # fill up the empty raster with value

# dealing with missing values
kmeansraster <- lsat[[1]] # create a copy file with same properties as lsat-raster...
kmeansraster[] <- NA # ... and fill it with NAs
values <- getValues(lsat) # extract values from raster layers and store as variable
valid <- complete.cases(values) # create a mask-layer only with rows(?) without NAs
lsat_kmeans <- kmeans(values[valid, ], # run the kmean clustering -->
                      3, 
                      iter.max=100, 
                      nstart=3)
kmeansraster[valid] <- lsat.kmeans$cluster


plot(kmeansraster) # first possibility for plotting 
plot(kmeansraster)


cols <- c("1"="blue", "2"="darkred", "3"="darkgreen") # second possiblity for plotting
ggR(kmeansraster, forceCat = TRUE, geom_raster = TRUE) + 
  ggtitle("Unsupervised classification 2") +
  scale_fill_manual(values = cols, 
                    labels=c("Class1: Water", "Class2: non-Forest", "Class3: Forest"), 
                    name = "Classes\n")

  
plotRGB(lsat, stretch = "lin") # third possibility for plotting
plot(kmeansraster) # plot with pre-defined colours
click(kmeansraster, n = 3)
arg <- list(at = seq(1,3,1), labels = c("forest", "water", "non-forest"))
col <- c("darkgreen", "blue", "red")
plot(kmeansraster, col = col, axis.arg = arg)



## 2B) Unsupervised Classification - kmeans() with included ndvi ####

p224r63.ndvi <- spectralIndices(p224r63, red=3, nir=4, index="ndvi") # calculate the ndvi
p224r63.ndvi_stack <- stack(p224r63, p224r63.ndvi) # stack image an ndvi together
p224r63_uc.ndvi <- unsuperClass(p224r63.ndvi_stack, nClasses=3) # calculate the classification from stack
p224r63_uc <- unsuperClass(p224r63, nClasses=3) # calculate the Classification without ndvi for comparison
plot(p224r63_uc$map)


cols1 <- c("1"="blue", "2"="darkgreen", "3"="darkred")
plot1 <- ggR(p224r63_uc$map, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle("Unsupervised classification - without NDVI") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold")) +
  scale_fill_manual(values = cols1, 
                    labels=c("Class1: Forest", "Class2: Water", "Class3: non-Forest"), 
                    name = "Classes\n")
plot1

cols2 <- c("1"="darkgreen", "2"="darkred", "3"="blue")
plot2 <- ggR(p224r63_uc.ndvi$map, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle("Unsupervised classification - with NDVI") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold")) +
  scale_fill_manual(values = cols2, 
                    labels=c("Class1: Forest", "Class2: non-Forest", "Class3: Water"), 
                    name = "Classes\n")
plot2

# multiplot(plot1, plot2, col=1)
# multiplot-function from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/



## 3A) Supervised Landcover Classification - randomForest (model="rf) ####

lsat <- brick("raster/lsat.tif") # load the data
p224r63 <- brick("raster/crop_p224r63_all_bands.tif")

# read in the training data
td <- readOGR("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_theme_scripts/vector",
              "classification_1_data")
# run the classification
sc <- superClass(p224r63, 
                 trainData=td, 
                 responseCol="id", 
                 trainPartition=0.7) # define the percentage of training-/validation data

# plot the classification
plot(sc$map)

cols <- c("1"="blue", "2"="darkgreen", "3"="darkred")
ggR(sc$map, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle("Supervised classification 1") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold")) +
  scale_fill_manual(values = cols, 
                    labels=c("Class1: Forest", "Class2: Water", "Class3: non-Forest"), 
                    name = "Classes\n")

writeRaster(sc$map, filename="results/sc_p224r63_1.tif") # safe the map to put it in QGIS for example.


## 3B) Supervised Landcover Classification - randomForest ("rf") but with class streets ####

# same classification, but additionally with class "streets"
td_2 <- readOGR("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_theme_scripts/vector",
              "classification_1_data_2")
# run the classification
sc_2 <- superClass(p224r63, 
                   trainData=td_2, 
                   responseCol="id")

# plot the classification
plot(sc_2$map)

cols <- c("1"="blue", "2"="darkgreen", "3"="orange", "4"="darkred")
ggR(sc_2$map, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle("Supervised classification 1") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold")) +
  scale_fill_manual(values = cols, 
                    labels=c("Class1: Water", "Class2: Forest", "Class3: non-Forest", "Class4: Streets"), 
                    name = "Classes\n")

writeRaster(sc_2$map, filename="results/sc_p224r63_2.tif") # safe the map


## 4) supervised Classification - models "rf","smvRadial","pls" and Entropy ####

# read in the training data
td <- readOGR("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_theme_scripts/vector",
              "classification_1_data")

# run the classification
#sc <- superClass(p224r63, trainData=td, responseCol="id", model=)

model_names <- c("rf", "svmRadial", "pls") # define witch models shell be used

for (i in 1:length(model_names)){ # for the whole vector of models (model[1] to last model) ...
  sc_3 <- superClass(p224r63, 
                   trainData=td, 
                   responseCol = "id",
                   model=model_names[i]) # ... run the classification like so and use model[i]]
  
  names(sc_3$map) <- model_names[i] # directly rename the name of the layers (new name == name of used model)
  
  # plot each classifcation
  p <- ggR(sc_3$map, forceCat = TRUE, geom_raster = TRUE) +
    ggtitle(paste("Supervised classification - model:", model_names[i])) +
    theme(plot.title = element_text(size = 12, colour = "black", face="bold")) +
    scale_fill_manual(values = cols, 
                      labels=c("Class1: Water", "Class2: Forest", "Class3: non-Forest"), 
                      name = "Classes\n")
  print(p)
  
  if (i==1){                     # in the first run: safe the sc-result as sc_stack
            sc_3_stack <- sc_3$map
           } else{                        # later on: stack the actual sc-result with sc_stack 
                  sc_3_stack <- stack(sc_3_stack, sc_3$map)
                 }
}


# plot the classification

cols <- c("1"="blue", "2"="darkgreen", "3"="darkred")
ggR(sc_3$map["pls"], forceCat = TRUE, geom_raster = TRUE) +
  ggtitle("Supervised classification - model pls") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold")) +
  scale_fill_manual(values = cols, 
                    labels=c("Class1: Forest", "Class2: Water", "Class3: non-Forest"), 
                    name = "Classes\n")

ggR(test, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle(paste("Supervised classification - model:", model_names[3])) +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold")) +
  scale_fill_manual(values = cols, 
                    labels=c("Class1: Water", "Class2: Forest", "Class3: non-Forest"), 
                    name = "Classes\n")



