###############################
## Theme _ ggplot - Graphics ##
###############################

setwd("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_theme_scripts")

library(ggplot2)
library(ggmap)
library(mapproj)
library(ggalt)
library(gganimate)
# help.search("geom_", package="ggplot2") # get a list of available geometric objects

## 1) Plotting self created label ####

# define the dataset
x <- data.frame(x = 1, y = 1, label = "ggplot2 introduction \n @ EAGLE") 
# plot the dataset.
# Therefore, the previously defined x and y values are used. the defined label is plottet at this place.
ggplot(data = x, aes(x = x, y = y)) + geom_text(aes(label = label), size = 15) 


## 2) Plotting sub-classes ####

# example data set: mpg
ggplot(mpg, aes(x = displ, y =cty, colour = class)) + # set the variables for x and y axis. colours chosen after "class"
  geom_point() +
  geom_smooth()

# same plot, but sub-divided in classes
names(mpg) # check the column names from the data set
ggplot(mpg, aes(x = displ, y = cty)) + # set the variables for x and y axis
  geom_point() + # plot the single data points
  facet_wrap(~class) +  # devides the data into groups for plotting (here: after "class")
  geom_smooth() # adds a smoothed regression line


## 3) ggboxplot - example 1: mpg-dataset #####

# example data set: mpg
ggplot(mpg, aes(x = class, y = cty)) + # define x and y axis
  geom_boxplot(alpha = .5) + # adopt transparency of the boxes
  geom_point(aes(color = hwy), # add points for single data points
             alpha = 0.7, size = 1.5, # adopt transparency and size of the points
             position = position_jitter(width = 0.25, height = 0)) # loosen the points if several on same position
# (!!! jitter can change meaning of plot! not too much!)

## 4) ggboxplots - example 2: self-created data ####

# create the data set
location <- rep(c("location_1", "location_2", "location_3"), each = 20)
plot_ID <- rep(LETTERS[1:10], each = 2)
treatment <- rep(c("short", "long"), each = 1)
value <- runif(300)*500
x <- data.frame(location, plot_ID, treatment, value)


# One box per treatment
ggplot(x, aes(x=location, y=value, fill=treatment)) +
  geom_boxplot() +
  facet_wrap(~treatment) +
  ggtitle("Measurements on different plots\nTreatments") +
  xlab("Location") + 
  ylab("Value") +
  guides(fill=guide_legend(title="Treatment")) +
  theme(plot.title=element_text(face="bold", color="red", size=16), 
        axis.title.x =element_text(face= "italic", color="black", size=14),
        axis.title.y =element_text(face= "italic", color="black", size=14),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



# one box per treatment plus grouping ~ locations
ggplot(x, aes(x=treatment, y=value, fill=location)) +
  geom_boxplot(outlier.colour="red", 
               outlier.shape=16,
               outlier.size=2) +
  facet_wrap(~treatment, scale = "free") +
  ggtitle("Measurements on different plots\nLocations") +
  xlab("Treatment") + 
  ylab("Value") +
  guides(fill=guide_legend(title="Location")) +
  theme(plot.title=element_text(face="bold", color="red", size=16), 
        axis.title.x =element_text(face= "italic", color="black", size=14),
        axis.title.y =element_text(face= "italic", color="black", size=14),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



# one box per location 
ggplot(x, aes(x=treatment, y=value, fill=location)) +
  geom_boxplot(outlier.colour="red", 
               outlier.shape=16,
               outlier.size=2) +
  facet_wrap(~location) +
  ggtitle("Measurements on different plots\nLocations") +
  xlab("Treatment") + 
  ylab("Value") +
  guides(fill=guide_legend(title="Location")) +
  theme(plot.title=element_text(face="bold", color="red", size=16), 
        axis.title.x =element_text(face= "italic", color="black", size=14),
        axis.title.y =element_text(face= "italic", color="black", size=14),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())




## 5) Coplot - example 1 ####

# create the data set
location <- rep(c("location_1", "location_2"), each = 20)
plot_ID <- rep(c("A", "B"), each = 4)
treatment <- rep(c("none", "short", "middle", "long"), each = 1)
value <- runif(200)*500
x <- data.frame(location, plot_ID, treatment, value)

# plot with coplot() fumction:
coplot(value ~ treatment | location*plot_ID,
       data = x, 
       xlab = c("treatment", "location"), 
       ylab = "value", 
       col = "red", pch = 21, bg = "pink", 
       bar.bg = c(fac = "lightblue"))

## 5) Scatterplots - example 1: self-created data ####

# create a data-frame with location_name, plot_ID and two measurements
df <- data.frame(location = "location_name_1", measure1 = runif(100) * 1000, measure2 = round(runif(100) *100), 
                 value = rnorm(100, 2, 1), plot_ID = rep(LETTERS, 100))
# runif creates random numbers (by default from [0,1]). Here n = 100 times.
df_1 <- data.frame(location="location_name_2", measure1 = runif(50)*100, measure2 = round(runif(50)*10),
                   value = rnorm(50), plot_ID = rep(LETTERS, 50))
df_2 <- data.frame(location="location_name_3", measure1 = runif(50)*500, measure2 = round(runif(50)*50),
                   value = rnorm(50), plot_ID = rep(LETTERS, 50))
df_3 = rbind(df, df_1, df_2) # connect the three data frames.



## 1. possibility: plotting depending on locataion_name/qplot
x <- df_3[ , c("location", "measure1", "measure2")] # define the data.
qplot(measure1, measure2, colour = location, data = x) # first possibility.


## 2. possibility: plotting depending on lobation_name/qqplot

# plot the whole data for plot and measure 1 and 2.
x2 <- df_3[ , c("location", "measure1", "measure2")] # define the data.

library(ggplot2)
qplot(measure1, measure2, colour = location, data = x2) # first possibility.

ggplot(x3)+                                         # second possibility
  geom_point(aes(x=x3$measure1,y=x3$measure2,colour=x3$plot))+
  ggtitle("Measurements on different plots") +
  xlab("measure 1") + 
  ylab("measure 2") +
  theme(axis.title.x = element_text(face = "italic", colour = "black", size = 14), 
        axis.title.y = element_text(face = "italic", colour = "black", size = 14),
        plot.title = element_text(face="bold", color="red", size=16), 
        legend.title = element_text(face = "plain", colour = "black", size = 14),
        legend.position="bottom",
        legend.text=element_text(size=11)) +
  labs(colour= "location")


## 3. possibilty: plotting depending on plot_ID
# save only these 4 columns as new data.frame and plot the data.
x2 <- df_3[ , c("location", "measure1", "measure2", "plot_ID")]

ggplot(x)+                                         
  geom_point(aes(x=x2$measure1,y=x2$measure2,colour=x2$plot_ID))+
  ggtitle("Measurements on different plots") +
  xlab("measure 1") + 
  ylab("measure 2") +
  theme(axis.title.x = element_text(face = "italic", colour = "black", size = 14), 
        axis.title.y = element_text(face = "italic", colour = "black", size = 14),
        plot.title = element_text(face="bold", color="red", size=16), 
        legend.title = element_text(face = "plain", colour = "black", size = 14),
        legend.position="bottom",
        legend.text=element_text(size=11)) +
  labs(colour= "Plot-ID")






## 6) ggplot - Steigerwald-dataset + Themes ####
# preparation for the data-download from bitbucket
install.packages("devtools")
library(devtools)
install_bitbucket("EAGLE_MSc/steigerwald", 
                  build_vignette()) # normally within the brackets: ", build_vignettes=TRUE". BUT: download failed, if included...
install.packages("steigerwald")
library(steigerwald)

data("bio_data") # load the dataset
head(bio_data) # have a look at the data --> it´s a list with many different objects.

forest <- data.frame(bio_data[1]) # extraxt one of the data sets

# dot plot, beech basal area vs. ndvi
ggplot(forest, aes(x=forest_short.beech, y=forest_short.ndvi)) + #define the data
  geom_point() + # command to create points
  facet_wrap(~ forest_short.sub_basin) + # create sub-classes
  geom_smooth() # add a smoothed line

# boxplot with point "jitter"
ggplot(forest, aes(forest_short.sub_basin, forest_short.ndvi)) + #define dataset
  geom_boxplot(alpha=0.5) + #command for boxplot
  geom_point(aes(color=forest_short.height), #colors shell depend on "height"
             alpha=0.6, # transparency of points
             size=1.5, # size of points
             position=position_jitter(width=0.25, height=0)) #size of jitter-effect/straightening out the points

# compare: same info, but less informative
ggplot()+
  geom_point(data=forest, aes(forest_short.sub_basin, forest_short.ndvi))

# same, but without color
ggplot()+
  geom_point(data=forest, aes(forest_short.sub_basin, forest_short.ndvi, color=forest_short.height))

## some other plots:
ggplot(forest, aes(x=forest_short.beech, y=forest_short.ndvi))+
  geom_jitter()

ggplot(forest, aes(x=forest_short.beech, y=forest_short.ndvi))+
  geom_boxplot()

ggplot(forest, aes(x=forest_short.beech, y=forest_short.ndvi)) +
  geom_violin() +
  geom_jitter(aes(alpha=0.7, size=2), color="blue") 



# "store" the plot and add new shemes
a <- ggplot() +
  geom_point(data=forest, aes(forest_short.sub_basin, forest_short.ndvi, color=forest_short.height))

a # call plot with global settings

a + theme_bw() # plot with new color sheme

a + theme_grey() # back to default-setting (for this single plot)

theme_set(theme_grey) # set this theme globally for all plots

theme_get() # get the current theme --> attributes can be modified!!!
theme_update() # the current theme is automatically applied to each new plot



## 7) ggplot for Spatial Data - example: Würzburg-Map ####

# used libraries: ggplot2, ggmap, mapproj, ggalt

############ plotting Würzburg
map.wue <- get_map("Wurzburg") # load data of Wurzburg
ggmap(map.wue) # plot the map
ggmap(map.wue, zoom=15)

map <- get_map("Bavaria", zoom=6) # load and plot an overview of Bavaria
ggmap(map)


############ encircle Würzburg and mark singel neighbourhoods
wue <- geocode("Wurzburg") # load the central coordinates of Würzburg
wue_ggl_hybrid_map <- qmap("wue",  # get the Google Hybrid map --> check alternatves (Satellit, roads, openStreetMap)!!
                           zoom=12, 
                           source="google", 
                           maptype="hybrid")

wue_places <- c("Zellerau", # define names of neighbourhoods in Würzburg
                "Sanderau", 
                "Gerbrunn", 
                "Estenfeld")

places_loc <- geocode(wue_places) # load center coordinates of neighbourhoods

# plot the map as before, but with dots on neighbourhoods and a circle around them
wue_ggl_hybrid_map +
  geom_point(aes(x=lon, y=lat),
             data=places_loc, 
             alpha=0.7, 
             size=7, 
             color="tomato") +
  geom_encircle(aes(x=lon, y=lat), 
                data=places_loc, 
                size=2, 
                color="blue")



## 8) ggplot for Spatial Data - example: lsat$B3_dn ####
library(RStoolbox)
data(lsat) # load example data

lsat.df <- data.frame(coordinates(lsat), getValues(lsat)) # create data.frame from coordinates and raster-values
lsat.df <- lsat.df[lsat.df$B1_dn!=0, ] # remove background if needed

ggplot(lsat.df) +
  geom_raster(aes(x=x, y=y, fill=B3_dn)) +
  scale_fill_gradient(na.value=NA) +
  coord_equal()

# add another color sheme
ggplot(lsat.df) +
  geom_raster(aes(x=x, y=y, fill=B3_dn)) +
  scale_fill_gradient(low="black", high="white", na.value=NA) +
  coord_equal()



##### plot the lsat raster data and add a vector data layer
# same plot as before, but "stored"
a <- ggplot(lsat.df) +
  geom_raster(aes(x=x, y=y, fill=B3_dn)) +
  scale_fill_gradient(low="black", high="white", na.value=NA) +
  coord_equal()

# load a spatial vector file form RStoolbox package
poly <- readRDS(system.file("external/trainingPolygons.rds", package="RStoolbox"))

# extract and store the coordinates of the vector data
plots <-  as.data.frame(coordinates(poly))

names(plots) # check aes of plots for plotting
a + guides(fill=guide_colorbar()) +
  geom_point(data=plots, aes(x=V1, y=V2), shape=3, colour="yellow") +
  theme(axis.title.x=element_blank())



##### limit the plotting to extent of lsat to extent of poly
lim <- extent(lsat)
a + guides(fill=guide_colorbar()) +
  geom_point(data=plots, aes(x=V1, y=V2), shape=3, colour="yellow") +
  theme(axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(lim@xmin, lim@xmax)) +
  ylim(c(lim@ymin, lim@ymax))



## 9) Defining an own Theme for plotting ####
# --> see day5_main.pdf/slide 81 and develope own theme!!!

## 10) gganimate with gabminder - Animated ggplot graphics ####

# install package using devtools:
devtools::install_github("dgrtwo/gganimate")

# install gapminder for example data and set the theme
library(gapminder)
theme_set(theme_bw())

# install also imagemagick (visualization programm)
# see: https://www.rdocumentation.org/packages/installr/versions/0.19.0/topics/install.ImageMagick
# ! click the checkbox in the DLL installer that is for "legacy" support that will install "convert".
library(installr)
install.imagemagick("https://www.imagemagick.org/script/download.php")

# point R path to the folder of ImageMagick executables
animation::ani.options(convert="C:/PROGRA~1/ImageMagick-7.0.7-Q16/convert.exe")


# write the plot
p <- ggplot(gapminder, 
            aes(gdpPercap, lifeExp, size=pop, color=continent, frame=year)) + # aes(frame) defines which variable is used for the animation
  geom_point() +
  scale_x_log10()

# call the animated plot
gganimate(p)

# call and save the animated plot as GIF-file
gganimate(p, "example_gapminder_1.gif")






## 11) Plotting probability -  ####

# load the libraries needed
library(tidyverse)
library(ggjoy)
library(scales)

# load the data
probly <- read.csv("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/perceptions-master/probly.csv", stringsAsFactors=FALSE)
numberly <- read.csv("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/perceptions-master/numberly.csv", stringsAsFactors=FALSE)


#Melt data into column format.
# --> the values are in columns per variable and will bee restructured in one column 
## with variable names and a second column with all the single values after the corresponding variable name.
numberly <- gather(numberly, # choose the data
                   "variable", # the "key": reorder table per giben variable
                   "value", # the "value": which values are to add for each variable
                   1:10) # use columns 1:10
numberly$variable <- gsub("[.]"," ",numberly$variable) # adopt the name, replace "." with " " in column numberly$variable

probly <- gather(probly, "variable", "value", 1:17) # same for the second table
probly$variable <- gsub("[.]"," ",probly$variable)
probly$value<-probly$value/100 # convert to %

#Order in the court!
probly$variable <- factor(probly$variable, # define the vairiable-column as factor (befor: character)
                          c("Chances Are Slight",
                            "Highly Unlikely",
                            "Almost No Chance",
                            "Little Chance",
                            "Probably Not",
                            "Unlikely",
                            "Improbable",
                            "We Doubt",
                            "About Even",
                            "Better Than Even",
                            "Probably",
                            "We Believe",
                            "Likely",
                            "Probable",
                            "Very Good Chance",
                            "Highly Likely",
                            "Almost Certainly"))
numberly$variable <- factor(numberly$variable, 
                            c("Hundreds of",
                              "Scores of",
                              "Dozens",
                              "Many",
                              "A lot",
                              "Several",
                              "Some",
                              "A few",
                              "A couple",
                              "Fractions of"))

#Modify Theme:
source("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/perceptions-master/ztheme.R")
z_theme() # have a look at the theme

#Plot probability data
ggplot(probly,aes(variable,value))+
  geom_boxplot(aes(fill=variable),alpha=.5)+
  geom_jitter(aes(color=variable),size=3,alpha=.2)+
  scale_y_continuous(breaks=seq(0,1,.1), labels=scales::percent)+
  guides(fill=FALSE,color=FALSE)+
  labs(title="Perceptions of Probability",
       x="Phrase",
       y="Assigned Probability",
       caption="created by /u/zonination")+
  coord_flip()+
  z_theme()
#ggsave("plot1.png", height=8, width=8, dpi=120, type="cairo-png") # directly save plot to current work directory

#Plot numberly data
ggplot(numberly,aes(variable,value))+
  geom_boxplot(aes(fill=variable),alpha=0.5)+
  geom_jitter(aes(color=variable),size=3,alpha=.2)+
  scale_y_log10(labels=trans_format("log10",math_format(10^.x)),
                breaks=10^(-2:6))+
  guides(fill=FALSE,color=FALSE)+
  labs(title="Perceptions of Probability",
       x="Phrase",
       y="Assigned Number",
       caption="created by /u/zonination")+
  coord_flip()+
  z_theme()
#ggsave("plot2.png", height=5, width=8, dpi=120, type="cairo-png")

# Joyplot for probly
ggplot(probly,aes(y=variable,x=value))+
  geom_joy(scale=4, aes(fill=variable), alpha=3/4)+
  scale_x_continuous(breaks=seq(0,1,.1), labels=scales::percent)+
  guides(fill=FALSE,color=FALSE)+
  labs(title="Perceptions of Probability",
       y="",
       x="Assigned Probability",
       caption="created by /u/zonination")+
  z_theme()
#ggsave("joy1.png", height=8, width=8, dpi=120, type="cairo-png")

#Joyplot for numberly
ggplot(numberly,aes(y=variable,x=value))+
  geom_joy(aes(fill=variable, alpha=3/4))+
  scale_x_log10(labels=trans_format("log10",math_format(10^.x)),
                breaks=10^(-2:6))+
  guides(fill=FALSE,color=FALSE)+
  labs(title="Perceptions of Probability",
       x="Assigned Number",
       y="",
       caption="created by /u/zonination")+
  z_theme()
#ggsave("joy2.png", height=5, width=8, dpi=120, type="cairo-png")


