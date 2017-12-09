###############################
## Theme _ ggplot - Graphics ##
###############################

setwd("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_theme_scripts")

library(ggplot2)

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


## 3) ggboxplot - example 1 #####

# example data set: mpg
ggplot(mpg, aes(x = class, y = cty)) + # define x and y axis
  geom_boxplot(alpha = .5) + # adopt transparency of the boxes
  geom_point(aes(color = hwy), # add points for single data points
             alpha = 0.7, size = 1.5, # adopt transparency and size of the points
             position = position_jitter(width = 0.25, height = 0)) # loosen the points if several on same position
# (!!! jitter can change meaning of plot! not too much!)

## 4) ggboxplots - example 2 ####

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

## 5) Scatterplots - example 1 ####

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





