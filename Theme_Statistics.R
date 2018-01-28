######################
## Theme_Statistics ##
######################

setwd("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_theme_scripts")
library(raster)
library(RStoolbox)
library(ggplot2)
library(car)
library(corrplot)
library(usdm)



## 1) creation of Correlation data and check for correlation ####

# no correlation
x <- runif(100)
y <- runif(100)
plot(x, y)
cor(x,y) # only gives the ratio of correlation 
cor.test(x, y) # also gives the p-value and degrees of freedom for the correlation


# positive correlation 
x <- sort(runif(100))
y <- sort(runif(100))
plot(x,y)
cor(x,y)
cor.test(x,y)

# positive correlation 2
x <- (runif(100))
y <- x +(runif(100)*0.5)
plot(x,y)
cor(x,y)
cor.test(x,y)

# negative correlation
x <- (runif(100))
y <- -x +(runif(100)*0.5)
plot(x,y)
cor(x,y)
cor.test(x,y)



## 2) Interactive plot to change Correlation Values ####
library(TeachingDemos)
library(tkrplot)

if(interactive()){
                  run.cor2.examp()
                 }


if(interactive()){
  put.points.demo()
  
  x <- rnorm(25, 5, 1)
  y <- x + rnorm(25)
  put.points.demo(x,y)
}




## 3) Different Correlations - r is always identical ####
source("http://janhove.github.io/RCode/plot_r.R") # download the code for function plot_r()

plot_r(r=0.5, n=50) # plot self defined data
plot_r(r=0, n=50)

plot_r(r=0.4, n=10, showdata=11) # possibility to store the data
plot_r(r=0.8, n=15, showdata="all")


plot_r # look at the actual code



## 4) plotting correlation ####

######## 1. plotcorr()
lsat <- brick("raster/lsat.tif")

cm_lsat <- cor(getValues(lsat), use="complete.obs")

library(ellipse)
plotcorr(cm_lsat, col=ifelse(abs(cm_lsat) > 0.7, "red", "grey"))
# the larger the circles, the higher is the correlation

## --> can be used, if non-correlated datasets are needed --> check for example, which layers don?t correlate for later use in analysis



######## 2. corrplot()
library(corrplot)

#
data(mtcars)
M <- cor(mtcars)
set.seed(0)
corrplot(M, method = "number", col = "black", cl.pos = "n")





## 5) Moving Window

# load the data
lsat <- brick("raster/lsat.tif")

# calculate the correlation bewtween band 2 and 3 of these data
lsat_band_2_3 <- corLocal(lsat[[2]], lsat[[3]], ngb=11)
plot(lsat_band_2_3) # --> positive values: the higher value on band 2, the higher also value on band 3




## 6) causation and false correlations ####
library(TeachingDemos)
data(stork)

plot(stork)
summary(stork)

cor(stork$No.storks, stork$No.babies)
cor.test(stork$No.storks, stork$No.babies) 
# --> indirect/second order correlation, BUT: not direct link between these datasets, although correlated
# --> check for causation/misleading correlation("Scheinkorrelation")


## 7) basic statistics ####

a <- c(5,2,6,59,84,55,6,99,55,77,2,3,5,6,8,15,11,12,46,85,40, 1, 1, 1, 1, 60,60,60,60,60)
b <- c(1:length(a))

# variance = mittlerer quadratischer Abstand zum Mittelwert
# standard deviation = sqrt(variance)
# standard fehler = standard deviation/sqrt(sampling size)

mean(a) # average of numbers --> BUT: no information about how well the mean represents the data!!
median(a) # middle number
var(a)
sd(a) # == sqrt(var)

plot(b,a)
lines(x=c(0, length(b)+1), y=c(mean(a), mean(a)), col="red")
## --> data pattern can be changed without changing the mean!



## 8) Spatial correlation with corLocal() ####
# load image and calculate ndvi and  evi
p224r63 <- brick("raster/p224r63.tif")
p224r63_ndvi <- spectralIndices(p224r63, red = "p224r63.3", nir = "p224r63.4", indices = "NDVI")
p224r63_evi <- spectralIndices(p224r63, red="p224r63.3", blue = "p224r63.1", nir = "p224r63.4", indices = "EVI")

p224r63_ndvi_evi <- corLocal(p224r63_ndvi, p224r63_evi, ngb=11)

plot(p224r63_ndvi_evi)

ggR(p224r63_ndvi_evi, geom_raster=TRUE) +
  ggtitle("Spatial Correlation NDVI and EVI (p224r63-image") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold"), 
        legend.title=element_text(size=12, colour="black", face="bold")) +
  scale_fill_gradientn(colors=terrain.colors(100),
                       na.value="white",
                       name="Correlation\n")

## 9) Shapiro test  (normal distribution) and Levene Test (equallity of variance) ####

# create data: normal and non-normal distributed
set.seed(120)
d1 <- rnorm(100)
d2 <- rt(100, df=3)

# first possibility to visually check data distribution
plot(density(d2), col="blue", lwd=2)
lines(density(d1), col="green", lwd=2)

shapiro.test(d1) # --> norml distributed
shapiro.test(d2) # --> not normal distributed

# second possibility to visually check data distribution
par(mfrow=c(1,2))
qqnorm(d1);qqline(d1, col=2)
qqnorm(d2);qqline(d2, col=2)

## cor() command: requires normal distribution!!
## nice to understand test: create data with certain distribution, and test this data to see result of test


# test for equally distributed data
D <- data.frame(GroupID=as.factor(c(rep(1, length(d1)), rep(2, length(d2)))),
                data=c(d1, d2))

leveneTest(data ~ GroupID, data=D)


## 10) Colinearity and mulitcolinearity - corrplot caret and vif(?!!!!)####

########## 1. library corrplot

# create data which correlate
a <- runif(200)
b <- a * runif(200)
c <- a*b
d <- runif(200) * 2
data <- data.frame(cbind(a, b, c, d))
colnames(data) <- cbind("a", "b", "c", "d")

# plot correlation per pair in circles
corrplot::corrplot(cor(data), type="lower")

# plot correlations per pair in numbers
corrplot::corrplot(cor(data), method="number")





########## 2. library caret

# create correlation data
R1 <- structure(c(1, 0.86, 0.56, 0.32, 0.85, 0.86, 1, 0.01, 0.74, 0.32, 
                  0.56, 0.01, 1, 0.65, 0.91, 0.32, 0.74, 0.65, 1, 0.36,
                  0.85, 0.32, 0.91, 0.36, 1), 
                .Dim = c(5L, 5L))
colnames(R1) <- rownames(R1) <- paste0("x", 1:ncol(R1))

caret::findCorrelation(cor(R1), cutoff=0.5, names=TRUE, verbose=TRUE)






########## 3. library usdm
lsat <- brick("raster/lsat.tif")
vif(lsat)






## 11) 

