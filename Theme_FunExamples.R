##########################
## Theme _ Fun Examples ##
##########################

library(RCurl)
library(reshape2)
library(ggplot2)
library(fortunes)
library(cowsay)
library(fun)
library(sudoku)
library(BRRR)



## 1) Analysis of Appearance Time of Students ####

# load the data (as csv-file online)
x <-  read.csv(textConnection(getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vTbXxJqjfY-voU-9UWgWsLW09z4dzWsv9c549qxvVYxYkwbZ9RhGE4wnEY89j4jzR_dZNeiWECW9LyW/pub?gid=0&single=true&output=csv")))

# check the data structure
x 
names(x)
summary(x) # -->it´s a matrix with student names as column-names and minutes of delay per course day

# possibility to put names on x-axes and time on y-axes for ggplot-ing
x2 <- melt(data=x)

names(x2) # check aes for plotting
ggplot(x2, aes(x=variable, y=value)) + # produces a boxplot (delay per person)
  geom_boxplot() +
  ylab("Delay [min]") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_text(size=12, color="black"), 
        axis.text.x=element_text(size=8, angle=90, color="black"))

# compute the cumulative sum
# store it as "cs"
# create new data.frame from cs and the students names
x.cs <- data.frame(variable = names(x), cs=t(cumsum(x)[nrow(x), ])) # nrow(x): calculate sum directly untitl the end of the x. result is then turned ("t()")
#x.cs_test <- data.frame(variable = names(x), cs=t(cumsum(x))) # if not specified: cumsum accumulates per row, but all rows "stay"
#x.cs_test2 <- data.frame(variable = names(x), cs=t(cumsum(x)[3, ])) # example: calculate sum only until row 3 of x

# add column names
names(x.cs) <- c("variable", "cumsum")

# compare: x2 with one row per entry, x.cs with one summed up row for all entries
x2 <- melt(data=x)

# merge x.cs and x2 by column "variable"
x3 <- merge(x.cs, x2, by.x ="variable", all =TRUE)

# plot the sum as color
ggplot(x3, aes(x=variable, y=value, color=cumsum)) +
  geom_point() +
  xlab("Students") +
  ylab("Delay [min]") +
  theme(axis.text.x = element_text(size=8, angle=90))

# plot boxplot + points with jitter
ggplot(x3, aes(x=variable, y=value, color=cumsum)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.7, size = 1.5, position =position_jitter(width = 0.25, height =0.5)) +
  xlab("Students") +
  ylab("Delay [min]") +
  theme(axis.text.x = element_text(size=8, angle=45, vjust=1, hjust=1))




####### automatically divide in groups, here male-female

library(gender)

x.g <- gender(names(x)) # detect gender of names with gender()
x.g # check --> see proportion of calculation which points to male/female name and final gender

colnames(x.g)[1] <- "variable" # rename the column name

x4 <- merge(x3, x.g, by.x="variable", all = TRUE) # merge the gender with the other data

# plot by sepparating ~ gender of the Students
a <- ggplot(x4, aes(x=variable, y = value, color = cumsum)) +
  geom_boxplot() +
  facet_wrap(~gender) + # class sepparation female-male
  ylab("Delay [min]") +
  xlab("Students - Gender")

# call the plot with modifications
a + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  coord_flip() + # exchange the axes/coordinates


# plot boxplot with jitter
b <- ggplot(x4, aes(x=variable, y = value, color = cumsum)) +
  geom_boxplot() +
  geom_point(aes(color=cumsum), #colors shell depend on "height"
             alpha=0.6, # transparency of points
             size=1.5, # size of points
             position=position_jitter(width=0.25, height=0)) + #size of jitter-effect/straightening out the points
  facet_wrap(~gender) +
  ylab("Delay [min]") +
  xlab("Students - Gender") +
  theme(axis.text.x = element_blank())
b


## 2) Library Fortunes - Calling for quotes ####

fortune() # call a quote from programmers
fortune("memory") # call for quote topics
fortune("Spatial") 

## 3) Library Cowsay - drawing animals with returned text ####

say("Hello World!") # print cat which says command

# create a function
someone_say_hello <- function(){
  animal <- sample(names(animals), 1) # choose a random animal from the list of animals
  say(paste("Hello, I´m a", animal, ".", collapse=""), by=animal)
}
someone_say_hello()


someone_say_my_fortune <- function(x){
  animal <- sample(names(animals), 1)
  say(paste(fortune(), collapse="\n"), by=animal)
}
someone_say_my_fortune()


## 4) Library fun - Playing Games - chess, sudoku, etc. ####

# choose which type of window shell be opened for playing the game (depending on system)
if(.Platform$OS.type =="windows") x11() else x11(type="Xlib")
mine_sweeper()


## 5) Library Sudoku- Sudoku ####
if(.Platform$OS.type =="windows") x11() else x11(type="Xlib")
playSudoku()


## 6) package BRRR - making sounds ####

# install package devtools if it´s not already loaded
if(!require(devtools)) {install.packages(devtools)}

# download package BRRR from github
devtools::install_github("brooke-watson/BRRR")

# try some noises
skrrrahh("drummaboy")
skrrrahh("snoop")
skrrrahh(41)
skrrrahh(expr = "yes!")

# check if code works or get sound feedback
x <- 11
if (sqrt(x) == sqrt(10)){
  skrrrahh("snoop")
} else {
  skrrrahh(41)
}

## check details on: https://github.com/brooke-watson/BRRR



## 7) Drawing a Christmas Tree #####
cat(c("\u4D\u65\u72\u72\u79\u20\u43\u68\u72\u69\u73\u74\u6D\u61\u73\u0A\u74\u6F\u20\u61\u6c\u6c\u20\u45\u41\u47\u4C\u45\u20\u73\u74\u75\u64\u65\u6E\u74\u73\u21\u0A\u0A",unlist(lapply(c(1:17*2-1,rep(3,6)),function(x)
cat(rep("\u20",(37-x)/2),".",rep("\u23", x), ".\n", sep="")))))




## 6) package praise - praises the coder
devtools::install_github("gaborcsardi/praise")
library(praise)
praise() # print only adjectives
praise("${EXCLAMATION}! You have done this ${adverb_manner}!") # print exlamation and adjective
# see further examples on: https://github.com/rladies/praise

## 8) Sunset and rise ####
install.packages("suncalc")
install.packages("V8")
library(suncalc)
library(V8)
getSunlightTimes(date = Sys.Date(), lat = 50.1, lon = 1.83, tz = "CET")

## 9) drawing in Candinsky-style ####

#(!!! R version is to old, version 3.3.3 is required)

devtools::install_github("gsimchoni/kandinsky")
library(kandinsky)

## 10) Displaying networks ####

library(network)
library(sna)
library(maps)
library(ggplot2)
library(GGally)



#(!!! the following is the example code from: https://ggobi.github.io/ggally/#example_us_airports)



airports <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/airports.csv", header = TRUE)
rownames(airports) <- airports$iata

# select some random flights
set.seed(1234)
flights <- data.frame(
  origin = sample(airports[200:400, ]$iata, 200, replace = TRUE),
  destination = sample(airports[200:400, ]$iata, 200, replace = TRUE)
)

# convert to network
flights <- network(flights, directed = TRUE)

# add geographic coordinates
flights %v% "lat" <- airports[ network.vertex.names(flights), "lat" ]
flights %v% "lon" <- airports[ network.vertex.names(flights), "long" ]

# drop isolated airports
delete.vertices(flights, which(degree(flights) < 2))

# compute degree centrality
flights %v% "degree" <- degree(flights, gmode = "digraph")

# add random groups
flights %v% "mygroup" <- sample(letters[1:4], network.size(flights), replace = TRUE)

delete.vertices(flights, which(flights %v% "lon" < min(usa$data$long)))
delete.vertices(flights, which(flights %v% "lon" > max(usa$data$long)))
delete.vertices(flights, which(flights %v% "lat" < min(usa$data$lat)))
delete.vertices(flights, which(flights %v% "lat" > max(usa$data$lat)))

# create a map of the USA
usa <- ggplot(map_data("usa"), aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), color = "grey65",
               fill = "#f9f9f9", size = 0.2)

# overlay network data to map
p <- ggnetworkmap(
  usa, flights, size = 4, great.circles = TRUE,
  node.group = mygroup, segment.color = "steelblue",
  ring.group = degree, weight = degree)
p


## 11) Colorblindr ####

# instell packages needed
devtools::install_github("wilkelab/cowplot")
install.packages("colorspace", repos = "http://R-Forge.R-project.org")
library(colorspace)
library(cowplot)

devtools::install_github("clauswilke/colorblindr")

# plot the first plot
library(ggplot2)
fig <- ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_density(alpha = 0.7)
fig

# vary the plot using differen color shemes
library(colorblindr)
cvd_grid(fig)

# suitable plot for people with color-vision problems
fig2 <- ggplot(iris, aes(Sepal.Length, fill = Species)) + 
  geom_density(alpha = 0.7) + scale_fill_OkabeIto()
fig2
cvd_grid(fig2) # same grid, but better for people with color-vision problems


