##########################
## Theme _ Fun Examples ##
##########################

library(RCurl)
library(reshape2)
library(ggplot2)
library(fortunes)
library(cowsay)


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