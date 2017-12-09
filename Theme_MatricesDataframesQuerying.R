###################################
## Theme _ Matrices, Dataframes, Lists and querying ##
###################################

setwd("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_theme_scripts")


## 1) Creating Matrices ####

## create a simple matrix
x <- matrix(c(4,7,3,8,9,2), nrow = 2) # matrix with two rows
x

# query values from the matrix
x[2,2] # value row2, column 2
x[ , 2] # values all rows, column2


## 2) Transfroming Matrix to Dataframe and querying values ####

# create a bigger matrix and transform the matrix to a data frame
numbers_1 <- rnorm(80, mean = 0, sd = 1) # creates vector with n = 80 observations, these with mean = 0, sd = 1 and normal distributed.
mat_1 <- matrix(numbers_1, nrow =20, ncol = 4) # define the extents for the matrix and create it with values from numbers_1
mat_1
df_1 = data.frame(mat_1)

names(df_1) <- c("var1", "var2", "var3", "var4") # add header / column names to the data frame
rownames(df_1) = c(1:20) # add rownames to the data frame, here numbers 1:20.

head(df_1) # show the beginning off the data frame.
df_1[, 1] # all rows, column 1
df_1$var1[1:3] # variable1, row 1:3
df_1[1:2, 1:2] # rows 1 and 2, column 1 and 2
df_1[df_1 <= 0] # all values <= 0 (actual values)
df_1[df_1 >= 0 & df_1 <= 1] # same with 2 conditions
df_1 <= 0 # all values that fit the condition (TRUE or FALSE)
(df_1 <= 0 | df_1 <= -1) # same with 2 conditions



## 3) Querying vectors and setting values ####

# create a vector and query single values/groups of values.
x2 <- seq(1, 100, by =2.5)
x2
x2[1:10]
x2[20:30]
x2[length(x2)] # last value
x2[length(x2)-1] # second last value
x2[-1] # all but first position
position <- c(1,5,10) # create a vector with intersting positions
x2[position] # query the entries which fit to the condition of "position".
x2[-position] # query entries which don´t fit to the condition of "position".
x2 > 10
x2[x2 <= 10 | x2 >= 15] # gives actual data that fits to the condition
(x2 <= 10) | (x2 >= 15) # gives the positions that fit to the condition


x2[(x2 > 30)&(x2 < 70)] <- 2 # sets values >30 and <70 to 2

# faster possibility to change ceveral values at the same time
library(car)
x2 <- recode(x2, "0:20=1; 21:50=2; else=3") # changes severeal entries (following the condition, not the entry-number) at the same time.

#x2 <- numeric(length(x2)) # numeric: creates double precision vector of the given length, here with 40 entries.
#x2[x2 <= 30] <- 1 # set values smaller 30 to 1


# get information about the dataset.
summary(x2)
sum(x2)
cumsum(x2) # cumulativ sum from one entry to the next.

rev(x2) # reverts the order
sort(x2, decreasing = FALSE) # sorts the values
sample(x2, 10) # takes samples from x2, here 10 times 


## 4) Dataframes with multiple columns and adding columns #### 

test <- data.frame(A = c(1,2,3), B=c("aB1", "aB2", "aB3")) # create a data frame with rows and columns.
test

test[, "A"] # queriing as before.
test$A # equal to command before.


# create data-frame with multiple variables and values.
df <- data.frame(plot = "location_name_1", measure1 = runif(100) * 1000, measure2 = round(runif(100) *100), 
                 value = rnorm(100, 2, 1), ID = rep(LETTERS, 100))
# runif creates random numbers (by default from [0,1]). Here n = 100 times.
df_2 <- data.frame(plot="location_name_2", measure1 = runif(50)*100, measure2 = round(runif(50)*10),
                   value = rnorm(50), ID = rep(LETTERS, 50))
df_3 = rbind(df, df_2) # connect the two data frames.


df_3[100:115, c("plot", "measure1", "measure2")] # rows 100:115, columns as specified.
df_3[df_3$value > 3.0, ]
df_3[df_3$value <=3 & df_3$value >= 2, ]

df_3$new_col <- df_3$measure1*df_3$measure2 # add a new column to the data frame (here multiplication from existing columns)



## 5) Creating and querying list-objects ####

a <- runif(199)
b <- c("aa", "bb", "cc", "dd", "ee")

c <- list(a,b)
c 
# --> objects are not joined, they are just together
# a lot of objects (also differnt classes) can be stored together

# query data
c[2] # the second object
c[[2]] # same

c[[2]][1] # the first entry of second object

a <- list(obj_1 = runif(100), obj_2 = c("aa", "bb"), obj_3 = c(1,2,4))
a$obj_1
a["obj_1"] # equal to command before
a[[1]] # equal to command before

a <- list(m1 = matrix(runif(50), nrow =5), v1 =c(1,6,10), df1 = data.frame(a = runif(100), b =rnorm(100)))
a$df1[ ,1] # query the first column from object df1 from the list a.
