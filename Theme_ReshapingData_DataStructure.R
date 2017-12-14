###############################################
## Theme _ Reshaping Data and Data Structure ##
###############################################

library(reshape2)


## 1) Example 1 - potential field data ####

## create (potential) field data by hand
fielddata_wide <- read.table(header = TRUE, text ='
                             plot_id name Cover LAI DBH
                             1 Sophie 7.9 12.3 10.7
                             2 Achmed 6.3 10.6 11.1
                             3 Achmed 9.5 13.1 13.8
                             4 Sophia 11.5 13.4 12.9
                             ')
# look at the data
fielddata_wide

# make sure that plot_id is a factor 
class(fielddata_wide$plot_id) # (at the moment it´s integer)
fielddata_wide$plot_id <- factor(fielddata_wide$plot_id) 

# change data structure --> see ?melt.data.frame
melt(fielddata_wide, # data frame to melt
     id.vars = c("plot_id", "name")) # variables to preserve --> values per id_plot and names are constructed

# kind of same process, but directly store columnames within new dataframe
fielddata_long <- melt(fielddata_wide, 
                       id.vars =c("plot_id", "name"), #  vector of variables to preserve but not to split apart on
                       measure.vars = c("Cover", "LAI", "DBH"), # vector of measured variables/ source columns
                       variable.name = "method",   # (column-) name of variable used to store measured variable/destination column
                       value.name = "measurement") # name of variabel to store values/destination column
fielddata_long # check the data

## if measured values are needed in columns of methods:
data_wide <- dcast(fielddata_long, plot_id + name ~ method, value.var = "measurement")
data_wide
