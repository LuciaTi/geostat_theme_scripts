####################################################
## Theme_Coding Best Practice / Building Packages ##
####################################################

setwd("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_theme_scripts")
# use: source() to load singel functions
# use: library() to load a full package with functions


## 1 Reprucibility of codes ####
sample(LETTERS, 5) # --> run tow times: not the same result!

set.seed(10)
sample(LETTERS, 5) # --> same sample numbers if the two codes are run together!

