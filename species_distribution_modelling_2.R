rm(list=ls())

# install packages
install.packages("geodata",dependencies=TRUE,repos="https://cloud.r-project.org")
install.packages("predicts",dependencies=TRUE,repos="https://cloud.r-project.org")
install.packages("terra",dependencies=TRUE,repos="https://cloud.r-project.org")

# load packages
library(geodata)
library(predicts)
library(terra)

# choose species - chose Tapirus terrestris
occurrence_data <- geodata::sp_occurrence("Tapirus", "terrestris*", geo=FALSE,removeZeros=TRUE,start=1,end=10000)
# star - all naming variants
# geo = TRUE - select only records with lattitude and lingitude recorded

# need to site the DOI - keep the datasetkey variable in your data set

# check the data frame has as many rows as there are occurences
dim(occurrence_data)

# view data
occurrence_data[1:10,]

# we can now plot the global distribution to make sure it fits our expectations
global_map <- world(path = ".") # gives outline of world's polotical boundaries
# create a plot using this
plot(global_map, xlim=c(-90,-20), ylim=c(-60,20), col="light yellow", border="grey")
# add the points
points(occurrence_data$lon, occurrence_data$lat, col="light blue", pch=20)
