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
occurrence_data1 <- geodata::sp_occurrence("Tapirus", "terrestris*", geo=FALSE,removeZeros=TRUE,start=1,end=10000)
# star - all naming variants
# geo = TRUE - select only records with lattitude and lingitude recorded

# need to site the DOI - keep the datasetkey variable in your data set

# check the data frame has as many rows as there are occurrences
dim(occurrence_data1)

# view data
occurrence_data1[1:10,]

# we can now plot the global distribution to make sure it fits our expectations
global_map <- world(path = ".") # gives outline of world's political boundaries
# create a plot using this
plot(global_map, xlim=c(-90,-20), ylim=c(-60,20), col="light yellow", border="grey")
# add the points
points(occurrence_data1$lon, occurrence_data1$lat, col="light blue", pch=20)


# 2. CLEANING UP OCCURRENCE DATA

# It is important to scrutinize the GBIF data first

# cut any data from outside of the their range 
# do not need to do that here, but will save code for future reference
# occurrence_data1<-subset(occurrence_data1,lat>0)
# removes observations from the southern hemisphere

# look for duplicate records and remove them
duplicates <- duplicated(occurrence_data1[,c("lon", "lat")])
# sum the total number of duplicates
sum(duplicates)
# there are a lot, so let's remove them!
occurrence_data2 <- occurrence_data1[!duplicates,]

# Questions we should ask ourselves about the occurrence data
# Does the distribution of data look sensible for our selected species? - Yes
# Do we see evidence of spatial biases in sampling? - potentially some bias towards water sources and capitals?

# 3. DOWNLOADING WORLDCLIM DATA
output_dir<-"./data/worldclim_data"
bio_glob<-worldclim_global(var="bio", res=10,path=output_dir, version="2.1")
# check dimensions
dim(bio_glob)
# bio_glob is in the form of a spatraster (spatial raster). Raster = spatial grid. Spratraster combines several grids.

# get the spatial spread of the data we are using
summary(occurrence_data2$lat)
summary(occurrence_data2$lon)

extent <- ext(-90, -30, -60, 20)
predictors <- crop(bio_glob, extent)
# shorten the names of predictors by taking the 11th and 16th characters...
names(predictors) <- substring(names(predictors), 11,16)

# can now look at the global climate data - first 9 climate variables
plot(predictors,1:9)

# then we can add our species data to the plot
plot(predictors,1)
points(occurrence_data$lon, occurrence_data$lat, col="maroon", pch=16, cex=0.2)


# 4. GENERATING BACKGROUND DATA

# here we only have presence data - no absence data
# one approach is to sample background data from a region, covering the regions where the species is present and absent

# set the spatial extent to be broadly consistent with that of the study species
?spatSample
background <- spatSample(predictors, 5000, "random", na.rm=TRUE, as.points=TRUE, ext=extent)

# plot our points on a map of climwin variable 1 (can do for any of the variables)
plot(predictors, 1)
points(background, cex=0.1, col="black")



