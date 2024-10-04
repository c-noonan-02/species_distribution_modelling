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
points(occurrence_data2$lon, occurrence_data2$lat, col="maroon", pch=16, cex=0.2)


# 4. GENERATING BACKGROUND DATA

# here we only have presence data - no absence data
# one approach is to sample background data from a region, covering the regions where the species is present and absent

# set the spatial extent to be broadly consistent with that of the study species
?spatSample
background <- spatSample(predictors, 5000, "random", na.rm=TRUE, as.points=TRUE, ext=extent)

# plot our points on a map of climwin variable 1 (can do for any of the variables)
plot(predictors, 1)
points(background, cex=0.1, col="black")


# MATCHING OCCURRENCE AND CLIMATE DATA

# match the climate and occurrence data
# extract the longitude and lattitudes from the data set
occurrence_latlon <- cbind(occurrence_data2$lon, occurrence_data2$lat)

# save climate data for where the species is present
present_climate <- extract(predictors, occurrence_latlon)
# save climate data for the background data
present_back <- values(background)
# extracts the geometry of a Spatvector
background_lonlat <- geom(background)
# combine all climate data. In background data x and y correspond to long and lat
coordinates <- rbind(occurrence_latlon, background_lonlat[,c("x", "y")])
# first column of dataset is a vector of 1s for presences and 0s for absences
pb <- c(rep(1, nrow(present_climate)), rep(0, nrow(present_back)))
# combine presence and background data into single dataframe
summary_data <- data.frame(cbind(coordinates, pb, rbind(present_climate, present_back)))

# we can examine how correlated the predictor variables are. Highly correlated = statistical issues
pairs(summary_data[,4:7], cex=0.1)
# could extend this to other predictor variables


# 6. FITTING A SPECIES DISTRIBUTION MODEL

# Removing rows where climate data are NAs
summary_data <- subset(summary_data, is.na(bio_1)==F)

spec_data <- as.data.frame(cbind(rep("Tapirus terrestris",length(summary_data[,1])), summary_data))
names(spec_data)[1:4]<-c("species","longitude","latitude","presabs")
spec_data<-subset(spec_data,presabs==1)
backdata<-as.data.frame(cbind(rep("background",length(summary_data[,1])), summary_data))
names(backdata)[1:4]<-c("","longitude","latitude","presabs")
backdata<-subset(backdata,presabs==0)

# save data
output_dir<-"./data"
write.table(spec_data[,-4],paste(output_dir,"/Tairus_terrestris.csv",sep=""),col.names=T,row.names=F,sep=",")
write.table(backdata[,-4],paste(output_dir,"/background.csv",sep=""),col.names=T,row.names=F,sep=",")

# build a model, ignoring any data that comes from the same cell
tapir_model <-MaxEnt(summary_data[,-c(1:3)],summary_data[,3],removeDuplicates=TRUE)

plot(tapir_model)
# most important variable for tapir is bio_19 (precipitation of coldest quarter), followed by bio_13 (precipitation of wettest month) and bio_4 (temperature seasonality)

# can look at predicted climate suitability globally and see how it matches where the species has been recorded
# remember with occurrence data no data does not equal absence
predicted_occurrence <- predict(tapir_model, predictors, args=c("outputformat=raw")) 
# plot this data
par(mfrow=c(2,1))
plot(predicted_occurrence) # plot climate data
plot(predicted_occurrence) # plot occurrences on duplicate
points(coordinates,pch=".", col="black")
par(mfrow=c(1,1))


# 7. PREDICTING FUTURE DISTRIBUTION

# we need another element in order to predict future suitability - download future climate data
?cmip6_world
output_dir<-"./data/future_clim"


future_clim <- cmip6_world(model = "ACCESS-ESM1-5", ssp = "245", time = "2041-2060", var = "bioc", res = 10, path = output_dir)
future_predictors <- crop(future_clim, extent)

# compare baseline to future climates for any of our variables
par(mfrow=c(2,1))
plot(predictors,4)
plot(future_predictors,4)
par(mfrow=c(1,1))
# 19 shows changes
# 13 shows less change
# 4 shows some change


# generate a prediction of future climate suitability for tapirs

# make header names correspond
names(future_predictors)<-names(predictors)

# create future predictions using the model of tapirs, and the future climate data
future_predictions <- predict(tapir_model, future_predictors, args=c("outputformat=raw")) 

par(mfrow=c(2,1))
plot(predicted_occurrence,main="current")
plot(future_predictions,main="2050")
par(mfrow=c(1,1))
# decline of suitable areas into a small region

# check model AUC etc
tapir_model
