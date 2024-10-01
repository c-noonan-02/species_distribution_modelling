# install required packages
install.packages("raster", dependencies=TRUE, repos="https://cloud.r-project.org")
library(raster)


# 1. CONCEPTUALISATION


# Q1. You will be analysing presence/absence data using a generalised linear model
# (GLM), what distribution family should you use?
# Presence/absence usually analysed with binomial models. 

# Other questions to ask yourself during the conceptualisation stage:
# What biases may our dataset have?
# Do we expect our species to be at equilibrium with climate during the survey period?
# What impact do we expect each of our climate variables to have on our species
# and why? Are these effects of climate likely to be direct or indirect via impacts
# on resources?


# 2. DATA PREPARATION


# Import the data set on swiss breeding birds
avian_data <- read.table("./data/swiss_breeding_birds.csv", header=T, sep=",")

# subset the data into just those columns that are relevant to our analysis
ouzel_cols <- c("Turdus_torquatus", "bio_5", "bio_2", "bio_14", "blockCV_tile")
ouzel_data <- data.frame(avian_data)[ouzel_cols]
summary(ouzel_data)

# import current and future climate data for Switzerland
# Download and import the same climate variables indirectly from CLIMWIN
# Next text relies on the raster package
# For first run add download=TRUE to each of the functions, you will also need
# to enter your working directory for the output_dir below
output_dir <- "C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Biodiversity Under Pressure/3. Week Three/Week Three Computer Practical 1/species_distribution_modelling_1/data"
options(timeout = 2000)
bio_current <- getData("worldclim", var="bio", res=0.5, lon=5.5, lat=45.5, path=output_dir)[[c(2,5,14)]]
