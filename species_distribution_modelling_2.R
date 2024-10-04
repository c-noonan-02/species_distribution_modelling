rm(list=ls())

# install packages
install.packages("geodata",dependencies=TRUE,repos="https://cloud.r-project.org")
install.packages("predicts",dependencies=TRUE,repos="https://cloud.r-project.org")
install.packages("terra",dependencies=TRUE,repos="https://cloud.r-project.org")

# load packages
library(geodata)
library(predicts)
library(terra)