rm(list=ls())

library("e1071")
library("caret")
library("Rsolnp")
library("doParallel")
library("foreach")
library(ROCR)

## Used to alert me after a long analysis is completed
library(beepr)



################################################
####    Set Directory
################################################
## TODO: Set to your own directory
# setwd("C:/Users/DongWei/Documents/Projects/data-scooping")

################################################
####    Load code for Algorithms
################################################


##    Spy-EM
source("Spy_EM.R")

################################################
####    Loading the data
################################################
bcw <- read.table("data/breast-cancer-wisconsin/breast-cancer-wisconsin.data", sep=",")
names(bcw) <- c('id', paste0('V',1:9), 'class')
bcw.features <- names(bcw)[2:10]


## bcw has non-unique patient IDs
## We will replace all IDs to ensure uniqueness
bcw$id <- 1:length(bcw$id)


## bcw$V6 has missing values
## We will replace them with the mean of V6
index <- which(bcw$V6 %in% "?")

## Convert factor to numeric to find mean
bcw$V6 <- as.numeric(levels(bcw$V6))[bcw$V6]
V6.mean <- floor(sum(bcw$V6, na.rm=TRUE) / length(bcw$V6))

## Replace missing V6 values with mean
bcw$V6[index] <- V6.mean
bcw$V6 <- as.integer(bcw$V6)
rm(V6.mean)


################################################
################################################
################################################
####    Start testing here

trnPercent <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.45, 0.55, 0.65, 0.75)
spy <- 0.15
perf.EM <- do.call('rbind', lapply(trnPercent, perfSpy_EM, data= bcw, Spy.rate=spy))

