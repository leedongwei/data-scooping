rm(list=ls())

library("e1071")
library("caret")
library("Rsolnp")
library("doParallel")
library("foreach")

## Used to alert me after a long analysis is completed
library(beepr)



################################################
####    Set Directory
################################################
## TODO: Set to your own directory
setwd("C:/Users/DongWei/Documents/Projects/data-scooping")




################################################
####    Loading the data
################################################
bcw <- read.table("data/breast-cancer-wisconsin/breast-cancer-wisconsin.data", sep=",")
bcw.headers <- c("id", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "class")
bcw.features <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9")
names(bcw) <- bcw.headers


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

## Splitting the data
temp <- createDataPartition(
  bcw$class,
  times = 1,
  p = 0.6,
  list = FALSE)

bcw.trn <- bcw[temp, ]
bcw.tst <- bcw[-temp, ]

## Class "4" (malignant) is the positive set
bcw.trn.positive <- subset(bcw.trn, class=="4")
bcw.trn.negative <- subset(bcw.trn, class=="2")

## Vary % of labeled data
temp <- createDataPartition(
  bcw.trn.positive$class,
  times = 1,
  p = 0.55,
  list = FALSE)

## Set up PS and US
bcw.PS <- bcw.trn.positive[temp, ]
bcw.US <- bcw.trn.positive[-temp, ]
bcw.US <- rbind(bcw.US, bcw.trn.negative)

source("Spy_EM.R")
source("bcw-NaiveBayes.R")
beepr::beep(2)

model <- Spy_EM(rbind(bcw.PS, bcw.US), 0.10)
results <- predict(model, bcw.tst)
beepr::beep(1)

model.dw <- bcw.getNaiveBayesClassifier(bcw.PS, bcw.US)
results.dw <- predict(model.dw, bcw.tst)
beepr::beep(1)




temp <- which(bcw.tst$class == 4)
testClass <- rep(-1, nrow(bcw.tst))
testClass[temp] <- 1

results.dw.a <- rep(-1, length(results.dw))
results.dw.a[which(results.dw == 4)] <- 1

correctness <- length(which(testClass == results)) / length(testClass)
correctness.dw <- length(which(testClass == results.dw.a)) / length(testClass)

