# install.packages(
#   c("tm", "tm.plugin.mail",
#     "e1071", "SnowballC", "caret", "ineq",
#     "parallel", "doParallel", "foreach", "snow",
#     "beepr"))

rm(list=ls())

## Essential libraries
library(tm)
library(tm.plugin.mail)
library(e1071)
library(SnowballC)
library(caret)
library(ineq)

## Parallel libraries
library(parallel)
library(doParallel)
library(foreach)
library(snow)

## For sound alerts
library(beepr)



setwd("C:/Users/DongWei/Documents/Projects/data-scooping/")
# setwd("C:/Users/DongWei/Desktop/data-scooping")



source("newsgroup-utils-corpus.R")
source("newsgroup-utils-dtm.R")
source("newsgroup-utils-perf.R")
source("newsgroup-utils-functions.R")

source("newsgroup-SpyEM.R")
source("newsgroup-Rocchio.R")
source("newsgroup-RocchioSVM.R")
source("newsgroup-KMeans.R")









##############################################
####    Start of code
##############################################
# file.remove("newsgroup-output.txt")

bcw <- read.table("data/breast-cancer-wisconsin/breast-cancer-wisconsin.data", sep=",")
bcw.headers <- c("id", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "class")
bcw.features <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9")
names(bcw) <- bcw.headers


## bcw has non-unique patient IDs
## We will replace all IDs to ensure uniqueness
rownames(bcw) <- paste("D", 1:length(bcw$id), sep="")

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


ngp.PS <- bcw[bcw$class == 4, ]
ngp.NS <- bcw[bcw$class == 2, ]
ngp.PS$class <- NULL
ngp.PS$id <- NULL
ngp.NS$class <- NULL
ngp.NS$id <- NULL


ngp.class <- bcw[ ,c("id", "class")]
ngp.class[ngp.class$class == 4, ]$class <- 1
ngp.class[ngp.class$class == 2, ]$class <- -1
ngp.class$class <- as.factor(ngp.class$class)

cat("Read data completed, starting loop\n")





##############################################
####    Function for 1 sampling iteration
####    This will run on the parallel loop below
##############################################
trnLabeled <- c(0.10, 0.20, 0.40, 0.65)
var.i <- 1
var.j <- 1

## Moved to separate file
source("newsgroup-mainfunction.R")





##############################################
####    Begin loops to sample data
##############################################
## Percentage of training data to be set as labeled
# TODO: Set correct values after code is stable
trnLabeled <- c(0.10, 0.15, 0.25, 0.35, 0.50, 0.65)
# trnLabeled <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50, 0.65)

## Set reps in multiples of cores for maximize processing time
## If you have 3 cores, repSamples should be 3/6/9/12
repSamples <- 6

namesOfClassifiers <- c("AllPos", 
                        "nBayes", "SVM", "Rocchio", 
                        "Spy-EM", "RocSVM", "KMeans")
numberOfClassifiers <- length(namesOfClassifiers)





## Build matrix to store overall results
ngp.fmeasure.results <- matrix(rep(0, numberOfClassifiers * length(trnLabeled)),
                               length(trnLabeled))
rownames(ngp.fmeasure.results) <- trnLabeled
colnames(ngp.fmeasure.results) <- namesOfClassifiers
ngp.accuracy.results <- ngp.fmeasure.results

## Vary % of data that is labeled data
for (var.i in 1:length(trnLabeled)) {
  
  print(paste("---------- TrnPercent:", trnLabeled[var.i], "----------"))

  ## Build a row of of the results, for ngp.sampling to return
  ngp.sampling.output.row <- matrix(rep(0, numberOfClassifiers*2), nrow=2)
	colnames(ngp.sampling.output.row) <- namesOfClassifiers
	rownames(ngp.sampling.output.row) <- c("fmeasure", "accuracy")

	## Set up cluster for parallel
	parallel.cluster <- utils.createParallelCluster()
	registerDoParallel(parallel.cluster)

	##############################################
  #### Repeat sampling 9 times to avoid sampling bias
  ngp.sampling.results <- foreach(var.j = 1:repSamples,
          .packages = c("tm", "e1071", "SnowballC", "caret", "ineq")) %dopar% ngp.sampling(
            ngp.PS, ngp.NS, ngp.class, ngp.sampling.output.row, trnLabeled, var.i)
  ##############################################
  print(ngp.sampling.results)

	## Reset cluster for parallel. Prevents crashing.
	stopCluster(parallel.cluster)


  ## Pull results from repeated sampling
  ngp.sampling.fmeasure <- matrix(rep(0, numberOfClassifiers), nrow=1)
  colnames(ngp.sampling.fmeasure) <- namesOfClassifiers
  ngp.sampling.accuracy <- ngp.sampling.fmeasure

  for (sample in ngp.sampling.results) {
    stopifnot(!(-1 %in% sample | NaN %in% sample))
    ngp.sampling.fmeasure <- ngp.sampling.fmeasure + sample["fmeasure", ]
    ngp.sampling.accuracy <- ngp.sampling.accuracy + sample["accuracy", ]
  }

  ## Calculate mean
  ngp.fmeasure.results[var.i, ] <- ngp.sampling.fmeasure / repSamples
  ngp.accuracy.results[var.i, ] <- ngp.sampling.accuracy / repSamples

  beepr::beep(1)
}

timer.loop <- proc.time() - timer.readNewsgroup
beepr::beep(8)


## Save workspace, just in case
save.image("newsgroup-results.RData")


## PLOT FOR F-MEASURE
utils.plotGraph(ngp.fmeasure.results, "F-Measure")
utils.plotGraph(ngp.accuracy.results, "Accuracy")
timer.total <- proc.time() - timer.start

utils.plotGraph(ngp.fmeasure.results[,c("Rocchio", "KMeans")], "F-Measure") 
utils.plotGraph(ngp.accuracy.results[,c("Rocchio", "KMeans")], "Accuracy") 
