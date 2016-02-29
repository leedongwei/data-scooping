# install.packages(
#   c("tm", "tm.plugin.mail",
#     "e1071", "SnowballC", "caret"
#     "parallel", "doParallel", "foreach","snow",
#     "beepr"))

rm(list=ls())

## Essential libraries
library(tm)
library(tm.plugin.mail)
library(e1071)
library(SnowballC)
library(caret)

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









##############################################
####    Start of code
##############################################
# file.remove("newsgroup-output.txt")
timer.start <- proc.time()
parallel.cluster <- utils.createParallelCluster()

dir.all <- c("alt.atheism",              "comp.graphics",
             "comp.os.ms-windows.misc",  "comp.sys.ibm.pc.hardware",
             "comp.sys.mac.hardware",    "comp.windows.x",
             "misc.forsale",             "rec.autos",
             "rec.motorcycles",          "rec.sport.baseball",
             "rec.sport.hockey",         "sci.crypt",
             "sci.electronics",          "sci.med",
             "sci.space",                "soc.religion.christian",
             "talk.politics.guns",       "talk.politics.mideast",
             "talk.politics.misc",       "talk.religion.misc")

## TODO: Select directories to use
dir.selected <- dir.all

## TODO: Set positive classes
dir.positive <- c( "comp.graphics",
                   "comp.os.ms-windows.misc",  "comp.sys.ibm.pc.hardware",
                   "comp.sys.mac.hardware",    "comp.windows.x")


## Read to corpus, convert into DTM and clean it
## TODO: Set data folder in "newsgroup-utils-corpus.R/utils-prepCorpus"
# Non-parallel code: ngp.data <- utils.prepCorpora(dir.selected)
ngp.data <- utils.prepCorpora.parallel(parallel.cluster, dir.selected)
ngp.dtm  <- utils.createDtmFromCorpora(ngp.data, 35)

## Parallel initiated in utils.prepCorpora
stopCluster(parallel.cluster)

## Reset document ID and extract class names
ngp.class <- utils.extractDtmClasses(ngp.dtm)
ngp.dtm <- utils.resetDtmDocNames(ngp.dtm)

## Split into Positive and Negative
ngp.dtm.index <- utils.getDocsIdForClass(ngp.dtm, ngp.class, dir.positive)
ngp.PS <- ngp.dtm[ngp.dtm.index, ]
ngp.NS <- ngp.dtm[!(rownames(ngp.dtm) %in% ngp.dtm.index), ]

## Mark Positive data in class
ngp.class["class"] <- -1
ngp.class[rownames(ngp.PS), ]$class <- 1

##  Check, then clean up env
stopifnot(nrow(ngp.PS) + nrow(ngp.NS) == nrow(ngp.dtm))
rm(ngp.data, ngp.dtm, ngp.dtm.index, parallel.cluster)


timer.readNewsgroup <- proc.time() - timer.start
beepr::beep(1)
cat("Read data completed, starting loop")





##############################################
####    Function for 1 sampling iteration
####    This will run on the parallel loop below
##############################################
ngp.sampling <- function
    (ngp.PS, ngp.NS, ngp.class, ngp.output, trnLabeled, var.i) {

  cat("TrnPct", trnLabeled[var.i], " |  Sample", var.j, "of 10\n")
  ## Split for training/testing sets, 60% of data to be set as training
  temp <- sample(1:nrow(ngp.PS), 0.6 * nrow(ngp.PS), replace = FALSE)
  ngp.trn.PS <- ngp.PS[temp, ]
  ngp.tst <- ngp.PS[-temp, ]

  temp <- sample(1:nrow(ngp.NS), 0.6 * nrow(ngp.NS), replace = FALSE)
  ngp.trn.NS <- ngp.NS[temp, ]
  ngp.tst <- c(ngp.tst, ngp.NS[-temp, ])

  ## Check for train/test split
  stopifnot(nrow(ngp.trn.PS) + nrow(ngp.trn.NS) + nrow(ngp.tst)
            == nrow(ngp.PS) + nrow(ngp.NS))



  ## Split for labeled data and unlabeled data
  temp <- sample(
  		  1:nrow(ngp.trn.PS),
  		  ceiling(trnLabeled[var.i] * nrow(ngp.trn.PS)),
  		  replace = FALSE)
  ngp.trn.US <- c(ngp.trn.PS[-temp, ], ngp.trn.NS)  # Move unlabeled PS into US
  ngp.trn.PS <- ngp.trn.PS[temp, ]                  # Remove unlabeled PS from the set
  rm(ngp.trn.NS)

  ## Check for labeled/unlabeled split
  stopifnot(nrow(ngp.trn.PS) + nrow(ngp.trn.US) + nrow(ngp.tst)
            == nrow(ngp.PS) + nrow(ngp.NS))



  ## Mark labeled training data in data frame
  ngp.trn.class <- ngp.class[c(rownames(ngp.trn.PS), rownames(ngp.trn.US)), ]
  ngp.trn.class["isLabeled"] <- FALSE
  ngp.trn.class["label"] <- -1
  ngp.trn.class[rownames(ngp.trn.PS), ]$isLabeled <- TRUE
  ngp.trn.class[rownames(ngp.trn.PS), ]$label <- 1
  ngp.trn.class$label <- as.factor(ngp.trn.class$label)

  ## Convert to matrix
  ngp.trn <- c(ngp.trn.PS, ngp.trn.US)
  ngp.trnMatrix <- as.matrix(ngp.trn)

  ## Arrange by document ID, check IDs are correct
  ngp.trnMatrix <- ngp.trnMatrix[order(rownames(ngp.trnMatrix)), ]
  ngp.trn.class <- ngp.trn.class[order(rownames(ngp.trn.class)), ]
  stopifnot(rownames(ngp.trnMatrix) == rownames(ngp.trn.class))


  ## Repeat matrix + class for testing data
  ngp.tstMatrix <- as.matrix(ngp.tst)
  ngp.tstMatrix <- ngp.tstMatrix[order(rownames(ngp.tstMatrix)), ]
  ngp.tst.class <- ngp.class[rownames(ngp.tst), ]
  ngp.tst.class <- ngp.tst.class[order(rownames(ngp.tst.class)), ]

  ## Creating folds for 10-fold cross validation used later
  ngp.tst.class$fold <- createFolds(rownames(ngp.tst.class), k = 10, list = FALSE, returnTrain = FALSE)



  ##############################################
  ####    Build Models
  cat("    Building Models: ", trnLabeled[var.i], "% / sample", var.j, "\n", sep="")
  sink("newsgroup-output.txt", append=TRUE) 
  # models.nBayes <- naiveBayes(ngp.trnMatrix, ngp.trn.class$label, laplace = 0.1)
  models.Spy_EM <- ngp.model.Spy_EM(ngp.trnMatrix, ngp.trn.class)

  
  ################################################
  ## Run the models on test data
  cat("    Predicting: ", trnLabeled[var.i], "% / sample", var.j, "\n", sep="")
  sink("newsgroup-output.txt", append=TRUE) 
  # results.nBayes <- predict(models.nBayes, ngp.tstMatrix)
  results.Spy_EM <- predict(models.Spy_EM, ngp.tstMatrix)

  # TODO: Remove fake stand-in for actual
  results.nBayes <- as.factor(rep(-1, nrow(ngp.tstMatrix)))


  ################################################
  ## Calculating performance
  cat("    Calculating Performance: ", trnLabeled[var.i], "% / sample", var.j, "\n", sep="")
  sink("newsgroup-output.txt", append=TRUE) 

  ## Calculate performance for each fold
  for (i in 1:10) {
    if (i == 1) {
      ngp.output[which(ngp.output == -1)] <- 0
    }

    ## Naive-Bayes
    ngp.tst.class$predict <- utils.convertFactorToNumeric(results.nBayes)
    ngp.output["fmeasure", "nBayes"] <- ngp.output["fmeasure", "nBayes"] + utils.calculateFMeasure(ngp.tst.class[ngp.tst.class$fold == i, ])
    ngp.output["accuracy", "nBayes"] <- ngp.output["accuracy", "nBayes"] + utils.calculateAccuracy(ngp.tst.class[ngp.tst.class$fold == i, ])

    ## Spy-EM
    ngp.tst.class$predict <- utils.convertFactorToNumeric(results.Spy_EM)
    ngp.output["fmeasure", "Spy-EM"] <- ngp.output["fmeasure", "Spy-EM"] + utils.calculateFMeasure(ngp.tst.class[ngp.tst.class$fold == i, ])
    ngp.output["accuracy", "Spy-EM"] <- ngp.output["accuracy", "Spy-EM"] + utils.calculateAccuracy(ngp.tst.class[ngp.tst.class$fold == i, ])
  }

  ## Average results over 10 folds
  ngp.output <- ngp.output / 10

  return(ngp.output)
}





##############################################
####    Begin loops to sample data
##############################################
## Percentage of training data to be set as labeled
# TODO: Set correct values after code is stable
# trnLabeled <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50, 0.65)
trnLabeled <- c(0.10, 0.20, 0.40, 0.65)
# trnLabeled <- c(0.30, 0.65)

# TODO: Set to 10 for actual run
repSamples <- 4

namesOfClassifiers <- c("nBayes", "Spy-EM")
numberOfClassifiers <- length(namesOfClassifiers)


## Set up cluster for parallel
parallel.cluster <- utils.createParallelCluster()
registerDoParallel(parallel.cluster)


## Build matrix to store overall results
ngp.fmeasure.results <- matrix(rep(-1, numberOfClassifiers * length(trnLabeled)),
                               length(trnLabeled))
rownames(ngp.fmeasure.results) <- trnLabeled
colnames(ngp.fmeasure.results) <- namesOfClassifiers
ngp.accuracy.results <- ngp.fmeasure.results

## Vary % of data that is labeled data
for (var.i in 1:length(trnLabeled)) {

  ## Build a row of of the results, for ngp.sampling to return
  ngp.sampling.output.row <- matrix(rep(-1, numberOfClassifiers*2), nrow=2)
	colnames(ngp.sampling.output.row) <- namesOfClassifiers
	rownames(ngp.sampling.output.row) <- c("fmeasure", "accuracy")


	##############################################
  #### Repeat sampling 10 times to avoid sampling bias
  ngp.sampling.results <- foreach(var.j = 1:repSamples,
          .packages = c("tm", "e1071", "SnowballC", "caret")) %dopar% ngp.sampling(
            ngp.PS, ngp.NS, ngp.class, ngp.sampling.output.row, trnLabeled, var.i)
  ##############################################


  ## Pull results from repeated sampling
  ngp.sampling.fmeasure <- matrix(rep(0, numberOfClassifiers), nrow=1)
  colnames(ngp.sampling.fmeasure) <- namesOfClassifiers
  ngp.sampling.accuracy <- ngp.sampling.fmeasure
  print(ngp.sampling.results)
  for (sample in ngp.sampling.results) {
    stopifnot(!(-1 %in% sample | NaN %in% sample))
    ngp.sampling.fmeasure <- ngp.sampling.fmeasure + sample["fmeasure", ]
    ngp.sampling.accuracy <- ngp.sampling.accuracy + sample["accuracy", ]
  }

  ## Calculate mean
  ngp.fmeasure.results[var.i, ] <- ngp.sampling.fmeasure / repSamples
  ngp.accuracy.results[var.i, ] <- ngp.sampling.fmeasure / repSamples

  beepr::beep(1)
}

stopCluster(parallel.cluster)
timer.loop <- proc.time() - timer.readNewsgroup
beepr::beep(8)


## Save workspace, just in case
save.image("newsgroup-results.RData")


## PLOT FOR F-MEASURE
utils.plotGraph(ngp.fmeasure.results, "F-Measure")
utils.plotGraph(ngp.accuracy.results, "Accuracy")
timer.total <- proc.time() - timer.start