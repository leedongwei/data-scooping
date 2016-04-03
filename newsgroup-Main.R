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

dir.selected <- dir.all
dir.positive <- c( "comp.graphics",
                   "comp.os.ms-windows.misc",  "comp.sys.ibm.pc.hardware",
                   "comp.sys.mac.hardware",    "comp.windows.x")


## TODO: For debugging
# dir.positive <- c("comp.os.ms-windows.misc", "comp.windows.x", "comp.sys.mac.hardware")
# dir.selected <- c("comp.os.ms-windows.misc", "comp.windows.x", "comp.sys.mac.hardware",
#                   "rec.sport.baseball", "rec.sport.hockey",
#                   "alt.atheism", "sci.space", "soc.religion.christian",
#                   "talk.politics.guns",       "talk.politics.mideast",
#                   "talk.politics.misc",       "talk.religion.misc")
dir.positive <- c("comp.os.ms-windows.misc", "comp.windows.x")
dir.selected <- c("comp.os.ms-windows.misc", "comp.windows.x", 
                  "rec.sport.baseball", "rec.sport.hockey",
                  "alt.atheism", "sci.space")


## Read to corpus, convert into DTM and clean it
## TODO: Set data folder in "newsgroup-utils-corpus.R/utils-prepCorpus"
# Non-parallel code: ngp.data <- utils.prepCorpora(dir.selected)
ngp.data <- utils.prepCorpora.parallel(parallel.cluster, dir.selected)

# TODO: Set to 35 for actual run
# ngp.dtm  <- utils.createDtmFromCorpora(ngp.data, 35)
ngp.dtm  <- utils.createDtmFromCorpora(ngp.data, 20)

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
trnLabeled <- c(0.10, 0.15, 0.25)
# trnLabeled <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50, 0.65)

## Set reps in multiples of cores for maximize processing time
## If you have 3 cores, repSamples should be 3/6/9/12
repSamples <- 9

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

# utils.plotGraph(ngp.fmeasure.results[,c("AllPos", "nBayes", "SVM", "Rocchio", "Spy-EM", "RocSVM")], "F-Measure") 
