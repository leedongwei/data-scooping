rm(list=ls())

## Essential libraries
library(tm)
library(tm.plugin.mail)
library(e1071)
library(SnowballC)

## Parallel libraries
library(parallel)
library(doParallel)
library(foreach)
library(snow)

## For sound alerts
library(beepr)



setwd("C:/Users/DongWei/Documents/Projects/data-scooping/")



source("newsgroup-utils-perf.R")
source("newsgroup-utils-corpus.R")
source("newsgroup-utils-dtm.R")

# source("Spy_EM.R")



utils.createParallelCluster <- function() {
  cat("Creating parallel cluster...\n")
  numberOfCores <- max(1, detectCores() - 1)
  cluster <- makeCluster(numberOfCores,
                         outfile = "newsgroup-output.txt")

  cat("Created parallel cluster with ", numberOfCores, " cores", sep="")
  return(cluster)
}

utils.convertFactorToNumeric <- function(factor) {
  as.numeric(levels(factor))[factor]
}

utils.plotGraph <- function(ngp.results, nameOfGraph) {
  xrange <- range(rownames(ngp.results))
  yrange <- range(c(0, 1))

  plot(xrange, yrange, type = "n", xlab = "% of training set", ylab=nameOfGraph)
  colors <- rainbow(ncol(ngp.results))
  linetype <- c(1:ncol(ngp.results))
  plotchar <- seq(18,18+length(rownames(ngp.results)),1)

  for (j in 1:ncol(ngp.results)) {
      singleCol <- ngp.results[,j]
      lines(rownames(ngp.results), as.numeric(singleCol), type="b", lwd=1.5,
            lty=linetype[j], col=colors[j], pch=plotchar[j])
  }

  legend("bottomright", colnames(ngp.results), cex=0.8, col=colors,
         pch=plotchar, lty=linetype, title=paste(nameOfGraph, "Graph"))
}







##############################################
####    Start of code
##############################################
# file.remove("newsgroup-output.txt")
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
# ngp.data <- utils.prepCorpora(dir.selected)   Non-parallel code
ngp.data <- utils.prepCorpora.parallel(parallel.cluster, dir.selected)
ngp.dtm  <- utils.createDtmFromCorpora(ngp.data, 35)


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
rm(ngp.data, ngp.dtm, ngp.dtm.index)


beepr::beep(1)
stopCluster(parallel.cluster)
cat("Read data completed, starting loop")



##############################################
####    Begin loops
##############################################
## Percentage of training data to be set as labeled
# TODO: Set correct values after code is stable
# trnLabeled <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.45, 0.55, 0.65)
# trnLabeled <- c(0.10, 0.30, 0.65)
# repSamples <- 4
trnLabeled <- c(0.30, 0.65)
repSamples <- 3

namesOfClassifiers <- c("nBayes", "Spy-EM")
numberOfClassifiers <- length(namesOfClassifiers)
# var.i <- 1
# var.j <- 1


## Set up cluster for parallel
parallel.cluster <- utils.createParallelCluster()
registerDoParallel(parallel.cluster)


## Build matrix to store overall results
ngp.fmeasure.results <- matrix(rep(-1, numberOfClassifiers * length(trnLabeled)), length(trnLabeled))
  rownames(ngp.fmeasure.results) <- trnLabeled
  colnames(ngp.fmeasure.results) <- namesOfClassifiers
ngp.accuracy.results <- ngp.fmeasure.results

## Vary % of data that is labeled data
for (var.i in 1:length(trnLabeled)) {

  ## Build matrix to store results from samples
  ngp.fmeasure.samples <- matrix(rep(-1, numberOfClassifiers * repSamples), repSamples)
  colnames(ngp.fmeasure.samples) <- namesOfClassifiers
  ngp.accuracy.samples <- ngp.fmeasure.samples


  ## Repeat 10 times to avoid sampling bias
  # for (var.j in 1:repSamples) {
  foreach(var.j = 1:repSamples,
          .packages = c("tm", "e1071", "SnowballC", "beepr")) %dopar% {

    cat("TrnPct", trnLabeled[var.i], " |  Sample", var.j, "of 10\n")
    ## Split for training/testing sets, 60% of data to be set as training
    temp <- sample(1:nrow(ngp.PS), 0.6 * nrow(ngp.PS), replace = FALSE)
    ngp.trn.PS <- ngp.PS[temp, ]
    ngp.tst <- ngp.PS[-temp, ]

    temp <- sample(1:nrow(ngp.NS), 0.6 * nrow(ngp.NS), replace = FALSE)
    ngp.trn.NS <- ngp.NS[temp, ]
    ngp.tst <- c(ngp.tst, ngp.NS[-temp, ])

    ## Check for train/test split
    stopifnot(nrow(ngp.trn.PS) + nrow(ngp.trn.NS) + nrow(ngp.tst) == nrow(ngp.PS) + nrow(ngp.NS))



    ## Split for labeled data and unlabeled data
    temp <- sample(
              1:nrow(ngp.trn.PS),
              ceiling(trnLabeled[var.i] * nrow(ngp.trn.PS)),
              replace = FALSE)
    ngp.trn.US <- c(ngp.trn.PS[-temp, ], ngp.trn.NS)  # Move unlabeled PS into US
    ngp.trn.PS <- ngp.trn.PS[temp, ]                  # Remove unlabeled PS from the set
    rm(ngp.trn.NS)

    ## Check for labeled/unlabeled split
    stopifnot(nrow(ngp.trn.PS) + nrow(ngp.trn.US) + nrow(ngp.tst) == nrow(ngp.PS) + nrow(ngp.NS))



    ## Mark labeled training data in data frame
    ngp.trn.class <- ngp.class[c(rownames(ngp.trn.PS), rownames(ngp.trn.US)), ]
    ngp.trn.class["isLabeled"] <- FALSE
    ngp.trn.class["label"] <- -1
    ngp.trn.class[rownames(ngp.trn.PS), ]$isLabeled <- TRUE
    ngp.trn.class[rownames(ngp.trn.PS), ]$label <- 1
    ngp.trn.class$label <- as.factor(ngp.trn.class$label)

    ## Convert to matrix
    ngp.trn <- c(ngp.trn.PS, ngp.trn.US)
    ngp.trnM <- as.matrix(ngp.trn)

    ## Arrange by document ID, check IDs are correct
    ngp.trnM <- ngp.trnM[order(rownames(ngp.trnM)), ]
    ngp.trn.class <- ngp.trn.class[order(rownames(ngp.trn.class)), ]
    stopifnot(rownames(ngp.trnM) == rownames(ngp.trn.class))


    ## Repeat matrix + class for testing data
    ngp.tstM <- as.matrix(ngp.tst)
    ngp.tstM <- ngp.tstM[order(rownames(ngp.tstM)), ]
    ngp.tst.class <- ngp.class[rownames(ngp.tst), ]
    ngp.tst.class <- ngp.tst.class[order(rownames(ngp.tst.class)), ]



    ##############################################
    ####    Build Classifiers
    cat("    Building Classifiers: ", trnLabeled[var.i], "% / sample", var.j, "\n", sep="")
    classifer.nb <- naiveBayes(ngp.trnM, ngp.trn.class$label, laplace = 0.15)




    ################################################
    ## Run the classifers on test data
    cat("    Predicting: ", trnLabeled[var.i], "% / sample", var.j, "\n", sep="")
    # results.nb <- predict(classifer.nb, ngp.tstM)


    ################################################
    ## Calculating performance
    cat("    Calculating Performance: ", trnLabeled[var.i], "% / sample", var.j, "\n", sep="")

    ## Naive-Bayes
    # ngp.tst.class$predict <- utils.convertFactorToNumeric(results.nb)
    ngp.fmeasure.samples[var.j, 1] <- utils.calculateFMeasure(ngp.tst.class)
    ngp.accuracy.samples[var.j, 1] <- utils.calculateAccuracy(ngp.tst.class)

    ## Spy-EM
    ngp.fmeasure.samples[var.j, 2] <- (var.j + var.i) / 10
    ngp.accuracy.samples[var.j, 2] <- (var.j + var.i) / 10


    ## Remove biggest data variables from memory
    rm(ngp.trnM, ngp.tstM)
  }

  ## Mean results from 10 random samples, and store in main matrix
  for (i in 1:numberOfClassifiers) {
    ngp.fmeasure.results[var.i, i] <- mean(ngp.fmeasure.samples[, i])
    ngp.accuracy.results[var.i, i] <- mean(ngp.accuracy.samples[, i])
  }

  beepr::beep(1)
}

stopCluster(parallel.cluster)
beepr::beep(2)






## PLOT FOR F-MEASURE
utils.plotGraph(ngp.fmeasure.results, "F-Measure")
utils.plotGraph(ngp.accuracy.results, "Accuracy")