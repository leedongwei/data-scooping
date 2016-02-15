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
####    Load code for Algorithms
################################################
##    Naive Bayes
source("bcw-NaiveBayes.R")

##    Spy-EM
source("bcw-SpyEM.R")

##    Utilities
source("bcw-utils-CalculateF.R")
source("bcw-utils-CalculateAccuracy.R")






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


################################################
################################################
################################################
####    Start testing here


f.NB <- numeric(0)
f.SEM <- numeric(0)
a.NB <- numeric(0)
a.SEM <- numeric(0)

trnPercent <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.45, 0.55, 0.65, 0.75)

## Vary % of data that is labeled data
for (var.i in 1:length(trnPercent)) {

  f.NB.row          <- trnPercent[var.i]
  f.SEM.row         <- trnPercent[var.i]
  f.RocSVM.row      <- trnPercent[var.i]
  f.RocCluSVM.row   <- trnPercent[var.i]
  f.LELC.row        <- trnPercent[var.i]
  f.globalSPUL.row  <- trnPercent[var.i]
  f.localSPUL.row   <- trnPercent[var.i]
  a.NB.row          <- trnPercent[var.i]
  a.SEM.row         <- trnPercent[var.i]
  a.RocSVM.row      <- trnPercent[var.i]
  a.RocCluSVM.row   <- trnPercent[var.i]
  a.LELC.row        <- trnPercent[var.i]
  a.globalSPUL.row  <- trnPercent[var.i]
  a.localSPUL.row   <- trnPercent[var.i]

  ## Avoid sampling bias, repeat 10 times
  for (var.j in 1:10) {

    cat("TrnPct", trnPercent[var.i], " |  Sample", var.j, "of 10\n")

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
      p = trnPercent[var.i],
      list = FALSE)

    ## Set up PS and US
    bcw.PS <- bcw.trn.positive[temp, ]
    bcw.US <- bcw.trn.positive[-temp, ]
    bcw.US <- rbind(bcw.US, bcw.trn.negative)

    ## Delete variables that are never again used
    ## Prevents confusion in Global Env
    rm(bcw.trn.positive, bcw.trn.negative)


    ## Creating folds for 10-fold cross validation used later
    bcw.tst$fold <- createFolds(rownames(bcw.tst), k = 10, list = FALSE, returnTrain = FALSE)



    ################################################
    ## Build the classifiers
    print("    Building Classifiers...")

    classifier.naiveBayes <- bcw.getNaiveBayesClassifier(bcw.PS, bcw.US)
    classifier.spyEm <- bcw.getSpyEmClassifier(bcw.PS, bcw.US)


    ################################################
    ## Run the classifers on test data
    print("    Predicting...")
    bcw.tst.NB <- bcw.tst
    bcw.tst.NB$predict <- predict(classifier.naiveBayes, bcw.tst[, bcw.features])

    bcw.tst.SEM <- bcw.tst
    bcw.tst.SEM$predict <- predict(classifier.spyEm, bcw.tst[, bcw.features])


    ################################################
    ## Calculating performance
    print("    Calculating Performance...")

    ## Calculate F-measure+Accuracy for each fold
    bcw.tst.NB.folds.f <- numeric(0)
    bcw.tst.NB.folds.a <- numeric(0)

    bcw.tst.SEM.folds.f <- numeric(0)
    bcw.tst.SEM.folds.a <- numeric(0)


    for (i in 1:10) {
      bcw.tst.NB.folds.f <- c(bcw.tst.NB.folds.f, bcw.calculateFMeasure(bcw.tst.NB[bcw.tst.NB$fold == i, ]))
      bcw.tst.NB.folds.a <- c(bcw.tst.NB.folds.a, bcw.calculateAccuracy(bcw.tst.NB[bcw.tst.NB$fold == i, ]))

      bcw.tst.SEM.folds.f <- c(bcw.tst.SEM.folds.f, bcw.calculateFMeasure(bcw.tst.SEM[bcw.tst.SEM$fold == i, ]))
      bcw.tst.SEM.folds.a <- c(bcw.tst.SEM.folds.a, bcw.calculateAccuracy(bcw.tst.SEM[bcw.tst.SEM$fold == i, ]))

    }
    f.NB.row <- c(f.NB.row, mean(bcw.tst.NB.folds.f))
    a.NB.row <- c(a.NB.row, mean(bcw.tst.NB.folds.a))

    f.SEM.row <- c(f.SEM.row, mean(bcw.tst.SEM.folds.f))
    a.SEM.row <- c(a.SEM.row, mean(bcw.tst.SEM.folds.a))
  }

  f.NB <- rbind(f.NB, f.NB.row)
  a.NB <- rbind(a.NB, a.NB.row)

  f.SEM <- rbind(f.SEM, f.SEM.row)
  a.SEM <- rbind(a.SEM, a.SEM.row)
}


## Utility function
shiftRownameThenMean <- function(dataset) {

  dataset <- data.frame(dataset, row.names = NULL)
  trnPercentName <- dataset[, 1]

  dataset <- dataset[, 2:length(dataset)]
  dataset <- data.frame(rowMeans(dataset, na.rm = TRUE))

  rownames(dataset) <- trnPercentName
  colnames(dataset) <- NULL
  return(dataset)
}

NB.f <- shiftRownameThenMean(f.NB)
SEM.f <- shiftRownameThenMean(f.SEM)
results.f.raw <- rbind(f.NB, f.SEM)
results.f <- cbind(NB.f, SEM.f)

NB.a <- shiftRownameThenMean(a.NB)
SEM.a <- shiftRownameThenMean(a.SEM)
results.a.raw <- rbind(a.NB, a.SEM)
results.a <- cbind(NB.a, SEM.a)



## PLOT FOR F-MEASURE
xrange <- range(rownames(results.f))
yrange <- range(c(0.5, 1))
plot(xrange, yrange, type = "n", xlab = "% of training set", ylab = "F-measure")
colors <- rainbow(length(results.f))
linetype <- c(1:length(results.f))
plotchar <- seq(18,18+length(rownames(results.f)),1)

for (j in 1:length(results.f)) {
  for (i in 1:1) {
    singleCol <- results.f[,j]
    lines(rownames(results.f), singleCol, type="b", lwd=1.5,
          lty=linetype[j], col=colors[j], pch=plotchar[j])

  }
}

legend("bottomright", colnames(results.f), cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="F-measure Graph")


## PLOT FOR ACCURACY
xrange <- range(rownames(results.a))
yrange <- range(c(0.5, 1))
plot(xrange, yrange, type = "n", xlab = "% of training set", ylab = "Accuracy")
colors <- rainbow(length(results.a))
linetype <- c(1:length(results.a))
plotchar <- seq(18,18+length(rownames(results.a)),1)

for (j in 1:length(results.a)) {
  for (i in 1:1) {
    singleCol <- results.a[,j]
    lines(rownames(results.a), singleCol, type="b", lwd=1.5,
          lty=linetype[j], col=colors[j], pch=plotchar[j])

  }
}

legend("bottomright", colnames(results.a), cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="Accuracy Graph")