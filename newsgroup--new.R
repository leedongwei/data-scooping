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



setwd("C:/Users/DongWei/Documents/Projects/data-scooping/20news-bydate-test")

source("Spy_EM.R")

utils.createParallelCluster <- function() {
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
  yrange <- range(c(0.5, 1))

  plot(xrange, yrange, type = "n", xlab = "% of training set", ylab=nameOfGraph)
  colors <- rainbow(length(ngp.results))
  linetype <- c(1:length(ngp.results))
  plotchar <- seq(18,18+length(rownames(ngp.results)),1)

  for (j in 1:length(ngp.results)) {
    for (i in 1:1) {
      singleCol <- ngp.results[,j]
      lines(rownames(ngp.results), singleCol, type="b", lwd=1.5,
            lty=linetype[j], col=colors[j], pch=plotchar[j])

    }
  }

  legend("bottomright", colnames(ngp.results), cex=0.8, col=colors,
         pch=plotchar, lty=linetype, title=paste(nameOfGraph, "Graph"))
}


# utils.prepCorpora <- function(dirVector) {
#   corpora <- lapply(dirVector, function(dirName) {
#     return(utils.prepCorpus(dirName))
#   })
#   names(corpora) = dirVector
#   return(corpora)
# }

utils.prepCorpora.parallel <- function(cluster, dirVector) {
  ## Set up for parLapply
  clusterExport(cluster,
                c("utils.prepCorpus", "utils.cleanCorpus"))
  clusterEvalQ(cluster,
               c(library(tm), library(tm.plugin.mail), library(parallel)))

  corpora <- parLapply(cluster, dirVector, function(dirName) {
    cat("Reading docs from ", dirName, "\n", sep="")
    return(utils.prepCorpus(dirName))
  })
  names(corpora) = dirVector
  return(corpora)
}

utils.prepCorpus <- function(dirName) {
  corpus <- Corpus(
              DirSource(
                paste("data/20news-bydate/", dirName, sep = "")),
              readerControl=list(reader=readMail, language="en_US"))
  names(corpus) <- dirName
  return (utils.cleanCorpus(corpus))
}

utils.cleanCorpus <- function(corpus) {
  removeStopWords <- function(doc) {
    return(removeWords(doc, stopwords("english")))
  }
  convert <- function(t) {
    return(iconv(t, "ISO-8859-1", "UTF-8"))
  }
  data <- corpus
  data <- tm_map(data, removeSignature)
  data <- tm_map(data, convert)
  data <- tm_map(data, PlainTextDocument)
  data <- tm_map(data, content_transformer(tolower))
  data <- tm_map(data, removeStopWords)
  data <- tm_map(data, stripWhitespace)
  data <- tm_map(data, removeNumbers)
  data <- tm_map(data, removePunctuation)
  data <- tm_map(data, stemDocument)
  return (data)
}


utils.createDtmFromCorpora <- function(corpora, sparsenessThreshold) {
  dtms = utils.createDtms(corpora)
  dtm = do.call(c, dtms)
  dtm <- utils.filterTerms(dtm, sparsenessThreshold)
  dtm <- utils.resetDtmColumnsNames(dtm)
  return (dtm)
}

utils.createDtms <- function(corpora) {
  count <- as.integer(1:length(corpora))
  corporaNames <- names(corpora)

  data <- lapply(count, function(i) {
    ## weird hack to label each document with corpus name
    return (utils.createDtm(corpora[[i]], corporaNames[i]))
  })

  names(data) = names(corpora)
  return(data)
}

utils.createDtm <- function(corpus, corpusName) {
  m <- DocumentTermMatrix(corpus)
  rownames(m) <- rep(corpusName, dim(m)[1])
  return(m)
}

utils.resetDtmColumnsNames <- function(dtm) {
  count <- length(colnames(dtm))
  names <- sapply(1:count, function(num) {
    paste("V", as.String(num), sep="")
  })

  colnames(dtm) <- names
  return(dtm)
}

utils.extractDtmClasses <- function(dtm) {
  classes <- as.data.frame(rownames(dtm))
  colnames(classes) <- c("className")
  rownames(classes) <- sapply(1:nrow(classes), function(nn) {
    return(paste("D", nn, sep=""))
  })

  return(classes)
}

utils.resetDtmDocNames <- function(dtm) {
  newnames <- as.character(1:nrow(dtm))
  rownames(dtm) <- sapply(newnames, function(nn) {
    return(paste("D", nn, sep=""))
  })

  return(dtm)
}


utils.filterTerms <- function(x, minDocFreq) {
  # select terms with frequency >= minDocFreq
  # inputs:
  #   x, the DocumentTermMatrix matrix;
  #   minDocFreq, the minimum required frequency
  # returns:
  #   the modified DocumentTermMatrix matrix
  stopifnot(inherits(x, c("DocumentTermMatrix", "TermDocumentMatrix")),
            is.numeric(minDocFreq), minDocFreq > 0, minDocFreq < max(dim(x)))

  if (inherits(x, "DocumentTermMatrix")) {
    m <- t(x)
  } else {
    m <- x
  }

  t <- table(m$i) > minDocFreq

  termIndex <- as.numeric(names(t[t]))
  if (inherits(x, 'DocumentTermMatrix')) {
    return(x[, termIndex])
  } else {
    return(x[termIndex, ])
  }
}

utils.getDocsForClass <- function(dtm, dtm.class, classVector) {
  index <- sapply(classVector, function(class) {
     which(dtm.class == class)
  })
  index <- unlist(index, use.names = FALSE)
  documentId <- rownames(dtm.class)[index]
  return(documentId)
}

## True positives (TP) - Positive labeled correctly
## True negatives (TN) - Negative labeled correctly
## False positives (FP) - Positive labeled wrongly
## False negatives (FN) - Negative labeled wrongly
utils.calculateFMeasure <- function(ngp.results) {
  TP <- nrow(ngp.results[(ngp.results$class == 1 & ngp.results$predict == 1), ])
  TN <- nrow(ngp.results[(ngp.results$class == -1 & ngp.results$predict == -1), ])
  FP <- nrow(ngp.results[(ngp.results$class == -1 & ngp.results$predict == 1), ])
  FN <- nrow(ngp.results[(ngp.results$class == 1 & ngp.results$predict == -1), ])

  Precision <- TP / (TP+FP)
  Recall    <- TP / (TP+FN)

  return ((2 * Precision * Recall) / (Precision + Recall))
}

utils.calculateAccuracy <- function(ngp.results) {
  TP <- nrow(ngp.results[(ngp.results$class == 1 & ngp.results$predict == 1), ])
  TN <- nrow(ngp.results[(ngp.results$class == -1 & ngp.results$predict == -1), ])
  FP <- nrow(ngp.results[(ngp.results$class == -1 & ngp.results$predict == 1), ])
  FN <- nrow(ngp.results[(ngp.results$class == 1 & ngp.results$predict == -1), ])

  Accuracy <- (TP+TN) / (TP+FP+TN+FN)
  return (Accuracy)
}











##############################################
####    Start of code
##############################################
file.remove("newsgroup-output.txt")
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
# ngp.data <- utils.prepCorpora(dir.selected)
ngp.data <- utils.prepCorpora.parallel(parallel.cluster, dir.selected)
ngp.dtm  <- utils.createDtmFromCorpora(ngp.data, 35)


## Reset document ID and extract class names
ngp.class <- utils.extractDtmClasses(ngp.dtm)
ngp.dtm <- utils.resetDtmDocNames(ngp.dtm)

## Split into Positive and Negative
ngp.dtm.index <- utils.getDocsForClass(ngp.dtm, ngp.class, dir.positive)
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
trnLabeled <- c(0.10, 0.30, 0.65)
repSamples <- 5
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
    results.nb <- predict(classifer.nb, ngp.tstM)


    ################################################
    ## Calculating performance
    cat("    Calculating Performance: ", trnLabeled[var.i], "% / sample", var.j, "\n", sep="")

    ngp.tst.class$predict <- utils.convertFactorToNumeric(results.nb)
    ngp.fmeasure.samples[var.j, 1] <- utils.calculateFMeasure(ngp.tst.class)
    ngp.accuracy.samples[var.j, 1] <- utils.calculateAccuracy(ngp.tst.class)

  }

  ## Mean results from 10 random samples, and store in main matrix
  for (i in numberOfClassifiers) {
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