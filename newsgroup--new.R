rm(list=ls())

library(tm)
library(tm.plugin.mail)
library(e1071)
library(beepr)
library(SnowballC)

library(parallel)
library(snow)



setwd("C:/Users/DongWei/Documents/Projects/data-scooping")

source("Spy_EM.R")

utils.createParallelCluster <- function() {
  numberOfCores <- max(1, detectCores() - 1)
  cluster <- makeCluster(parallel.numberOfCores,
                                  outfile = "newsgroup-output.txt")

  return(cluster)
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
  colnames(classes) <- c("class")
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
ngp.class["positive"] <- 0
ngp.class[rownames(ngp.PS), ]$positive <- 1

##  Check, then clean up env
stopifnot(nrow(ngp.PS) + nrow(ngp.NS) == nrow(ngp.dtm))
rm(ngp.data, ngp.dtm, ngp.dtm.index)

cat("Read data completed, entering loop")
beepr::beep(1)





##############################################
####    Begin loops
##############################################
## Percentage of training data to be set as labeled
# trnLabeled <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.45, 0.55, 0.65)
trnLabeled <- c(0.65)

var.i <- 1
var.j <- 1
## Vary % of data that is labeled data
# for (var.i in 1:length(trnLabeled)) {

  ## Repeat 10 times to avoid sampling bias
  ## TODO: Set to 10 after code is stable
  # for (var.j in 1:1) {
    cat("TrnPct", trnLabeled[var.i], " |  Sample", var.j, "of 10\n")

    ## Split for training/test sets, 60% of data to be set as training
    temp <- sample(1:nrow(ngp.PS), 0.6 * nrow(ngp.PS), replace = FALSE)
    ngp.trn.PS <- ngp.PS[temp, ]
    ngp.tst <- ngp.PS[-temp, ]

    temp <- sample(1:nrow(ngp.NS), 0.6 * nrow(ngp.NS), replace = FALSE)
    ngp.trn.NS <- ngp.NS[temp, ]
    ngp.tst <- c(ngp.tst, ngp.NS[-temp, ])

    stopifnot(nrow(ngp.trn.PS) + nrow(ngp.trn.NS) + nrow(ngp.tst) == nrow(ngp.PS) + nrow(ngp.NS))



    ## Split for labeled data and unlabeled data
    temp <- sample(
              1:nrow(ngp.trn.PS),
              ceiling(trnLabeled[var.i] * nrow(ngp.trn.PS)),
              replace = FALSE)
    ngp.trn.PS <- ngp.trn.PS[temp, ]
    ngp.trn.US <- c(ngp.trn.PS[-temp, ], ngp.trn.NS)
    rm(ngp.trn.NS)


    ## Mark labeled data in df to track classes
    ngp.trn.class <- ngp.class[c(rownames(ngp.trn.PS), rownames(ngp.trn.US)), ]
    ngp.trn.class["labeled"] <- 0
    ngp.trn.class[rownames(ngp.trn.PS), ]$labeled <- 1


    ## Convert to matrix
    ngp.trn <- c(ngp.trn.PS, ngp.trn.US)
    ngp.trnM <- as.matrix(ngp.trn)

    ## Arrange by document ID, check IDs are correct
    ngp.trnM <- ngp.trnM[order(rownames(ngp.trnM)), ]
    ngp.trn.class <- ngp.trn.class[order(rownames(ngp.trn.class)), ]
    stopifnot(rownames(ngp.trnM) == rownames(ngp.trn.class))


    ngp.tstM <- as.matrix(ngp.tst)


    ##############################################
    ####    Build Classifiers

    cat("    Building Classifiers: ", trnLabeled[var.i], "% / sample", var.j, "\n", sep="")
    # classifer.nb <- naiveBayes(ngp.trnM.data, ngp.trn.class, laplace = 0.15)
    classifer.nb <- naiveBayes(ngp.trnM.data, ngp.trn.class)
    results.nb <- predict(classifer.nb, ngp.tstM)

    count <- which(ngp.tst.class == results.nb)
    count <- length(count)
    total <- length(results.nb)

    beepr::beep(10)



    ################################################
    ## Run the classifers on test data
    cat("    Predicting: ", trnLabeled[var.i], "% / sample", var.j, "\n", sep="")




    ################################################
    ## Calculating performance
    cat("    Calculating Performance: ", trnLabeled[var.i], "% / sample", var.j, "\n", sep="")
#  }

  beepr::beep(1)
#}



beepr::beep(2)

