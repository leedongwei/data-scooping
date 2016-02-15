rm(list=ls())

library(tm)
library(e1071)
library(beepr)
library(SnowballC)

# install.packages("tm.plugin.mail", repos="http://R-Forge.R-project.org")
# library(tm.plugin.mail)



setwd("C:/Users/DongWei/Documents/Projects/data-scooping")

source("Spy_EM.R")

utils.prepCorpora <- function(dirVector) {
  corpora <- lapply(dirVector, function(dirName) {
    return(utils.prepCorpus(dirName))
  })
  names(corpora) = dirVector
  return(corpora)
}

utils.prepCorpus <- function(dirName) {
  corpus <- Corpus(
              DirSource(
                paste("data/20news-bydate/", dirName, sep = "")),
              readerControl=list(reader= readPlain, language="en_US"))
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
  data <- tm_map(data, convert)
  # data <- tm_map(data, removeSignature)
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

utils.getClassVector <- function(dtm, classNames) {
  
}










##############################################
####    Start of code
##############################################
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
dir.selected <- c("alt.atheism",              "comp.graphics",
             "comp.os.ms-windows.misc",  "comp.sys.ibm.pc.hardware")

## TODO: Set positive class
dir.positive <- "comp.graphics"



ngp.data <- utils.prepCorpora(dir.selected)
ngp.dtm  <- utils.createDtmFromCorpora(ngp.data, 40) ##increase to 5000 words

ngp.dtm.class <- rownames(ngp.dtm)
ngp.dtm.index <- which(ngp.dtm.class == dir.positive)

ngp.PS <- ngp.dtm[ngp.dtm.index, ]
ngp.NS <- ngp.dtm[-ngp.dtm.index, ]

##  Clean up env
rm(ngp.data, ngp.dtm.class, ngp.dtm.index)
beepr::beep(1)





##############################################
####    Begin loops
##############################################
# trnPercent <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.45, 0.55, 0.65)
trnPercent <- c(0.10, 0.65)

## TODO: Delete after test
var.i <- 2
var.j <- 1

## Vary % of data that is labeled data
for (var.i in 1:length(trnPercent)) {



  ## Repeat 10 times to avoid sampling bias
  ## TODO: Set to 10
  for (var.j in 1:1) {

    ## Split for training/test sets
    temp <- sample(1:nrow(ngp.PS), 0.6 * nrow(ngp.PS), replace = FALSE)
    ngp.trn.PS <- ngp.PS[temp, ]
    ngp.tst <- ngp.PS[-temp, ]

    temp <- sample(1:nrow(ngp.NS), 0.6 * nrow(ngp.NS), replace = FALSE)
    ngp.trn.NS <- ngp.NS[temp, ]
    ngp.tst <- c(ngp.tst, ngp.NS[-temp, ])

    stopifnot(nrow(ngp.trn.PS) + nrow(ngp.trn.NS) + nrow(ngp.tst) == nrow(ngp.dtm))



    ## Split for labeled data and unlabeled data
    temp <- sample(1:nrow(ngp.trn.PS), trnPercent[var.i] * nrow(ngp.trn.PS), replace = FALSE)
    ngp.trn.PS <- ngp.trn.PS[temp, ]
    ngp.trn.US <- c(ngp.trn.PS[-temp, ], ngp.trn.NS)
    rm(ngp.trn.NS)


    ## Create class vector for training and factorize it
    ngp.trn.data <- c(ngp.trn.PS, ngp.trn.US)
    ngp.trn.class <- rep("-1", nrow(ngp.trn.data))
    temp <- which(rownames(ngp.trn.data) == dir.positive)
    ngp.trn.class[temp] <- "1"
    ngp.trn.class <- as.factor(ngp.trn.class)

    ## Create class vector for testing too
    ngp.tst.class <- rep("-1", nrow(ngp.tst))
    temp <- which(rownames(ngp.tst) == dir.positive)
    ngp.tst.class[temp] <- "1"
    ngp.tst.class <- as.factor(ngp.tst.class)


    ## Convert to matrix
    ngp.trnM.data <- as.matrix(ngp.trn.data)
    ngp.tstM <- as.matrix(ngp.tst)


    ##############################################
    ####    Build Classifiers
    # classifer.nb <- naiveBayes(ngp.trnM.data, ngp.trn.class, laplace = 0.15)
    classifer.nb <- naiveBayes(ngp.trnM.data, ngp.trn.class)
    results.nb <- predict(classifer.nb, ngp.tstM)

    count <- which(ngp.tst.class == results.nb)
    count <- length(count)
    total <- length(results.nb)

    beepr::beep(10)
  }

  beepr::beep(1)
}



beepr::beep(2)

