libs <- c("tm", "e1071")
install.packages(libs, dependencies=TRUE)
lapply(libs, require, character.only=TRUE)

setwd("C:/Users/DongWei/Documents/Projects/data-scooping/data/20news-bydate")

# setwd("C:/Users/DongWei/Documents/Projects/data-scooping")





###############################################
#### Create Utility functions
################################################
dongwei.cleanData <- function(dataCorpus) {
  dataCorpus <- tm_map(dataCorpus, content_transformer(tolower))
  dataCorpus <- tm_map(dataCorpus, removeWords, stopwords("english"))
  dataCorpus <- tm_map(dataCorpus, removePunctuation)
  dataCorpus <- tm_map(dataCorpus, removeNumbers)
  dataCorpus <- tm_map(dataCorpus, stemDocument)
  dataCorpus <- tm_map(dataCorpus, stripWhitespace)
  dataCorpus <- tm_map(dataCorpus, PlainTextDocument)

  return(dataCorpus)
}

dongwei.setCorpusDocumentId <- function(startNum, dataCorpus) {
  for (i in seq(from=1, to=length(dataCorpus), by=1)){
    dataCorpus[[i]]$meta$id <- startNum + i
  }

  return(dataCorpus)
}

dongwei.labelClassToId <- function(stringClassName, dataCorpus) {
  classes <- rep(stringClassName, length(dataCorpus))
  classes <- data.frame(classes)
  
  row.names(classes) <- dongwei.getCorpusDocumentIdToVector(dataCorpus)
  return(classes)
}

dongwei.getCorpusDocumentIdToVector <- function(dataCorpus) {
  output <- c()

  for (i in seq(from=1, to=length(dataCorpus), by=1)){
    output <- rbind(output, dataCorpus[[i]]$meta$id)
  }

  return(output)
}






################################################
####    Loading the data
################################################
## PS: Positive set, subcategory: comp.graphics
data.PS.raw <- Corpus(
                  DirSource(directory="positive", encoding="UTF-8"),
                  readerControl=list(reader=readPlain,language="en"))

## NS: Negative set, note: "comp.graphics" has been moved out of "negative" folder
data.NS.raw <- Corpus(
                  DirSource(directory="negative", encoding="UTF-8", recursive=TRUE),
                  readerControl=list(reader=readPlain,language="en"))



################################################
####    Clean the data
################################################
data.PS <- dongwei.cleanData(data.PS.raw)
data.NS <- dongwei.cleanData(data.NS.raw)

# plainTextDocument resets IDs. This sets it back.
data.PS <- dongwei.setCorpusDocumentId(1,                   data.PS)
data.NS <- dongwei.setCorpusDocumentId(1 + length(data.PS), data.NS)



################################################
####    Split data into train/test sets
################################################
paper.g <- 0.15            # g is the percentage of positive set being labeled
paper.trn <- 0.6           # tp is the percentage of the entire set being used for training


# Split for NS
documentNum <- 1:length(data.NS)
trainingSize <- ceiling(length(data.NS) * paper.trn)
testingSize  <- length(data.NS) - trainingSize

trainingSet <- sample(documentNum, size=trainingSize)
testingSet  <- subset(documentNum, !(documentNum %in% trainingSet))

data.NS.train <- data.NS[trainingSet]
data.NS.test  <- data.NS[testingSet]


# Split for PS
documentNum <- 1:length(data.PS)
trainingSize <- ceiling(length(data.PS) * paper.trn)
testingSize  <- length(data.PS) - trainingSize

trainingSet <- sample(documentNum, size=trainingSize)
testingSet  <- subset(documentNum, !(documentNum %in% trainingSet))

data.PS.train <- data.PS[trainingSet]
data.PS.test  <- data.PS[testingSet]

# Split for labeled positives on training data
documentNum <- 1:length(data.PS.train)
labelledSize   <- ceiling(length(data.PS.train) * paper.g)
unlabelledSize <- length(data.PS.train) - trainingSize

labelledSet   <- sample(documentNum, size=labelledSize)
unlabelledSet <- subset(documentNum, !(documentNum %in% labelledSet))

data.PS.train.labelled   <- data.PS[labelledSet]
data.PS.train.unlabelled <- data.PS[unlabelledSet]



################################################
####    Labelling the sets of data <OLD>
################################################

## Testing Set:
# data.test = data.PS.test + data.NS.test
class.PS.test <- dongwei.labelClassToId("positive", data.PS.test)
class.NS.test <- dongwei.labelClassToId("positive", data.NS.test)

data.test  <- c(data.PS.test,  data.NS.test)
class.test <- rbind(class.PS.test, class.NS.test)


## Training Set:
# data.PS.train = data.PS.train.label + data.PS.train.unlabel
# data.train = data.PS.train + data.NS.train
class.PS.train.labelled   <- dongwei.labelClassToId("positive", data.PS.train.labelled)
class.PS.train.unlabelled <- dongwei.labelClassToId("positive", data.PS.train.unlabelled)
class.NS.train            <- dongwei.labelClassToId("negative", data.NS.train)
label.PS.train.labelled   <- dongwei.labelClassToId("positive", data.PS.train.labelled)
label.PS.train.unlabelled <- dongwei.labelClassToId("negative", data.PS.train.unlabelled)
label.NS.train            <- dongwei.labelClassToId("negative", data.NS.train)

# COMBINE BEFORE CLEANING
data.train.all      <- c(data.NS.train,          data.PS.train.unlabelled,      data.PS.train.labelled)
class.train.all     <- rbind(class.NS.train,     class.PS.train.unlabelled,     class.PS.train.labelled)
label.train.all     <- rbind(label.NS.train,     label.PS.train.unlabelled,     label.PS.train.labelled)

data.train.US       <- c(data.NS.train,          data.PS.train.unlabelled)
class.train.US      <- rbind(class.NS.train,     class.PS.train.unlabelled)
label.train.US      <- rbind(label.NS.train,     label.PS.train.unlabelled)


class.train.all    <- class.train.all[sort(rownames(class.train.all)),,drop=FALSE]
label.train.all    <- label.train.all[sort(rownames(label.train.all)),,drop=FALSE]
class.train.US <- class.train.US[sort(rownames(class.train.US)),,drop=FALSE]
label.train.US <- label.train.US[sort(rownames(label.train.US)),,drop=FALSE]



################################################
####    Create document term matrix
################################################
tdm.train.all <- t(TermDocumentMatrix(data.train.all))
tdm.train.all <- removeSparseTerms(tdm.train.all, 0.99)
tdm.train.all <- tdm.train.all[sort(rownames(tdm.train.all)),,drop=FALSE]

tdm.train.US <- t(TermDocumentMatrix(data.train.US))
tdm.train.US <- removeSparseTerms(tdm.train.US, 0.99)
tdm.train.US <- tdm.train.US[sort(rownames(tdm.train.US)),,drop=FALSE]

tdm.test <- t(TermDocumentMatrix(data.test))
tdm.test <- removeSparseTerms(tdm.test, 0.99)
tdm.test <- tdm.test[sort(rownames(tdm.test)),,drop=FALSE]



################################################
####    Run Naive-Bayes
################################################
#  Laplace = 0.1 is taken from [Bing Liu, 2003]
tdm.train.all.matrix        <- as.matrix(tdm.train.all)
tdm.train.US.matrix         <- as.matrix(tdm.train.US)


source("newsgroup-NaiveBayes.R")

# classifier.nb <- ngp.getNaiveBayesClassifier()
