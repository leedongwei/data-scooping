utils.prepCorpora <- function(dirVector) {
  corpora <- lapply(dirVector, function(dirName) {
    return(utils.prepCorpus(dirName))
  })
  names(corpora) = dirVector
  return(corpora)
}

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
  ## TODO: Use main folder to grab all documents
  # corpus <- Corpus(
  #             DirSource(
  #               paste("data/20news-bydate/", dirName, sep = "")),
  #             readerControl=list(reader=readMail, language="en_US"))
  corpus <- Corpus(
              DirSource(
                paste("data/20news-bydate/20news-bydate-test/", dirName, sep = "")),
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
