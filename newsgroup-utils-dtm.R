utils.createDtmFromCorpora <- function(corpora, sparsenessThreshold) {
  dtms = utils.createDtms(corpora)
  dtm = do.call(c, dtms)
  dtm <- utils.filterDtmTerms(dtm, sparsenessThreshold)
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

utils.filterDtmTerms <- function(x, minDocFreq) {
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

utils.getDocsIdForClass <- function(dtm, dtm.class, classVector) {
  index <- sapply(classVector, function(class) {
     which(dtm.class == class)
  })
  index <- unlist(index, use.names = FALSE)
  documentId <- rownames(dtm.class)[index]
  return(documentId)
}
