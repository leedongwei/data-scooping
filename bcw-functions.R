rm(list=ls())

library("e1071")
library("caret")
library("plyr")
library("Rsolnp")

## Used to alert me after a long analysis is completed
library(beepr)



################################################
####    Set Directory
################################################
## TODO: Set to your own directory
setwd("C:/Users/DongWei/Documents/Projects/data-scooping/data/breast-cancer-wisconsin")





################################################
####    Naive Bayes
################################################
bcw.getNaiveBayesClassifier <- function(bcw.PS, bcw,US) {
  bcw.PS$label <- 4
  bcw.US$label <- 2

  classifier.nb <- naiveBayes(
    as.factor(label) ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,
    data = (rbind(bcw.PS, bcw.US)),
    laplace = 0)

  return (classifier.nb)
}



################################################
####    Spy-Technique
################################################
bcw.getReliableNegativeWithSpyTechnique <- function(bcw.PS, bcw.US) {

  ## Label data with spy-tags
  bcw.PS$isSpy <- FALSE
  bcw.US$isSpy <- FALSE

  ## Value of s is taken from page 3 of Liu, Dai, Li, Lee, Yu (2003)
  cnst.s <- 0.15
  temp <- sample(nrow(bcw.PS), cnst.s * nrow(bcw.PS))

  ## Copy spies into US
  bcw.spy <- bcw.PS[temp, ]
  bcw.spy$isSpy <- TRUE
  bcw.US <- rbind(bcw.US, bcw.spy)

  # Remove spies from PS
  bcw.PS <- bcw.PS[-temp, ]

  ## Set up label, and set as factors for naiveBayes
  bcw.PS$spyLabel <- 4
  bcw.PS$spyLabel <- as.factor(bcw.PS$spyLabel)
  bcw.PS$Pr <- -1
  bcw.PS$PrN <- -1

  bcw.US$spyLabel <- 2
  bcw.US$spyLabel <- as.factor(bcw.US$spyLabel)
  bcw.US$Pr <- -1
  bcw.US$PrN <- -1


  classifier.nb <- naiveBayes(
    spyLabel ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,
    data = (rbind(bcw.PS, bcw.US)),
    laplace = 0)

  ## Loop till predicted results converge
  ## 35 is a sufficiently large enough number because
  ## results converge quite fast for BCW data set
  for (i in 1:35) {
    bcw.US$spyLabel <- predict(classifier.nb, bcw.US[, bcw.features])
    temp <- predict(classifier.nb, bcw.US[, bcw.features], type="raw")
    bcw.US$Pr  <- temp[,1]
    bcw.US$PrN <- temp[,2]

    ## build new classifier.nb
    classifier.nb <- naiveBayes(
      spyLabel ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,
      data = rbind(bcw.PS, bcw.US),
      laplace = 0)
  }

  ## Probability threshold
  index <- which(bcw.US$isSpy, TRUE)
  cnst.th <- min(bcw.US$Pr[index])

  ## RN documents <- Pr < cnst.th
  bcw.RN <- bcw.US[bcw.US$Pr < cnst.th, ]
  bcw.US <- bcw.US[bcw.US$Pr >= cnst.th, ]

  ## Convert factor to numeric for later use
  bcw.RN$spyLabel <- as.numeric(levels(bcw.RN$spyLabel))[bcw.RN$spyLabel]
  bcw.US$spyLabel <- as.numeric(levels(bcw.US$spyLabel))[bcw.US$spyLabel]


  bcw.PS$Pr <- NULL
  bcw.PS$PrN <- NULL
  bcw.PS$spyLabel <- 4
  bcw.US$Pr <- NULL
  bcw.US$PrN <- NULL
  bcw.US$spyLabel <- -1
  bcw.RN$Pr <- NULL
  bcw.RN$PrN <- NULL
  bcw.RN$spyLabel <- 2

  return (rbind(bcw.PS, bcw.US, bcw.RN))
}



################################################
####    Spy-EM
################################################
bcw.getSpyEmClassifier <- function(bcw.PS, bcw.US) {
  bcw.data <- bcw.getReliableNegativeWithSpyTechnique(bcw.PS, bcw.US)

  ## Move spy documents back to PS
  bcw.data[bcw.data$isSpy == TRUE, ]$spyLabel <- 4

  ## Split back to the sets
  bcw.PS <- bcw.data[bcw.data$spyLabel == 4, ]
  bcw.US <- bcw.data[bcw.data$spyLabel == -1, ]
  bcw.RN <- bcw.data[bcw.data$spyLabel == 2, ]

  ## Labels for next round for I-EM
  bcw.PS$Pr  <- 1
  bcw.PS$PrN <- 0
  bcw.PS$isFixed <- TRUE
  bcw.US$Pr  <- -1
  bcw.US$PrN <- -1
  bcw.US$isFixed <- FALSE
  bcw.RN$Pr  <- 0
  bcw.RN$PrN <- 1
  bcw.RN$isFixed <- FALSE

  ## Change $spyLabel to $label
  bcw.PS$label <- 4
  bcw.US$label <- -1
  bcw.RN$label <- 2

  ## Run EM algorithm to build final classifier
  classifier.nb <- naiveBayes(
    as.factor(spyLabel) ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,
    data = (rbind(bcw.PS, bcw.RN)),
    laplace = 0)

  ## Cleaning up the labels
  bcw.data <- rbind(bcw.PS, bcw.RN, bcw.US)
  bcw.data$isSpy <- NULL
  bcw.data$spyLabel <- NULL

  for (i in 1:35) {
    bcw.data$label <- predict(classifier.nb, bcw.data[, bcw.features])
    temp <- predict(classifier.nb, bcw.data[, bcw.features], type="raw")
    bcw.data$Pr  <- temp[,1]
    bcw.data$PrN <- temp[,2]

    ## PS does not change, reset it to correct values
    bcw.data[bcw.data$isFixed == TRUE, ]$label <- 4
    bcw.data[bcw.data$isFixed == TRUE, ]$Pr  <- 1
    bcw.data[bcw.data$isFixed == TRUE, ]$PrN <- 0

    ## build new classifier.nb
    classifier.nb <- naiveBayes(
      as.factor(label) ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,
      data = bcw.data,
      laplace = 0)
  }

  return (classifier.nb)
}



################################################
####    Rocchio
################################################
bcw.rocchioVectorBuilder <- function(DF1, DF2) {
  alpha <- 16
  beta <- 4

  ## Remove non-significant columns (e.g. id, class, rocLabel)
  DF1 <- DF1[ , bcw.features]
  DF2 <- DF2[ , bcw.features]

  norma.d.1 <- apply(DF1, 1, function(x){x/sqrt(sum(x^2))})
  DF1.size <- nrow(DF1)
  term.1 <- alpha * rowSums(norma.d.1)/DF1.size

  norma.d.2 <- apply(DF2, 1, function(x){x/sqrt(sum(x^2))})
  DF2.size <- nrow(DF2)
  term.2 <- beta * rowSums(norma.d.2)/DF2.size

  c <- term.1 + term.2
  return(c)
}


bcw.rocchioClassifer <- function(DF.row, vector1, vector2) {
  ## Remove non-significant columns
  DF.row <- DF.row[ , bcw.features]

  r1 <- sum(vector1 * DF.row)
  r2 <- sum(vector2 * DF.row)

  if (r1 >= r2) {
    rocLabel <- 4
  } else {
    rocLabel <- 2
  }

  return(rocLabel)
}



################################################
####    Rocchio-SVM
################################################
bcw.getReliableNegativeWithRocchio <- function(bcw.PS, bcw.US) {
  bcw.PS$rocLabel <- 4
  bcw.US$rocLabel <- 0

  ## Build Rocchio Vectors
  bcw.positiveVector <- bcw.rocchioVectorBuilder(bcw.PS, bcw.US)
  bcw.negativeVector <- bcw.rocchioVectorBuilder(bcw.US, bcw.PS)

  ## Find RN using Rocchio
  for (i in 1:nrow(bcw.US)) {
    bcw.US[i, ]$rocLabel <- bcw.rocchioClassifer(bcw.US[i, ], bcw.positiveVector, bcw.negativeVector)
  }

  ## Mark non-RN as -1, PS is already 4, US is already 2
  bcw.US[bcw.US$rocLabel == 4, ]$rocLabel <- -1

  return(rbind(bcw.PS, bcw.US))
}


bcw.getRocSvmClassifier <- function(bcw.PS, bcw.US) {

  bcw.data <- bcw.getReliableNegativeWithRocchio(bcw.PS, bcw.US)

  ## Split into the sets
  bcw.PS <- bcw.data[bcw.data$rocLabel == 4, ]
  bcw.RN <- bcw.data[bcw.data$rocLabel == 2, ]
  bcw.US <- bcw.data[bcw.data$rocLabel == -1, ]

  ## Labels for SVM
  bcw.PS$label <- 4
  bcw.RN$label <- 2
  bcw.US$label <- -1
  bcw.PS$rocLabel <- NULL
  bcw.RN$rocLabel <- NULL
  bcw.US$rocLabel <- NULL

  classifier.svm.0 <- svm(label ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,
                           data = rbind(bcw.PS, bcw.RN),
                           type = "C-classification")
  classifier.svm.i <- classifier.svm.0

  ## Count iterations
  classifier.i <- 0
  while (TRUE) {
    classifier.i <- classifier.i + 1

    ## Retrieved negatively classified documents
    bcw.US$label <- predict(classifier.svm.i, bcw.US)
    bcw.w <- bcw.US[bcw.US$label == 2, ]

    if (nrow(bcw.w) == 0) {
      break
    } else {
      bcw.US <- bcw.US[bcw.US$label == 4, ]
      bcw.RN <- rbind(bcw.RN, bcw.w)

      ## Build another model
      classifier.svm.i <- svm(label ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,
                               rbind(bcw.PS, bcw.RN),
                               type = "C-classification")
    }
  }

  ## Additional step: Use final classifier to check PS
  bcw.PS$svmlabel <- predict(classifier.svm.i, bcw.PS)
  negativeCount <- nrow(bcw.PS[bcw.PS$svmlabel == 2 , ])

  ## Selecting final classifier
  if (negativeCount / nrow(bcw.PS) > 0.05) {
    classifier.svm <- classifier.svm.0
  } else {
    classifier.svm <- classifier.svm.i
  }

  return (classifier.svm)
}



################################################
####    Rocchio-Clu-SVM
################################################
bcw.getReliableNegativeWithRocchioClustering <- function(bcw.PS, bcw.US) {
  bcw.data <- bcw.getReliableNegativeWithRocchio(bcw.PS, bcw.US)

  ## Split into the sets
  bcw.PS <- bcw.data[bcw.data$rocLabel == 4, ]
  bcw.RN <- bcw.data[bcw.data$rocLabel == 2, ]
  bcw.US <- bcw.data[bcw.data$rocLabel == -1, ]

  ## k = 10, from the paper: "choice of k does not affect
  ## classification results much as long as it is not too small"
  bcw.RN.fit <- kmeans(bcw.RN[, bcw.features], 10)
  bcw.RN$cluster <- bcw.RN.fit$cluster

  rocCluster.positiveVectors <- data.frame(
    "V1"=numeric(0), "V2"=numeric(0), "V3"=numeric(0),
    "V4"=numeric(0), "V5"=numeric(0), "V6"=numeric(0),
    "V7"=numeric(0), "V8"=numeric(0), "V9"=numeric(0))
  rocCluster.negativeVectors <- data.frame(
    "V1"=numeric(0), "V2"=numeric(0), "V3"=numeric(0),
    "V4"=numeric(0), "V5"=numeric(0), "V6"=numeric(0),
    "V7"=numeric(0), "V8"=numeric(0), "V9"=numeric(0))

  for (j in 1:10) {
    rocCluster.positiveVectors <- rbind(
                                    rocCluster.positiveVectors,
                                    bcw.rocchioVectorBuilder(
                                      bcw.PS,
                                      bcw.RN[bcw.RN$cluster == j, ]))
    rocCluster.negativeVectors <- rbind(
                                    rocCluster.negativeVectors,
                                    bcw.rocchioVectorBuilder(
                                      bcw.RN[bcw.RN$cluster == j, ],
                                      bcw.PS))
  }

  colnames(rocCluster.positiveVectors) <- bcw.features
  colnames(rocCluster.negativeVectors) <- bcw.features

  bcw.RN$rocLabel <- 0
  for (i in 1:nrow(bcw.RN)) {
    temp.row <- bcw.RN[i, ]
    temp.pSim <- numeric(0)
    temp.nSim <- numeric(0)

    for (j in 1:10) {
      temp.pSim <- c(temp.pSim, sum(rocCluster.positiveVectors[j, ] * bcw.RN[i , bcw.features]))
    }
    temp.pSim <- max(temp.pSim)

    for (j in 1:10) {
      temp.nSim <- sum(rocCluster.negativeVectors[j, ] * bcw.RN[i , bcw.features])

      if (temp.nSim > temp.pSim) {
        bcw.RN[i, ]$rocLabel <- 2
        break
      } else {
        bcw.RN[i, ]$rocLabel <- 4
      }
    }
  }

  bcw.US <- rbind(bcw.US, bcw.RN[bcw.RN$rocLabel == 4, ])
  bcw.RN <- bcw.RN[bcw.RN$rocLabel == 2, ]
  bcw.RN$cluster <- NULL

  bcw.PS$rocLabel <- 4
  bcw.US$rocLabel <- -1
  bcw.RN$rocLabel <- 2

  return(rbind(bcw.PS, bcw.RN, bcw.US))
}


bcw.getRocCluSvmClassifier <- function(bcw.PS, bcw.US) {
  bcw.data <- bcw.getReliableNegativeWithRocchioClustering(bcw.PS, bcw.US)

  bcw.data$label <- bcw.data$rocLabel
  bcw.data$rocLabel <- NULL

  bcw.PS <- bcw.data[bcw.data$label == 4, ]
  bcw.RN <- bcw.data[bcw.data$label == 2, ]
  bcw.US <- bcw.data[bcw.data$label == -1, ]

  ## Build initial classifier
  classifier.svm.0 <- svm(label ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,
                           data = rbind(bcw.PS, bcw.RN),
                           type = "C-classification")

  ## Enter loop to build classifier iteratively
  classifier.svm.i <- classifier.svm.0
  classifier.i <- 0
  while (TRUE) {
    classifier.i <- classifier.i + 1

    bcw.US$label <- predict(classifier.svm.i, bcw.US)
    bcw.w <- bcw.US[bcw.US$label == 2, ]

    if (nrow(bcw.w) == 0) {
      break
    } else {
      bcw.US <- bcw.US[bcw.US$label == 4, ]
      bcw.RN <- rbind(bcw.RN, bcw.w)

      ## Build new classifier
      classifier.svm.i <- svm(label ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,
                               data = rbind(bcw.PS, bcw.RN),
                               type = "C-classification")
    }
  }

  ## Additional step: Use final classifier to check PS
  bcw.PS$svmlabel <- predict(classifier.svm.i, bcw.PS)
  negativeCount <- nrow(bcw.PS[bcw.PS$svmlabel == 2 , ])

  ## Selecting final classifier
  if (negativeCount / nrow(bcw.PS) > 0.05) {
    classifier.svm <- classifier.svm.0
  } else {
    classifier.svm <- classifier.svm.i
  }

  return (classifier.svm)
}



################################################
####    Similary-Weight SVM
################################################
bcw.calculateSimilarityValue <- function(DF.row, prototype1) {
  DF.row <- DF.row[ , bcw.features]
  prototype1 <- prototype1[ , bcw.features]

  norma.x   <- apply(DF.row, 1, function(x){sqrt(sum(x^2))})
  norma.p.k <- apply(prototype1, 1, function(x){sqrt(sum(x^2))})

  sim <- sum((DF.row * prototype1) / (norma.x * norma.p.k))

  return(sim)
}


bcw.calculateLocalWeight <- function(bcw.PS, bcw.US, bcw.RN, bcw.pk, bcw.nk, cnst.r) {
  ## Set up Local SPUL weights
  bcw.PS$m.plus  <- 1
  bcw.PS$m.minus <- 0
  bcw.RN$m.plus  <- 0
  bcw.RN$m.minus <- 1
  bcw.US$m.plus  <- -1
  bcw.US$m.minus <- -1


  ## For each cluster in US
  for (j in 1:cnst.r) {
    cluster.j <- subset(bcw.US, cluster==j)

    cluster.size <- nrow(cluster.j)
    cluster.lp <- 0
    cluster.ln <- 0

    ## For each document in cluster
    for (i in 1:nrow(cluster.j)) {
      cluster.j.pk <- numeric(0)
      cluster.j.nk <- numeric(0)

      ## Run this document with every positive vector
      for (k in 1:nrow(bcw.pk)) {
        temp <- bcw.calculateSimilarityValue(cluster.j[i, ], bcw.pk[k, ])
        cluster.j.pk <- c(cluster.j.pk, temp)
      }

      ## Run this document with every negative vector
      for (k in 1:nrow(bcw.nk)) {
        temp <- bcw.calculateSimilarityValue(cluster.j[i, ], bcw.nk[k, ])
        cluster.j.nk <- c(cluster.j.nk, temp)
      }

      ## Count number of documents that are closer to positive/negative prototypes
      if (max(cluster.j.pk) > max(cluster.j.nk)) {
        cluster.lp <- cluster.lp + 1
      } else {
        cluster.ln <- cluster.ln + 1
      }
    }

    ## Calculate m+ for entire cluster
    bcw.US[bcw.US$cluster==j, ]$m.plus  <- cluster.lp / cluster.size

    ## Calculate m- for entire cluster
    bcw.US[bcw.US$cluster==j, ]$m.minus <- cluster.ln / cluster.size
  }

  bcw.US$cluster <- NULL
  bcw.RN$cluster <- NULL


  return (rbind(bcw.PS, bcw.RN, bcw.US))
}


bcw.calculateGlobalWeight <- function(bcw.PS, bcw.US, bcw.RN, bcw.pk, bcw.nk, cnst.r) {
  ## Set up Local SPUL weights
  bcw.PS$m.plus  <- 1
  bcw.PS$m.minus <- 0
  bcw.RN$m.plus  <- 0
  bcw.RN$m.minus <- 1
  bcw.US$m.plus  <- -1
  bcw.US$m.minus <- -1

  ## For each cluster in US
  for (j in 1:cnst.r) {
    cluster.j <- subset(bcw.US, cluster==j)

    ## For each document in cluster
    for (i in 1:nrow(cluster.j)) {
      cluster.j.pk <- numeric(0)
      cluster.j.nk <- numeric(0)

      ## Run this document with every positive vector
      for (k in 1:nrow(bcw.pk)) {
        temp <- bcw.calculateSimilarityValue(cluster.j[i, ], bcw.pk[k, ])
        cluster.j.pk <- c(cluster.j.pk, temp)
      }

      ## Run this document with every negative vector
      for (k in 1:nrow(bcw.nk)) {
        temp <- bcw.calculateSimilarityValue(cluster.j[i, ], bcw.nk[k, ])
        cluster.j.nk <- c(cluster.j.nk, temp)
      }

      ## Calculate m+ for this document, save in globalSPUL
      temp <- sum(cluster.j.pk) / (sum(cluster.j.pk) + sum(cluster.j.nk))
      bcw.US[bcw.US$id == cluster.j[i, ]$id, ]$m.plus <- temp

      ## Calculate m- for this document, save in globalSPUL
      temp <- sum(cluster.j.nk) / (sum(cluster.j.pk) + sum(cluster.j.nk))
      bcw.US[bcw.US$id == cluster.j[i, ]$id, ]$m.minus <- temp
    }
  }

  bcw.US$cluster <- NULL
  bcw.RN$cluster <- NULL
  return(rbind(bcw.PS, bcw.RN, bcw.US))
}


bcw.buildCustomWeightedSvm <- function() {
  fn1 <- function(Avector) {
    ## optimization function f(x)
    term.2 <- combn(1:dataRow, 2, function(x) {
      Avector[x[1]] * Avector[x[2]] * Yvector[x[1]] * Yvector[x[2]] * (Xdata[x[1], ] %*% t(Xdata[x[2], ]))
    })
    return(0.5 * sum(term.2) - sum(Avector))
  }
  
  eqfn1 <- function(Avector) {
    ## equal constraint function g(x)
    sum(Avector[index]) - sum(Avector[-index])
  }
  
  ineqfn1 <- function(Avector) {
    ## inequal constraint function h(x)
    Avector
  }
}


bcw.getSimilaryWeightSvmClassifier <- function(bcw.PS, bcw.US, isGlobal) {

  bcw.data.spy <- bcw.getReliableNegativeWithSpyTechnique(bcw.PS, bcw.US)
  bcw.data.roc <- bcw.getReliableNegativeWithRocchio(bcw.PS, bcw.US)

  bcw.data <-merge(
                bcw.data.spy[, c("id", bcw.features, "class", "spyLabel")],
                bcw.data.roc[, c("id", "rocLabel")],
                by="id")

  ## Extract RN
  bcw.RN <- c()
  for (i in 1:nrow(bcw.data)) {
    if ((bcw.data[i, ]$spyLabel == 2) && (bcw.data[i, ]$rocLabel == 2)) {
      bcw.RN <- rbind(bcw.RN, bcw.data[i, ])
    }
  }

  ## US documents are all documents that are !RN and !PS
  bcw.US <- subset(bcw.data, !(bcw.data$id %in% bcw.RN$id) & !(bcw.data$id %in% bcw.PS$id))

  ## Clean up labels
  bcw.PS$spyLabel <- NULL
  bcw.PS$rocLabel <- NULL
  bcw.US$spyLabel <- NULL
  bcw.US$rocLabel <- NULL
  bcw.RN$spyLabel <- NULL
  bcw.RN$rocLabel <- NULL


  ################################################
  ## Clustering
  cnst.t = 30
  cnst.m = floor(cnst.t * nrow(bcw.RN) / (nrow(bcw.US) + nrow(bcw.RN)))
  bcw.RN.fit <- kmeans(bcw.RN[, bcw.features], cnst.m)

  ## Label data with cluster number
  bcw.RN <- data.frame(bcw.RN, bcw.RN.fit$cluster)
  bcw.RN <- rename(bcw.RN, c("bcw.RN.fit.cluster" = "cluster"))


  ################################################
  ## Build representative prototypes
  temp <- matrix(0, ncol = 10, nrow = 0)
  bcw.pk <- data.frame(temp)
  bcw.nk <- data.frame(temp)

  for (k in 1:cnst.m) {
    cluster.k <- subset(bcw.RN, cluster == k)

    pk <- bcw.rocchioVectorBuilder(bcw.PS, cluster.k)
    pk <- c(k, pk)
    bcw.pk <- rbind(bcw.pk, pk)

    nk <- bcw.rocchioVectorBuilder(cluster.k, bcw.PS)
    nk <- c(k, nk)
    bcw.nk <- rbind(bcw.nk, nk)
  }

  names(bcw.pk) <- c("k", bcw.features)
  names(bcw.nk) <- c("k", bcw.features)


  ################################################
  ####    Similarity Weight Generation
  ## Take value of 't' from above, t <- 30
  cnst.r = floor(cnst.t * nrow(bcw.US) / (nrow(bcw.RN) + nrow(bcw.US)))
  bcw.US.fit <- kmeans(bcw.US[, bcw.features], cnst.r)

  ## Label data with cluster number
  bcw.US <- data.frame(bcw.US, bcw.US.fit$cluster)
  bcw.US <- rename(bcw.US, c("bcw.US.fit.cluster" = "cluster"))

  if (isGlobal) {
    bcw.data <- bcw.calculateGlobalWeight(bcw.PS, bcw.US, bcw.RN, bcw.pk, bcw.nk, cnst.r)
  } else {
    bcw.data <- bcw.calculateLocalWeight(bcw.PS, bcw.US, bcw.RN, bcw.pk, bcw.nk, cnst.r)
  }
  
  ## Re-label the sets
  bcw.PS <- bcw.data[bcw.data$m.plus == 1, ]
  bcw.RN <- bcw.data[bcw.data$m.minus == 1, ]
  bcw.US <- bcw.data[bcw.data$m.plus != 1 & bcw.data$m.minus != 1, ]
  bcw.PS$svmLabel <- 1
  bcw.RN$svmLabel <- -1
  bcw.US$svmLabel <- 0
  
  bcw.s.plus  <- rbind(bcw.PS, bcw.US)
  bcw.s.minus <- rbind(bcw.US, bcw.RN)
  bcw.s.star  <- rbind(bcw.PS, bcw.RN)
  
  dataRow <- nrow(bcw.s.star[1:40, ])
  dataCol <- length(bcw.features)
  Xdata   <- as.matrix(bcw.s.star[1:40, bcw.features])
  Yvector <- bcw.s.star[1:40, ]$svmLabel # y values
  pSize <- nrow(bcw.PS) # positive set size
  Cp <- 2 # upper bound for positive set
  Cn <- 1 # upper bound for negative set
  Idx <- sample(1:dataRow, pSize)
  Slabel <- rep(0, dataRow)
  Slabel[Idx] <- 1 # positive
  Slabel[-Idx] <- -1 # negative
  
  fn1 <- function(Avector) {
    print(Avector)
    term.2 <- combn(1:dataRow, 2, function(x) {
      Avector[x[1]] * Avector[x[2]] * Yvector[x[1]] * Yvector[x[2]] * (Xdata[x[1], ] %*% t(Xdata[x[2], ]))
    })
    return(0.5 * sum(term.2) - sum(Avector))
  }
  # equal constraint function g(x)
  eqfn1 <- function(Avector) {
    sum(Avector[Idx]) - sum(Avector[-Idx])
  }
  # inequal constraint function h(x)
  ineqfn1 <- function(Avector) {
    Avector
  }
  # inequal constraint function lower bound and upper bound
  ineqLB1 <- rep(0, dataRow)
  ineqUB1 <- sapply(1:dataRow, function (x) {ifelse(Slabel[x]==1, b <- Cp, b <- Cn)})
  
  x0 <- rep(0, dataRow)
  
  # apply the function
  a <- solnp(x0, fun=fn1, eqfun=eqfn1, ineqfun=ineqfn1, ineqLB=ineqLB1, ineqUB=ineqUB1) 
  
  ## solnp will take 1-3 hours. 
  ## beepr will alert you when it is done
  ## make sure your sound is on!
  beepr::beep(8)
  
  
  
  
  
  
  
  
  
}



################################################
####    Similary-Weight SVM
################################################
bcw.getReliableNegativeWithLELC <- function(bcw.PS, bcw.US) {
  bcw.data.spy <- bcw.getReliableNegativeWithSpyTechnique(bcw.PS, bcw.US)
  bcw.data.roc <- bcw.getReliableNegativeWithRocchio(bcw.PS, bcw.US)
  
  bcw.data <-merge(
    bcw.data.spy[, c("id", bcw.features, "class", "spyLabel")],
    bcw.data.roc[, c("id", "rocLabel")],
    by="id")
  
  ## Extract RN
  bcw.RN <- c()
  for (i in 1:nrow(bcw.data)) {
    if ((bcw.data[i, ]$spyLabel == 2) && (bcw.data[i, ]$rocLabel == 2)) {
      bcw.RN <- rbind(bcw.RN, bcw.data[i, ])
    }
  }
  
  ## US documents are all documents that are !RN and !PS
  bcw.US <- subset(bcw.data, !(bcw.data$id %in% bcw.RN$id) & !(bcw.data$id %in% bcw.PS$id))
  
  ## Clean up labels
  bcw.PS$spyLabel <- NULL
  bcw.PS$rocLabel <- NULL
  bcw.US$spyLabel <- NULL
  bcw.US$rocLabel <- NULL
  bcw.RN$spyLabel <- NULL
  bcw.RN$rocLabel <- NULL
  
  
  ####
  ## Skip step on tf-idf on BCW data
  ####
  
  
  ## The choice of k does not affect classification results if it is not too small or big
  ## I took the value of k frmo Similarity Based PU Learning Algorithm
  cnst.k = 30 
  cnst.r = floor(nrow(bcw.RN) * cnst.k / (nrow(bcw.PS) + nrow(bcw.RN) + nrow(bcw.US)))
  
  ## Cluster RN
  bcw.RN.fit <- kmeans(bcw.RN[, bcw.features], cnst.r)
  bcw.RN <- data.frame(bcw.RN, bcw.RN.fit$cluster)
  bcw.RN <- rename(bcw.RN, c("bcw.RN.fit.cluster" = "cluster"))
  
  ################################################
  ## Build representative prototypes
  temp <- matrix(0, ncol = 10, nrow = 0)
  bcw.pk <- data.frame(temp)
  bcw.nk <- data.frame(temp)
  
  for (r in 1:cnst.r) {
    cluster.r <- subset(bcw.RN, cluster == r)
    
    pk <- bcw.rocchioVectorBuilder(bcw.PS, cluster.r)
    pk <- c(r, pk)
    bcw.pk <- rbind(bcw.pk, pk)
    
    nk <- bcw.rocchioVectorBuilder(cluster.r, bcw.PS)
    nk <- c(r, nk)
    bcw.nk <- rbind(bcw.nk, nk)
  }
  
  names(bcw.pk) <- c("k", bcw.features)
  names(bcw.nk) <- c("k", bcw.features)
  
  
  ## Cluster US
  cnst.n = floor(nrow(bcw.US) * cnst.k / (nrow(bcw.PS) + nrow(bcw.RN) + nrow(bcw.US)))
  bcw.US.fit <- kmeans(bcw.US[, bcw.features], cnst.n)
  bcw.US <- data.frame(bcw.US, bcw.US.fit$cluster)
  bcw.US <- rename(bcw.US, c("bcw.US.fit.cluster" = "cluster"))
  
  
  bcw.LP <- c()   # Likely positive
  bcw.LN <- c()   # Likely negative
  ## For each cluster in US
  for (n in 1:cnst.n) {
    cluster.n <- subset(bcw.US, cluster == n)
    vote.positive <- 0
    vote.negative <- 0
    
    for (m in 1:nrow(cluster.n)) {
      cluster.n.pk <- numeric(0)
      cluster.n.nk <- numeric(0)
      
      ## Run this document with every positive vector
      for (k in 1:nrow(bcw.pk)) {
        temp <- bcw.calculateSimilarityValue(cluster.n[m, ], bcw.pk[k, ])
        cluster.n.pk <- c(cluster.n.pk, temp)
      }
      
      ## Run this document with every negative vector
      for (k in 1:nrow(bcw.nk)) {
        temp <- bcw.calculateSimilarityValue(cluster.n[m, ], bcw.nk[k, ])
        cluster.n.nk <- c(cluster.n.nk, temp)
      }
      
      if (max(cluster.n.pk) > max(cluster.n.nk)) {
        vote.positive <- vote.positive + 1
      } else {
        vote.negative <- vote.negative + 1
      }
    }
    
    if (vote.positive > vote.negative) {
      bcw.LP <- rbind(bcw.LP, cluster.n)
    } else {
      bcw.LN <- rbind(bcw.LN, cluster.n)
    }
  }
  
  bcw.LP$cluster <- NULL
  bcw.LN$cluster <- NULL
  bcw.RN$cluster <- NULL
  
  bcw.PS <- rbind(bcw.PS, bcw.LP) 
  bcw.RN <- rbind(bcw.RN, bcw.LN) 
  
  bcw.PS$lelcLabel <- 4
  bcw.RN$lelcLabel <- 2
  
  return(rbind(bcw.PS, bcw.RN))
}


bcw.getLelcClassifier <- function(bcw.PS, bcw.US) {
  
  bcw.data <- bcw.getReliableNegativeWithLELC(bcw.PS, bcw.US)
  
  bcw.PS <- bcw.data[bcw.data$lelcLabel == 4, ]
  bcw.RN <- bcw.data[bcw.data$lelcLabel == 2, ]
  bcw.PS$label <- 4
  bcw.RN$label <- 2
  bcw.PS$lelcLabel <- NULL
  bcw.RN$lelcLabel <- NULL
  
  classifier.svm <- svm(label ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,
                        data = rbind(bcw.PS, bcw.RN),
                        type = "C-classification")
  
  return (classifier.svm)
}



################################################
####    F-measure
################################################
## True positives (TP) - Positive labeled correctly
## True negatives (TN) - Negative labeled correctly
## False positives (FP) - Positive labeled wrongly
## False negatives (FN) - Negative labeled wrongly
bcw.calculateFMeasure <- function(bcw.data) {
  TP <- nrow(bcw.data[(bcw.data$class == 4 & bcw.data$predict == 4), ])
  TN <- nrow(bcw.data[(bcw.data$class == 2 & bcw.data$predict == 2), ])
  FP <- nrow(bcw.data[(bcw.data$class == 2 & bcw.data$predict == 4), ])
  FN <- nrow(bcw.data[(bcw.data$class == 4 & bcw.data$predict == 2), ])

  Precision <- TP / (TP+FP)
  Recall    <- TP / (TP+FN)

  return ((2 * Precision * Recall) / (Precision + Recall))
}


bcw.calculateAccuracy <- function(bcw.data) {
  TP <- nrow(bcw.data[(bcw.data$class == 4 & bcw.data$predict == 4), ])
  TN <- nrow(bcw.data[(bcw.data$class == 2 & bcw.data$predict == 2), ])
  FP <- nrow(bcw.data[(bcw.data$class == 2 & bcw.data$predict == 4), ])
  FN <- nrow(bcw.data[(bcw.data$class == 4 & bcw.data$predict == 2), ])

  Accuracy <- (TP+TN) / (TP+FP+TN+FN)
  return (Accuracy)
}











################################################
####    Loading the data
################################################
bcw <- read.table("breast-cancer-wisconsin.data", sep=",")
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





################################################
################################################
################################################
####    Start testing here

## Set up data storage for F-measure and Accuracy
f.NB <- numeric(0)
f.SEM <- numeric(0)
f.RocSVM <- numeric(0)
f.RocCluSVM <- numeric(0)
f.LELC <- numeric(0)
f.globalSPUL <- numeric(0)
f.localSPUL <- numeric(0)
a.NB <- numeric(0)
a.SEM <- numeric(0)
a.RocSVM <- numeric(0)
a.RocCluSVM <- numeric(0)
a.LELC <- numeric(0)
a.globalSPUL <- numeric(0)
a.localSPUL <- numeric(0)

trnPercent <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.45, 0.55, 0.65)

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
    classifier.naiveBayes <- bcw.getNaiveBayesClassifier(bcw.PS, bcw.US)
    classifier.spyEm <- bcw.getSpyEmClassifier(bcw.PS, bcw.US)
    classifier.rocchioSvm <- bcw.getRocSvmClassifier(bcw.PS, bcw.US)
    classifier.rocchioCluSvm <- bcw.getRocCluSvmClassifier(bcw.PS, bcw.US)
    classifier.lelc <- bcw.getLelcClassifier(bcw.PS, bcw.US)
    
    
    
    ################################################
    ## Run the classifers on test data
    bcw.tst.NB <- bcw.tst
    bcw.tst.NB$predict <- predict(classifier.naiveBayes, bcw.tst[, bcw.features])
    
    bcw.tst.SEM <- bcw.tst
    bcw.tst.SEM$predict <- predict(classifier.spyEm, bcw.tst[, bcw.features])
    
    bcw.tst.RocSVM <- bcw.tst
    bcw.tst.RocSVM$predict <- predict(classifier.rocchioSvm, bcw.tst[, bcw.features])
    
    bcw.tst.RocCluSVM <- bcw.tst
    bcw.tst.RocCluSVM$predict <- predict(classifier.rocchioCluSvm, bcw.tst[, bcw.features])
    
    bcw.tst.LELC <- bcw.tst
    bcw.tst.LELC$predict <- predict(classifier.lelc, bcw.tst[, bcw.features])
    
    
    ################################################
    ## Calculating performance
    
    ## Calculate F-measure+Accuracy for each fold
    bcw.tst.NB.folds.f <- numeric(0)
    bcw.tst.NB.folds.a <- numeric(0)
    
    bcw.tst.SEM.folds.f <- numeric(0)
    bcw.tst.SEM.folds.a <- numeric(0)
    
    bcw.tst.RocSVM.folds.f <- numeric(0)
    bcw.tst.RocSVM.folds.a <- numeric(0)
    
    bcw.tst.RocCluSVM.folds.f <- numeric(0)
    bcw.tst.RocCluSVM.folds.a <- numeric(0)
    
    bcw.tst.LELC.folds.f <- numeric(0)
    bcw.tst.LELC.folds.a <- numeric(0)
    
    
    for (i in 1:10) {
      bcw.tst.NB.folds.f <- c(bcw.tst.NB.folds.f, bcw.calculateFMeasure(bcw.tst.NB[bcw.tst.NB$fold == i, ]))
      bcw.tst.NB.folds.a <- c(bcw.tst.NB.folds.a, bcw.calculateAccuracy(bcw.tst.NB[bcw.tst.NB$fold == i, ]))
      
      bcw.tst.SEM.folds.f <- c(bcw.tst.SEM.folds.f, bcw.calculateFMeasure(bcw.tst.SEM[bcw.tst.SEM$fold == i, ]))
      bcw.tst.SEM.folds.a <- c(bcw.tst.SEM.folds.a, bcw.calculateAccuracy(bcw.tst.SEM[bcw.tst.SEM$fold == i, ]))
      
      bcw.tst.RocSVM.folds.f <- c(bcw.tst.RocSVM.folds.f, bcw.calculateFMeasure(bcw.tst.RocSVM[bcw.tst.NB$fold == i, ]))
      bcw.tst.RocSVM.folds.a <- c(bcw.tst.RocSVM.folds.a, bcw.calculateAccuracy(bcw.tst.RocSVM[bcw.tst.NB$fold == i, ]))
      
      bcw.tst.RocCluSVM.folds.f <- c(bcw.tst.RocCluSVM.folds.f, bcw.calculateFMeasure(bcw.tst.RocCluSVM[bcw.tst.RocCluSVM$fold == i, ]))
      bcw.tst.RocCluSVM.folds.a <- c(bcw.tst.RocCluSVM.folds.a, bcw.calculateAccuracy(bcw.tst.RocCluSVM[bcw.tst.RocCluSVM$fold == i, ]))
      
      bcw.tst.LELC.folds.f <- c(bcw.tst.LELC.folds.f, bcw.calculateFMeasure(bcw.tst.LELC[bcw.tst.LELC$fold == i, ]))
      bcw.tst.LELC.folds.a <- c(bcw.tst.LELC.folds.a, bcw.calculateAccuracy(bcw.tst.LELC[bcw.tst.LELC$fold == i, ]))
    }
    f.NB.row <- c(f.NB.row, mean(bcw.tst.NB.folds.f))
    a.NB.row <- c(a.NB.row, mean(bcw.tst.NB.folds.a))
    
    f.SEM.row <- c(f.SEM.row, mean(bcw.tst.SEM.folds.f))
    a.SEM.row <- c(a.SEM.row, mean(bcw.tst.SEM.folds.a))
    
    f.RocSVM.row <- c(f.RocSVM.row, mean(bcw.tst.RocSVM.folds.f))
    a.RocSVM.row <- c(a.RocSVM.row, mean(bcw.tst.RocSVM.folds.a))
    
    f.RocCluSVM.row <- c(f.RocCluSVM.row, mean(bcw.tst.RocCluSVM.folds.f))
    a.RocCluSVM.row <- c(a.RocCluSVM.row, mean(bcw.tst.RocCluSVM.folds.a))
    
    f.LELC.row <- c(f.LELC.row, mean(bcw.tst.LELC.folds.f))
    a.LELC.row <- c(a.LELC.row, mean(bcw.tst.LELC.folds.a))
  }
  
  f.NB <- rbind(f.NB, f.NB.row)
  a.NB <- rbind(a.NB, a.NB.row)
  
  f.SEM <- rbind(f.SEM, f.SEM.row)
  a.SEM <- rbind(a.SEM, a.SEM.row)
  
  f.RocSVM <- rbind(f.RocSVM, f.RocSVM.row)
  a.RocSVM <- rbind(a.RocSVM, a.RocSVM.row)
  
  f.RocCluSVM <- rbind(f.RocCluSVM, f.RocCluSVM.row)
  a.RocCluSVM <- rbind(a.RocCluSVM, a.RocCluSVM.row)
  
  f.LELC <- rbind(f.LELC, f.LELC.row)
  a.LELC <- rbind(a.LELC, a.LELC.row)
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
RocSVM.f <- shiftRownameThenMean(f.RocSVM)
RocCluSVM.f <- shiftRownameThenMean(f.RocCluSVM)
LELC.f <- shiftRownameThenMean(f.LELC)
results.f.raw <- rbind(f.NB, f.SEM, f.RocSVM, f.RocCluSVM, f.LELC)
results.f <- cbind(NB.f, SEM.f, RocSVM.f, RocCluSVM.f, LELC.f)

NB.a <- shiftRownameThenMean(a.NB)
SEM.a <- shiftRownameThenMean(a.SEM)
RocSVM.a <- shiftRownameThenMean(a.RocSVM)
RocCluSVM.a <- shiftRownameThenMean(a.RocCluSVM)
LELC.a <- shiftRownameThenMean(a.LELC)
results.a.raw <- rbind(a.NB, a.SEM, a.RocSVM, a.RocCluSVM, a.LELC)
results.a <- cbind(NB.a, SEM.a, RocSVM.a, RocCluSVM.a, LELC.a)



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

