source("bcw-SpyEM.R")
source("bcw-RocchioSVM.R")


bcw.calculateSimilarityValue <- function(DF.row, prototype1) {
  DF.row <- DF.row[ , bcw.features]
  prototype1 <- prototype1[ , bcw.features]

  norma.x   <- apply(DF.row, 1, function(x){sqrt(sum(x^2))})
  norma.p.k <- apply(prototype1, 1, function(x){sqrt(sum(x^2))})

  sim <- sum((DF.row * prototype1) / (norma.x * norma.p.k))

  return(sim)
}

bcw.getReliableNegativeWithLELC <- function(bcw.PS, bcw.US) {
  bcw.data.spy <- bcw.getReliableNegativeWithSpyTechnique(bcw.PS, bcw.US)
  bcw.data.roc <- bcw.getReliableNegativeWithRocchio(bcw.PS, bcw.US)

  bcw.data <- merge(
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
