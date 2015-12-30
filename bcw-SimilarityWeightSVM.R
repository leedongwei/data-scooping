################################################
####
####    Code does not work
####
################################################
library("Rsolnp")
source("bcw-NaiveBayes.R")
source("bcw-RocchioSVM.R")


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
