source("bcw-RocchioSVM.R")


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
