library("e1071")
source("bcw-Rocchio.R")


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
