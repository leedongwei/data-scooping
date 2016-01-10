library("e1071")


bcw.getReliableNegativeWithSpyTechnique <- function(bcw.PS, bcw.US) {

  ## Label data with spy-tags
  bcw.PS$isSpy <- FALSE
  bcw.US$isSpy <- FALSE

  ## Value of s is taken from page 3 of Liu, Dai, Li, Lee, Yu (2003)
  cnst.s <- 0.15
  temp <- sample(nrow(bcw.PS), ceiling(cnst.s * nrow(bcw.PS)))

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
    laplace = 0.1)

  ## Loop till predicted results converge
  ## 35 is a sufficiently large enough number because
  ## results converge quite fast for BCW data set
  for (i in 1:35) {
    bcw.US$spyLabel <- predict(classifier.nb, bcw.US[, bcw.features])

    ## build new classifier.nb
    if (i < 35) {
      classifier.nb <- naiveBayes(
        spyLabel ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,
        data = rbind(bcw.PS, bcw.US),
        laplace = 0.1)
    } else {
      temp <- predict(classifier.nb, bcw.US[, bcw.features], type="raw")
      bcw.US$Pr  <- temp[,1]
      bcw.US$PrN <- temp[,2]
    }
  }

  ## Probability threshold
  ## TODO: Account for noise when using text documents
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
    laplace = 0.1)

  ## Cleaning up the labels
  bcw.data <- rbind(bcw.PS, bcw.RN, bcw.US)
  bcw.data$isSpy <- NULL
  bcw.data$spyLabel <- NULL

  for (i in 1:35) {
    bcw.data$label <- predict(classifier.nb, bcw.data[, bcw.features])
#     temp <- predict(classifier.nb, bcw.data[, bcw.features], type="raw")
#     bcw.data$Pr  <- temp[,1]
#     bcw.data$PrN <- temp[,2]

    ## PS does not change, reset it to correct values
    bcw.data[bcw.data$isFixed == TRUE, ]$label <- 4
#     bcw.data[bcw.data$isFixed == TRUE, ]$Pr  <- 1
#     bcw.data[bcw.data$isFixed == TRUE, ]$PrN <- 0

    ## build new classifier.nb
    classifier.nb <- naiveBayes(
      as.factor(label) ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,
      data = bcw.data,
      laplace = 0.1)
  }

  return (classifier.nb)
}
