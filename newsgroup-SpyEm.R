source("newsgroup-utils-functions.R")


ngp.model.Spy_EM <- function (ngp.trnMatrix, ngp.class) {

  PS <- rownames(ngp.class[ngp.class$label == 1, ])
  US <- rownames(ngp.class[ngp.class$label == -1, ])

  ## default % of spies document is 15% of PS
  var.spy <- 0.15

  ## Mark out spies
  temp <- sample.int(length(PS), size=ceiling(length(PS) * var.spy), replace=FALSE)
  US.spies <- PS[temp]
  ngp.class$isSpy <- FALSE
  ngp.class[US.spies, ]$isSpy <- TRUE

  ## Set spies as negative with the rest of US
  ngp.class[US.spies, ]$label <- -1



  RN <- ngp.model.Spy_EM.FindRN(ngp.trnMatrix, ngp.class)
  ngp.class$RN <- 0
  ngp.class[RN, ]$RN <- -1

  model.Spy_EM <- ngp.model.Spy_EM.BuildModel(ngp.trnMatrix, ngp.class)

  return(model.Spy_EM)
}

ngp.model.Spy_EM.FindRN <- function (ngp.trnMatrix, ngp.class) {

  PS <- rownames(ngp.class[ngp.class$label == 1, ])
  US <- rownames(ngp.class[ngp.class$label == -1, ])
  US.spies <- rownames(ngp.class[ngp.class$isSpy == TRUE, ])

  ## Check that matrix and class are sorted
  if (all(rownames(ngp.trnMatrix) != rownames(ngp.class))) {
    ngp.trnMatrix <- ngp.trnMatrix[order(rownames(ngp.trnMatrix)), ]
    ngp.class     <- ngp.class[order(rownames(ngp.class)), ]
  }
  PS <- PS[order(PS)]
  US <- US[order(US)]

  ## Set up for Naive-Bayes iterations
  ngp.class$predict <- ngp.class$label
  if (!is.factor(ngp.class$predict)) {
    ngp.class$predict <- as.factor(ngp.class$predict)
  }

  ##############################################
  ####    Naive-Bayes iterations
  nBayes.model <- naiveBayes(ngp.trnMatrix, ngp.class$predict, laplace=0.15)
  US.predict <- predict(nBayes.model, ngp.trnMatrix[US, ])
  var.iter <- 1
  var.iter.values <- c(0, 0, 0)

  while (var.iter < 25) {
    utils.cat(paste("        Spy_EM.FindRN, iteration #", var.iter, "\n", sep=""))

    ngp.class[US, ]$predict <- US.predict

    nBayes.model <- naiveBayes(ngp.trnMatrix, ngp.class$predict, laplace=0.1)
    US.predict <- predict(nBayes.model, ngp.trnMatrix[US, ])
    var.iter <- var.iter + 1

    ## Stopping condition
    if (identical(ngp.class[US, ]$predict, US.predict)) {
      break
    } else {
      # print(which(ngp.class[US, ]$predict != US.predict))
      var.iter.values[3] <- var.iter.values[2]
      var.iter.values[2] <- var.iter.values[1]
      var.iter.values[1] <- length(which(ngp.class[US, ]$predict != US.predict))

      ## If the number of different predictions between last 3 iterations is less than 10%, we can assume that it has converged
      # print(var.iter.values)
      var.maxDiff <- ceiling(max(var.iter.values) / 10)
      if (!(0 %in% var.iter.values)
          & (var.iter.values[3] - var.iter.values[2] < var.maxDiff)
          & (var.iter.values[2] - var.iter.values[1] < var.maxDiff)) {
        break
      }
    }
  }
  utils.cat(paste("        Spy_EM.FindRN, break at #", var.iter, "\n", sep=""))

  ## Run final mode on PS and US to get Probability
  spies.predict <- predict(nBayes.model, ngp.trnMatrix[US.spies, ], type='raw')
  US.predict <- predict(nBayes.model, ngp.trnMatrix[US, ], type='raw')

  ## Extract RN and mark it in ngp.class
  ## TODO: Find out why threshold is zero
  # threshold <- min(spies.predict[which(spies.predict[, "1"] != 0), "1"])
  threshold <- min(spies.predict[, "1"])

  temp <- which(US.predict[, "1"] < threshold | US.predict[, "1"] == 0)
  RN <- rownames((ngp.class[US, ])[temp, ])

  return(RN)
}


ngp.model.Spy_EM.BuildModel <- function (ngp.trnMatrix, ngp.class) {

  ## Move spies back into PS
  US.spies <- rownames(ngp.class[ngp.class$isSpy == TRUE, ])
  ngp.class[US.spies, ]$label <- 1
  PS <- rownames(ngp.class[ngp.class$label == 1, ])
  US <- rownames(ngp.class[ngp.class$label == -1, ])
  RN <- rownames(ngp.class[ngp.class$label == -1 & ngp.class$RN == -1, ])

  ## QS = US = RN
  QS <- US [! US %in% RN]

  ## Set for iterations
  ngp.class$predict <- 0
  ngp.class[PS, ]$predict <- 1
  ngp.class[RN, ]$predict <- -1
  ngp.class$predict <- as.factor(ngp.class$predict)

  ## First run of Naive Bayes
  nBayes.model <- naiveBayes(
    ngp.trnMatrix[c(PS, RN), ],
    ngp.class[c(PS, RN), ]$predict,
    laplace=0.1)
  QS.predict <- predict(nBayes.model, ngp.trnMatrix[QS, ])
  var.iter <- 1
  var.iter.values <- c(0, 0, 0)

  while(var.iter < 25) {
    utils.cat(paste("        Spy_EM.BuildModel, iteration #", var.iter, "n", sep=""))

    ngp.class[QS, ]$predict <- QS.predict

    nBayes.model <- naiveBayes(ngp.trnMatrix, ngp.class$predict, laplace=0.1)
    QS.predict <- predict(nBayes.model, ngp.trnMatrix[QS, ])

    ## Stopping condition
    if (identical(ngp.class[US, ]$predict, US.predict)) {
      break
    } else {
      # print(which(ngp.class[US, ]$predict != US.predict))
      var.iter.values[3] <- var.iter.values[2]
      var.iter.values[2] <- var.iter.values[1]
      var.iter.values[1] <- length(which(ngp.class[US, ]$predict != US.predict))

      ## If the number of different predictions between last 3 iterations is less than 10%, we can assume that it has converged
      # print(var.iter.values)
      var.maxDiff <- ceiling(max(var.iter.values) / 10)
      if (!(0 %in% var.iter.values)
          & (var.iter.values[3] - var.iter.values[2] < var.maxDiff)
          & (var.iter.values[2] - var.iter.values[1] < var.maxDiff)) {
        break
      }
    }
  }
  utils.cat(paste("        Spy_EM.BuildModel, break at #", var.iter, "n", sep=""))
  return(nBayes.model)
}