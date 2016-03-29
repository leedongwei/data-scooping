source("newsgroup-utils-functions.R")


ngp.model.Spy_EM <- function (ngp.dtm, ngp.class) {

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



  RN <- ngp.model.Spy_EM.FindRN(ngp.dtm, ngp.class)
  utils.cat(paste("        Spy_EM.FindRN:  thread #", var.j, ", 6.1\n", sep=""))
  ngp.class$RN <- 0
  if (length(RN) > 0) {
    ngp.class[RN, ]$RN <- -1
  }
  
  utils.cat(paste("        Spy_EM.FindRN:  thread #", var.j, ", 6.2\n", sep=""))
  model.Spy_EM <- ngp.model.Spy_EM.BuildModel(ngp.dtm, ngp.class)

  return(model.Spy_EM)
}

ngp.model.Spy_EM.FindRN <- function (ngp.dtm, ngp.class) {

  PS <- rownames(ngp.class[ngp.class$label == 1, ])
  US <- rownames(ngp.class[ngp.class$label == -1, ])
  US.spies <- rownames(ngp.class[ngp.class$isSpy == TRUE, ])

  ## Check that matrix and class are sorted
  if (all(rownames(ngp.dtm) != rownames(ngp.class))) {
    ngp.dtm <- ngp.dtm[order(rownames(ngp.dtm)), ]
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
  nBayes.model <- naiveBayes(ngp.dtm, ngp.class$predict, laplace=0.1)
  US.predict <- predict(nBayes.model, ngp.dtm[US, ])
  var.iter <- 1
  var.iter.values <- c(0, 0, 0)

  while (var.iter < 25) {
    utils.cat(paste("        Spy_EM.FindRN thread #", var.j, ", iteration #", var.iter, "\n", sep=""))

    ngp.class[US, ]$predict <- US.predict

    nBayes.model <- naiveBayes(ngp.dtm, ngp.class$predict, laplace=0.1)
    US.predict <- predict(nBayes.model, ngp.dtm[US, ])
    var.iter <- var.iter + 1
    
    utils.cat(paste("        Spy_EM.FindRN:  thread #", var.j, ", 5  US-neg:", length(US.predict[-1]), "\n", sep=""))
    
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
      var.maxDiff <- ceiling(min(var.iter.values) / 10)
      if (!(0 %in% var.iter.values)
          & (var.iter.values[3] - var.iter.values[2] < var.maxDiff)
          & (var.iter.values[2] - var.iter.values[1] < var.maxDiff)) {
        break
      }
    }
  }
  utils.cat(paste("        Spy_EM.FindRN:  thread #", var.j, ", break at #", var.iter, "\n", sep=""))

  ## Run final mode on PS and US to get Probability
  spies.predict <- predict(nBayes.model, ngp.dtm[US.spies, ], type='raw')
  utils.cat(paste("        Spy_EM.FindRN:  thread #", var.j, ", 1\n", sep=""))
  US.predict <- predict(nBayes.model, ngp.dtm[US, ], type='raw')
  utils.cat(paste("        Spy_EM.FindRN:  thread #", var.j, ", 2\n", sep=""))

  ## Extract RN and mark it in ngp.class
  ## TODO: Find out why threshold is zero
  # threshold <- min(spies.predict[which(spies.predict[, "1"] != 0), "1"])
  threshold <- min(spies.predict[, "1"])
  utils.cat(paste("        Spy_EM.FindRN:  thread #", var.j, ", 3\n", sep=""))

  ##  DONGWEI: subscript out of bounds
  ## RN had 1091 docs, vs total US of 1090
  temp <- which(US.predict[, "1"] < threshold | US.predict[, "1"] == 0)
  utils.cat(paste("        Spy_EM.FindRN:  thread #", var.j, ", 4\n", sep=""))
  RN <- rownames((ngp.class[US, ])[temp, ])
  utils.cat(paste("        Spy_EM.FindRN:  thread #", var.j, ", 5  RN:", length(RN), "\n", sep=""))

  return(RN)
}


ngp.model.Spy_EM.BuildModel <- function (ngp.dtm, ngp.class) {

  ## Move spies back into PS
  utils.cat(paste("        Spy_EM.BuildModel:  thread #", var.j, ", 7\n", sep=""))
  US.spies <- rownames(ngp.class[ngp.class$isSpy == TRUE, ])
  ngp.class[US.spies, ]$label <- 1
  PS <- rownames(ngp.class[ngp.class$label == 1, ])
  US <- rownames(ngp.class[ngp.class$label == -1, ])
  RN <- rownames(ngp.class[ngp.class$label == -1 & ngp.class$RN == -1, ])
  
  utils.cat(paste("        Spy_EM.BuildModel:  thread #", var.j, ", 8\n", sep=""))
  ## QS = US - RN
  if (length(RN) > 0) {
    QS <- US [! US %in% RN]
  } else {
    QS <- US
  }
  utils.cat(paste("        Spy_EM.BuildModel:  thread #", var.j, ", 9\n", sep=""))

  ## Set for iterations
  ngp.class$predict <- -1
  ngp.class[PS, ]$predict <- 1
  ngp.class$predict <- as.factor(ngp.class$predict)

  ## First run of Naive Bayes
  nBayes.model <- naiveBayes(
    ngp.dtm[c(PS, RN), ],
    ngp.class[c(PS, RN), ]$predict,
    laplace=0.1)
  QS.predict <- predict(nBayes.model, ngp.dtm[QS, ])
  var.iter <- 1
  var.iter.values <- c(0, 0, 0)

  while(var.iter < 25) {
    utils.cat(paste("        Spy_EM.BuildModel: thread #", var.j, ", iteration #", var.iter, "\n", sep=""))

    ngp.class[QS, ]$predict <- QS.predict

    nBayes.model <- naiveBayes(ngp.dtm, ngp.class$predict, laplace=0.1)
    QS.predict <- predict(nBayes.model, ngp.dtm[QS, ])
    var.iter <- var.iter + 1

    ## Stopping condition
    if (identical(ngp.class[QS, ]$predict, QS.predict)) {
      break
    } else {
      var.iter.values[3] <- var.iter.values[2]
      var.iter.values[2] <- var.iter.values[1]
      var.iter.values[1] <- length(which(ngp.class[QS, ]$predict != QS.predict))

      ## If the number of different predictions between last 3 iterations is less than 10%, we can assume that it has converged
      # print(var.iter.values)
      var.maxDiff <- ceiling(min(var.iter.values) / 10)
      if (!(0 %in% var.iter.values)
          & (var.iter.values[3] - var.iter.values[2] < var.maxDiff)
          & (var.iter.values[2] - var.iter.values[1] < var.maxDiff)) {
        break
      }
    }
  }
  utils.cat(paste("        Spy_EM.BuildModel:  thread #", var.j, ", break at #", var.iter, "\n", sep=""))
  return(nBayes.model)
}