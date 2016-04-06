ngp.sampling <- function
(ngp.PS, ngp.NS, ngp.class, ngp.output, trnLabeled, var.i) {
  
  utils.cat(paste("TrnPct", trnLabeled[var.i], " |  Sample", var.j, "of", repSamples, "\n"))
  ## Split for training/testing sets, 60% of data to be set as training
  temp <- sample(1:nrow(ngp.PS), 0.6 * nrow(ngp.PS), replace = FALSE)
  ngp.trn.PS <- ngp.PS[temp, ]
  ngp.tst <- ngp.PS[-temp, ]
  
  temp <- sample(1:nrow(ngp.NS), 0.6 * nrow(ngp.NS), replace = FALSE)
  ngp.trn.NS <- ngp.NS[temp, ]
  ngp.tst <- c(ngp.tst, ngp.NS[-temp, ])
  
  ## Check for train/test split
  stopifnot(nrow(ngp.trn.PS) + nrow(ngp.trn.NS) + nrow(ngp.tst)
            == nrow(ngp.PS) + nrow(ngp.NS))
  
  
  
  ## Split for labeled data and unlabeled data
  temp <- sample(
    1:nrow(ngp.trn.PS),
    ceiling(trnLabeled[var.i] * nrow(ngp.trn.PS)),
    replace = FALSE)
  ngp.trn.US <- c(ngp.trn.PS[-temp, ], ngp.trn.NS)  # Move unlabeled PS into US
  ngp.trn.PS <- ngp.trn.PS[temp, ]                  # Remove unlabeled PS from the set
  
  ## Check for labeled/unlabeled split
  stopifnot(nrow(ngp.trn.PS) + nrow(ngp.trn.US) + nrow(ngp.tst)
            == nrow(ngp.PS) + nrow(ngp.NS))
  
  
  
  ## Mark labeled training data in data frame
  ngp.trn.class <- ngp.class[c(rownames(ngp.trn.PS), rownames(ngp.trn.US)), ]
  ngp.trn.class["isLabeled"] <- FALSE
  ngp.trn.class["label"] <- -1
  ngp.trn.class[rownames(ngp.trn.PS), ]$isLabeled <- TRUE
  ngp.trn.class[rownames(ngp.trn.PS), ]$label <- 1
  ngp.trn.class$label <- as.factor(ngp.trn.class$label)
  
  ## Convert to matrix
  ngp.trn <- c(ngp.trn.PS, ngp.trn.US)
  ngp.trnMatrix <- as.matrix(ngp.trn)
  
  ## Arrange by document ID, check IDs are correct
  ngp.trnMatrix <- ngp.trnMatrix[order(rownames(ngp.trnMatrix)), ]
  ngp.trn.class <- ngp.trn.class[order(rownames(ngp.trn.class)), ]
  stopifnot(rownames(ngp.trnMatrix) == rownames(ngp.trn.class))
  
  
  ## Repeat matrix + class for testing data
  ngp.tstMatrix <- as.matrix(ngp.tst)
  ngp.tstMatrix <- ngp.tstMatrix[order(rownames(ngp.tstMatrix)), ]
  ngp.tst.class <- ngp.class[rownames(ngp.tst), ]
  ngp.tst.class <- ngp.tst.class[order(rownames(ngp.tst.class)), ]
  
  ## Creating folds for 10-fold cross validation used later
  ngp.tst.class$fold <- createFolds(rownames(ngp.tst.class), k = 10, list = FALSE, returnTrain = FALSE)
  
  ## Cleaning up
  rm(ngp.trn.PS, ngp.trn.NS, ngp.trn.US, ngp.tst)
  
  # ngp.dtm <- ngp.trnMatrix
  # ngp.class <- ngp.trn.class
  
  
  
  ##############################################
  ####    Build Models
  utils.cat(paste("    Building Models: ", trnLabeled[var.i], "% / sample", var.j, "\n", sep=""))
  
  utils.cat(paste("        Start naiveBayes: ", trnLabeled[var.i], "% / sample", var.j, "\n", sep=""))
  models.nBayes <- naiveBayes(ngp.trnMatrix, ngp.trn.class$label, laplace = 0.1)

  utils.cat(paste("        Start SVM: ", trnLabeled[var.i], "% / sample", var.j, "\n", sep=""))
  models.SVM <- svm(x=ngp.trnMatrix, 
                    y=ngp.trn.class$label, 
                    type="C-classification",
                    kernel="linear",
                    cachesize=256)
  
  utils.cat(paste("        Start Rocchio: ", trnLabeled[var.i], "% / sample", var.j, "\n", sep=""))
  models.Rocchio <- ngp.model.Rocchio(ngp.trnMatrix, ngp.trn.class)
  
#   utils.cat(paste("        Start SpyEM: ", trnLabeled[var.i], "% / sample", var.j, "\n", sep=""))
#   models.Spy_EM <- ngp.model.Spy_EM(ngp.trnMatrix, ngp.trn.class)
  
  utils.cat(paste("        Start RocSVM: ", trnLabeled[var.i], "% / sample", var.j, "\n", sep=""))
  models.RocSVM <- ngp.model.RocchioSVM(ngp.trnMatrix, ngp.trn.class)

#   utils.cat(paste("        Start KMeans: ", trnLabeled[var.i], "% / sample", var.j, "\n", sep=""))
#   models.KMeans <- ngp.model.KMeans(ngp.trnMatrix, ngp.trn.class, 2, 3)
  
  
  
  ################################################
  ## Run the models on test data
  utils.cat(paste("    Predicting: ", trnLabeled[var.i], "% / sample", var.j, "\n", sep=""))
  
  results.AllPos  <- as.factor(rep(1, nrow(ngp.tstMatrix)))
  
  # Fake stand-in for actual
  results.nBayes  <- as.factor(rep(1, nrow(ngp.tstMatrix)))
  results.SVM     <- as.factor(rep(1, nrow(ngp.tstMatrix)))
  results.Rocchio <- as.factor(rep(1, nrow(ngp.tstMatrix)))
  results.Spy_EM  <- as.factor(rep(1, nrow(ngp.tstMatrix)))
  results.RocSVM  <- as.factor(rep(1, nrow(ngp.tstMatrix)))
  results.KMeans  <- as.factor(rep(1, nrow(ngp.tstMatrix)))
  
  ## Actual prediction
  results.nBayes  <- predict(models.nBayes, ngp.tstMatrix)
  results.SVM     <- predict(models.SVM, ngp.tstMatrix)
  results.Rocchio <- ngp.model.RocchioClassifer(models.Rocchio, ngp.tstMatrix)
  # results.Spy_EM  <- predict(models.Spy_EM, ngp.tstMatrix)
  results.RocSVM  <- predict(models.RocSVM, ngp.tstMatrix)
  # results.KMeans  <- ngp.model.KMeans.predict(models.KMeans, ngp.tstMatrix)
  
  
  
  
  
  ################################################
  ## Calculating performance
  utils.cat(paste("    Calculating Performance: ", trnLabeled[var.i], "% / sample", var.j, "\n", sep=""))
  
  ## Calculate performance for each fold
  for (i in 1:10) {
    
    ## AllPos
    ngp.tst.class$predict <- utils.convertFactorToNumeric(results.AllPos)
    ngp.output["fmeasure", "AllPos"] <- ngp.output["fmeasure", "AllPos"] + utils.calculateFMeasure(ngp.tst.class[ngp.tst.class$fold == i, ])
    ngp.output["accuracy", "AllPos"] <- ngp.output["accuracy", "AllPos"] + utils.calculateAccuracy(ngp.tst.class[ngp.tst.class$fold == i, ])
    
    ## Naive-Bayes
    ngp.tst.class$predict <- utils.convertFactorToNumeric(results.nBayes)
    ngp.output["fmeasure", "nBayes"] <- ngp.output["fmeasure", "nBayes"] + utils.calculateFMeasure(ngp.tst.class[ngp.tst.class$fold == i, ])
    ngp.output["accuracy", "nBayes"] <- ngp.output["accuracy", "nBayes"] + utils.calculateAccuracy(ngp.tst.class[ngp.tst.class$fold == i, ])
    
    ## SVM
    ngp.tst.class$predict <- utils.convertFactorToNumeric(results.SVM)
    ngp.output["fmeasure", "SVM"] <- ngp.output["fmeasure", "SVM"] + utils.calculateFMeasure(ngp.tst.class[ngp.tst.class$fold == i, ])
    ngp.output["accuracy", "SVM"] <- ngp.output["accuracy", "SVM"] + utils.calculateAccuracy(ngp.tst.class[ngp.tst.class$fold == i, ])
    
    ## Rocchio
    ngp.tst.class$predict <- utils.convertFactorToNumeric(results.Rocchio)
    ngp.output["fmeasure", "Rocchio"] <- ngp.output["fmeasure", "Rocchio"] + utils.calculateFMeasure(ngp.tst.class[ngp.tst.class$fold == i, ])
    ngp.output["accuracy", "Rocchio"] <- ngp.output["accuracy", "Rocchio"] + utils.calculateAccuracy(ngp.tst.class[ngp.tst.class$fold == i, ])
    
    ## Spy-EM
    ngp.tst.class$predict <- utils.convertFactorToNumeric(results.Spy_EM)
    ngp.output["fmeasure", "Spy-EM"] <- ngp.output["fmeasure", "Spy-EM"] + utils.calculateFMeasure(ngp.tst.class[ngp.tst.class$fold == i, ])
    ngp.output["accuracy", "Spy-EM"] <- ngp.output["accuracy", "Spy-EM"] + utils.calculateAccuracy(ngp.tst.class[ngp.tst.class$fold == i, ])
    
    ## RocSVM
    ngp.tst.class$predict <- utils.convertFactorToNumeric(results.RocSVM)
    ngp.output["fmeasure", "RocSVM"] <- ngp.output["fmeasure", "RocSVM"] + utils.calculateFMeasure(ngp.tst.class[ngp.tst.class$fold == i, ])
    ngp.output["accuracy", "RocSVM"] <- ngp.output["accuracy", "RocSVM"] + utils.calculateAccuracy(ngp.tst.class[ngp.tst.class$fold == i, ])
    
    ## KMeans
    ngp.tst.class$predict <- utils.convertFactorToNumeric(results.KMeans)
    ngp.output["fmeasure", "KMeans"] <- ngp.output["fmeasure", "KMeans"] + utils.calculateFMeasure(ngp.tst.class[ngp.tst.class$fold == i, ])
    ngp.output["accuracy", "KMeans"] <- ngp.output["accuracy", "KMeans"] + utils.calculateAccuracy(ngp.tst.class[ngp.tst.class$fold == i, ])
    
  }
  
  ## Average results over 10 folds
  ngp.output <- ngp.output / 10
  
  utils.cat(ngp.output)
  
  return(ngp.output)
}