library("e1071")
source("newsgroup-Rocchio.R")
source("newsgroup-utils-functions.R")


ngp.model.RocchioSVM <- function (ngp.dtm, ngp.class) {

  RN <- ngp.model.RocchioSVM.FindRN(ngp.dtm, ngp.class)
  ngp.class$RN <- 0
  ngp.class[RN, ]$RN <- -1

  model.RocchioSVM <- ngp.model.RocchioSVM.BuildModel(ngp.dtm, ngp.class)

  return(model.RocchioSVM)
}

ngp.model.RocchioSVM.FindRN <- function (ngp.dtm, ngp.class) {

  ## Find RN using Rocchio
  model.Rocchio <- ngp.model.Rocchio(ngp.dtm, ngp.class)
  ngp.class$predict <- ngp.model.RocchioClassifer(model.Rocchio, ngp.dtm)

  PS <- rownames(ngp.class[ngp.class$label == 1, ])
  RN <- rownames(ngp.class[ngp.class$predict == -1, ])
  RN <- RN [! RN %in% PS]

  return(RN)
}

ngp.model.RocchioSVM.BuildModel <- function (ngp.dtm, ngp.class) {
  PS <- rownames(ngp.class[ngp.class$label == 1, ])
  US <- rownames(ngp.class[ngp.class$label == -1, ])
  RN <- rownames(ngp.class[ngp.class$RN == -1, ])

  ## QS = US = RN
  QS <- US [! US %in% RN]
  rm(US)

  stopifnot(length(PS) + length(QS) + length(RN) == nrow(ngp.dtm))

  ngp.class$predict <- -1
  ngp.class[PS, ]$predict <- 1
  ngp.class$predict <- as.factor(ngp.class$predict)

  model.svm.init <- svm(ngp.dtm[c(PS, RN), ],
                     y=ngp.class[c(PS, RN), ]$predict,
                     type = "C-classification",
                     kernel="linear",
                     cachesize=256)
  model.svm.i <- model.svm.init

  var.iter <- 0
  while(length(QS) > 0) {
    var.iter <- var.iter + 1

    if (length(QS) == 1) {
      ## special case for bug causing logical subscript out of bound error
      ## because QS has only 1 document and becomes a vector instead of a matrix
      ngp.class[QS, ]$predict <- predict(model.svm.i, ngp.dtm[c(QS, QS), ])[QS ]
    } else {
      ngp.class[QS, ]$predict <- predict(model.svm.i, ngp.dtm[QS, ])
    }

    utils.cat(paste("        RocSVM thread #", var.j, ", 2\n", sep=""))
    predict.neg <- rownames(ngp.class[ngp.class$predict == -1, ])

    utils.cat(paste("        RocSVM thread #", var.j, ", 3\n", sep=""))
    WS <- QS[QS %in% predict.neg]

    utils.cat(paste("        RocSVM thread #", var.j, ", 4\n", sep=""))

    # utils.cat(paste("           RocSVM thread #", var.j, ", WS:", length(WS), "\n", sep=""))

    if (length(WS) == 0) {
      break
    } else {
      utils.cat(paste("        RocSVM thread #", var.j, ", 5\n", sep=""))
      if (length(QS) >= length(WS)) {
        utils.cat(paste("        RocSVM thread #", var.j, ", 6\n", sep=""))
        QS <- QS[! QS %in% WS]
      }
      utils.cat(paste("        RocSVM thread #", var.j, ", 7, QS:", length(QS), "\n", sep=""))
      RN <- c(RN, WS)

      # utils.cat(ngp.class[c(PS, RN), ]$predict)

      ## Build another model
      utils.cat(paste("        RocSVM thread #", var.j, ", 8\n", sep=""))
      model.svm.i <- svm(ngp.dtm[c(PS, RN), ],
                         y=ngp.class[c(PS, RN), ]$predict,
                         type="C-classification",
                         kernel="linear",
                         cachesize=256)
      utils.cat(paste("        RocSVM thread #", var.j, ", 9\n", sep=""))
    }
  }
  utils.cat(paste("            RocSVM:  thread #", var.j, ", break at #", var.iter, "\n", sep=""))


  ## Additional step: Use final classifier to check PS
  predict <- predict(model.svm.i, ngp.dtm[PS, ])
  negativeCount <- length(predict[predict == -1])

  ## Selecting final classifier
  if (negativeCount / length(PS) > 0.05) {
    model.svm.i <- model.svm.init
    utils.cat(paste("            RocSVM:  thread #", var.j, ", return initial classifier\n", sep=""))
  } else {
    model.svm.i <- model.svm.i
    utils.cat(paste("            RocSVM:  thread #", var.j, ", return final classifier\n", sep=""))
  }

  return (model.svm.i)
}