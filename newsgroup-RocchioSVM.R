library("e1071")
source("newsgroup-Rocchio.R")
source("newsgroup-utils-functions.R")


ngp.model.RocchioSVM <- function (ngp.trnMatrix, ngp.class) {

  RN <- ngp.model.RocchioSVM.FindRN(ngp.trnMatrix, ngp.class)
  ngp.class$RN <- 0
  ngp.class[RN, ]$RN <- -1

  model.RocchioSVM <- ngp.model.RocchioSVM.BuildModel(ngp.trnMatrix, ngp.class)

  return(model.RocchioSVM)
}

ngp.model.RocchioSVM.FindRN <- function (ngp.trnMatrix, ngp.class) {
  PS <- rownames(ngp.class[ngp.class$label == 1, ])
  US <- rownames(ngp.class[ngp.class$label == -1, ])

  ngp.positiveVector <- ngp.model.rocchioVectorBuilder(ngp.trnMatrix[PS, ], ngp.trnMatrix[US, ])
  ngp.negativeVector <- ngp.model.rocchioVectorBuilder(ngp.trnMatrix[US, ], ngp.trnMatrix[PS, ])

  ## Find RN using Rocchio
  ngp.class$predict <- 0
  for (doc in US) {
    ngp.class[doc, ]$predict <- ngp.model.rocchioClassifer(ngp.trnMatrix[doc, ],
                                                           ngp.positiveVector,
                                                           ngp.negativeVector)
  }

  return(rownames(ngp.class[ngp.class$predict == -1, ]))
}

ngp.model.RocchioSVM.BuildModel <- function (ngp.trnMatrix, ngp.class) {
  PS <- rownames(ngp.class[ngp.class$label == 1, ])
  US <- rownames(ngp.class[ngp.class$label == -1, ])
  RN <- rownames(ngp.class[ngp.class$label == -1 & ngp.class$RN == -1, ])
  
  ## QS = US = RN
  QS <- US [! US %in% RN]
  rm(US)
  
  ngp.class$predict <- 0
  ngp.class[PS, ]$predict <- 1
  ngp.class[RN, ]$predict <- -1
  ngp.class$predict <- as.factor(ngp.class$predict)

  model.svm.init <- svm(ngp.trnMatrix[c(PS, RN), ], 
                     y=ngp.class[c(PS, RN), ]$predict, 
                     type = "C-classification")
  model.svm.i <- model.svm.init
  
  var.iter <- 0
  while(TRUE) {
    var.iter <- var.iter + 1
    print (var.iter)
    
    ngp.class[QS, ]$predict <- predict(model.svm.i, ngp.trnMatrix[QS, ])
    WS <- QS[QS %in% rownames(ngp.class[ngp.class$predict == -1, ])]
  }
  
  if (nrow(WS) == 0) {
    break
  } else {
    QS <- QS[! QS %in% WS]
    RN <- c(RN, WS)
    
    ## Build another model
    model.svm.i <- svm(ngp.trnMatrix[c(PS, RN), ],
                       y=ngp.class[c(PS, RN), ]$predict, 
                       type = "C-classification")
  }
  
  
  ## Additional step: Use final classifier to check PS
  predict <- predict(model.svm.i, ngp.trnMatrix[PS, ])
  negativeCount <- length(predict[predict == -1])
  
  ## Selecting final classifier
  if (negativeCount / length(PS) > 0.05) {
    model.svm.i <- model.svm.init
  } else {
    model.svm.i <- model.svm.i
  }
  
  return (model.svm.i)
}