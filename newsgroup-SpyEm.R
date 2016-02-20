
FindReliableNegativeSet <- function (P, U, Spy.rate) {
  # Spy_EM step 1: find reliable negative set
  # Args: P, postive label data;
  #       U, unlabeled data
  P.size <- nrow(P)
  sample.index <- sample.int(P.size, round(P.size*Spy.rate))
  Us <- rbind(P[sample.index, ], U)
  Ps <- P[-sample.index, ]
  originalNames <- names(Ps)
  Ps$label <- 1
  Us$label <- -1
  Ps$label <- as.factor(Ps$label)
  Us$label <- as.factor(Us$label)
  data <- rbind(Ps, Us)
  NB.model <- naiveBayes(label~., data = data[, !names(data) %in% c('id', 'class')])
  Us.predict <- predict(NB.model, Us)
  iter <- 1
  while (iter < 25) {
    Us$label <- Us.predict
    data <- rbind(Ps, Us)
    NB.model <- naiveBayes(label~., data = data[, !names(data) %in% c('id', 'class')])
    Us.predict <- predict(NB.model, Us)
    iter <- iter+1
    if (identical(Us$label, Us.predict)) {
      break
    }
  }
  index <- which(Us$class == 4)
  thr <- min(predict(NB.model, Us[index, ], type='raw')[, 1])
  Us.predict <- predict(NB.model, Us, type='raw')[, 1]
  RN <- Us[which(Us.predict < thr), ]
  Q <- setdiff(Us, RN)
  RN <- RN[, originalNames]
  Q <- Q[, originalNames]
  return(list(RN=RN, Q=Q))
}

SeparateQSet <- function (P, RN, Q) {
  # Spy_EM step 2: further separte the Q set with Q=U-RN
  P$label <- 1
  RN$label <- -1
  P$label <- as.factor(P$label)
  RN$label <- as.factor(RN$label)
  data <- rbind(P, RN)
  NB.model <- naiveBayes(label~., data = data[, !names(data) %in% c('id', 'class')])
  Q.predict <- predict(NB.model, Q)
  Q$label <- 1 # initialize
  Q$label <- as.factor(Q$label)
  iter <- 1
  while (iter < 25) {
    Q$label <- Q.predict
    data <- rbind(rbind(P, RN), Q)
    NB.model <- naiveBayes(label~., data = data[, !names(data) %in% c('id', 'class')])
    Q.predict <- predict(NB.model, Q)
    iter <- iter+1
    if (identical(Q$label, Q.predict)) {
      break
    }
  }
  return(NB.model)
}

Spy_EM <- function (data, Spy.rate) {
  # Spy_EM algorithm
  # data <- trainData
  P <- data[data$class==4, ]
  U <- data[data$class==2, ]
  RNlist <- FindReliableNegativeSet(P, U, Spy.rate)
  S_EM.model <- SeparateQSet(P, RNlist$RN, RNlist$Q)
}

perfSpy_EM <- function (trnPercent, data, Spy.rate) {
  # run Spy_EM and get F score
#   trnPercent <- 0.3
#   data <- bcw
#   Spy.rate <- 0.15
  data.size <- nrow(data)
  train.index <- createDataPartition(data$class, times=1, p=trnPercent, list=FALSE)
  trainData <- data[as.vector(train.index), ]
  testData <- data[-as.vector(train.index), ]
  S_EM.model <- Spy_EM(data=trainData, Spy.rate = Spy.rate)
  predict.trn <- predict(S_EM.model, trainData, type='raw')
  trainData$label <- 1
  trainData$label[trainData$class==2] <- -1
  trainData$label <- as.factor(trainData$label)
  pred.obj.trn <- prediction(predict.trn[, 1], trainData$label)
  perf.pr.trn <- performance(pred.obj.trn, 'prec', 'rec')
  perf.acc.trn <- performance(pred.obj.trn, 'acc')
  rec <- perf.pr.trn@x.values[[1]]
  prec <- perf.pr.trn@y.values[[1]]
  fscore <- 2*prec*rec/(prec+rec)
  idx.1 <- which.max(fscore)
  cutoff <- perf.pr.trn@alpha.values[[1]][idx.1] # cut-off point
  predict.m <- predict(S_EM.model, testData, type='raw')
  testData$label <- 1
  testData$label[testData$class==2] <- -1
  testData$label <- as.factor(testData$label)
  pred.obj <- prediction(predict.m[, 1], testData$label)
  perf.pr <- performance(pred.obj, 'prec', 'rec')
  perf.acc <- performance(pred.obj, 'acc')
  idx <- which(abs(perf.pr@alpha.values[[1]]-cutoff) < 0.001)[1]
  rec <- perf.pr@x.values[[1]]
  prec <- perf.pr@y.values[[1]]
  fscore <- 2*prec*rec/(prec+rec)
  F <- fscore[idx] # maximum f score
  idx <- which(abs(perf.acc@x.values[[1]]-cutoff) < 0.001)[1]
  acc <- perf.acc@y.values[[1]][idx]
  return(data.frame(F, acc))
}