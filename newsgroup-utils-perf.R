utils.calculateFMeasure <- function(ngp.results) {
  TP <- nrow(ngp.results[(ngp.results$class == 1 & ngp.results$predict == 1), ])
  TN <- nrow(ngp.results[(ngp.results$class == -1 & ngp.results$predict == -1), ])
  FP <- nrow(ngp.results[(ngp.results$class == -1 & ngp.results$predict == 1), ])
  FN <- nrow(ngp.results[(ngp.results$class == 1 & ngp.results$predict == -1), ])

  ifelse(is.na(TP), 0, TP)
  ifelse(is.na(TN), 0, TN)
  ifelse(is.na(FP), 0, FP)
  ifelse(is.na(FN), 0, FN)

  Precision <- TP / (TP+FP)
  Recall    <- TP / (TP+FN)
  Fmeasure  <- (2 * Precision * Recall) / (Precision + Recall)

  return (ifelse(is.na(Fmeasure), 0, Fmeasure))
}

utils.calculateAccuracy <- function(ngp.results) {
  TP <- nrow(ngp.results[(ngp.results$class == 1 & ngp.results$predict == 1), ])
  TN <- nrow(ngp.results[(ngp.results$class == -1 & ngp.results$predict == -1), ])
  FP <- nrow(ngp.results[(ngp.results$class == -1 & ngp.results$predict == 1), ])
  FN <- nrow(ngp.results[(ngp.results$class == 1 & ngp.results$predict == -1), ])

  ifelse(is.na(TP), 0, TP)
  ifelse(is.na(TN), 0, TN)
  ifelse(is.na(FP), 0, FP)
  ifelse(is.na(FN), 0, FN)

  Accuracy <- (TP+TN) / (TP+FP+TN+FN)
  return (ifelse(is.na(Accuracy), 0, Accuracy))
}