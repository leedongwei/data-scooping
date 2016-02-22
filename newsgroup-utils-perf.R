utils.calculateFMeasure <- function(ngp.results) {
  TP <- nrow(ngp.results[(ngp.results$class == 1 & ngp.results$predict == 1), ])
  TN <- nrow(ngp.results[(ngp.results$class == -1 & ngp.results$predict == -1), ])
  FP <- nrow(ngp.results[(ngp.results$class == -1 & ngp.results$predict == 1), ])
  FN <- nrow(ngp.results[(ngp.results$class == 1 & ngp.results$predict == -1), ])

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

  Accuracy <- (TP+TN) / (TP+FP+TN+FN)
  return (ifelse(is.na(Accuracy), 0, Accuracy))
}