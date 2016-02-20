utils.calculateFMeasure <- function(ngp.results) {
  TP <- nrow(ngp.results[(ngp.results$class == 1 & ngp.results$predict == 1), ])
  TN <- nrow(ngp.results[(ngp.results$class == -1 & ngp.results$predict == -1), ])
  FP <- nrow(ngp.results[(ngp.results$class == -1 & ngp.results$predict == 1), ])
  FN <- nrow(ngp.results[(ngp.results$class == 1 & ngp.results$predict == -1), ])

  Precision <- TP / (TP+FP)
  Recall    <- TP / (TP+FN)

  return ((2 * Precision * Recall) / (Precision + Recall))
}

utils.calculateAccuracy <- function(ngp.results) {
  TP <- nrow(ngp.results[(ngp.results$class == 1 & ngp.results$predict == 1), ])
  TN <- nrow(ngp.results[(ngp.results$class == -1 & ngp.results$predict == -1), ])
  FP <- nrow(ngp.results[(ngp.results$class == -1 & ngp.results$predict == 1), ])
  FN <- nrow(ngp.results[(ngp.results$class == 1 & ngp.results$predict == -1), ])

  Accuracy <- (TP+TN) / (TP+FP+TN+FN)
  return (Accuracy)
}