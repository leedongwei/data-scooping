## True positives (TP) - Positive labeled correctly
## True negatives (TN) - Negative labeled correctly
## False positives (FP) - Positive labeled wrongly
## False negatives (FN) - Negative labeled wrongly
bcw.calculateFMeasure <- function(bcw.data) {
  TP <- nrow(bcw.data[(bcw.data$class == 4 & bcw.data$predict == 4), ])
  TN <- nrow(bcw.data[(bcw.data$class == 2 & bcw.data$predict == 2), ])
  FP <- nrow(bcw.data[(bcw.data$class == 2 & bcw.data$predict == 4), ])
  FN <- nrow(bcw.data[(bcw.data$class == 4 & bcw.data$predict == 2), ])

  Precision <- TP / (TP+FP)
  Recall    <- TP / (TP+FN)

  return ((2 * Precision * Recall) / (Precision + Recall))
}
