bcw.calculateAccuracy <- function(bcw.data) {
  TP <- nrow(bcw.data[(bcw.data$class == 4 & bcw.data$predict == 4), ])
  TN <- nrow(bcw.data[(bcw.data$class == 2 & bcw.data$predict == 2), ])
  FP <- nrow(bcw.data[(bcw.data$class == 2 & bcw.data$predict == 4), ])
  FN <- nrow(bcw.data[(bcw.data$class == 4 & bcw.data$predict == 2), ])

  Accuracy <- (TP+TN) / (TP+FP+TN+FN)
  return (Accuracy)
}