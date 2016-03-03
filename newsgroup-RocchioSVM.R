library("e1071")
source("newsgroup-Rocchio.R")
source("newsgroup-utils-functions.R")


ngp.model.RocchioSVM <- function (ngp.trnMatrix, ngp.class) {

  RN <- ngp.model.RocchioSVM.FindRN(ngp.trnMatrix, ngp.class, var.spy)
  ngp.class$RN <- 0
  ngp.class[RN, ]$RN <- -1

  model.RocchioSVM <- ngp.model.RocchioSVM.BuildModel(ngp.trnMatrix, ngp.class)

  return(model.RocchioSVM)
}

ngp.model.RocchioSVM.FindRN <- function (ngp.trnMatrix, ngp.class) {

}

ngp.model.RocchioSVM.BuildModel <- function (ngp.trnMatrix, ngp.class) {

}