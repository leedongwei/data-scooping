libs <- c("e1071", "caret")
install.packages(libs, dependencies=TRUE)
lapply(libs, require, character.only=TRUE)

setwd("C:/Users/DongWei/Documents/Projects/data-scooping/data/breast-cancer-wisconsin")





################################################
#### Create Utility functions
################################################



f.bcw <- bcw[,c(2:6,8:10)]
CCategory <- function(DF, kconst) {
  if (kconst == 2) {
    index <- which(bcw$V11==2)

  } else {
    index <- which(bcw$V11==4)
  }
  value <- DF[index, ]
  return(value)
}
CVector <- function(DF.1, DF.2) {
  alpha <- 16
  beta <- 4
  norma.d.1 <- apply(DF.1, 1, function(x) {x/sqrt(x)})
  DF.1.size <- nrow(DF.1)
  term.1 <- alpha*rowSums(norma.d.1)/DF.1.size
  norma.d.2 <- apply(DF.2, 1, function(x) {x/sqrt(x)})
  DF.2.size <- nrow(DF.2)
  term.2 <- beta*rowSums(norma.d.2)/DF.2.size
  c <- term.1+term.2
  return(c)
}
LabelData <- function(d, c.1, c.2) {
  r.1 <- sum(c.1*d.1)
  r.2 <- sum(c.2*d.1)
  if (r.1 >= r.2) {
    d$V11 <- 2
  } else {
    d$V11 <- 4
  }
  return(d)
}
Cat.1 <- CCategory(f.bcw, 2)
str(Cat.1)
Cat.2 <- CCategory(f.bcw, 4)
c.1 <- CVector(Cat.1, Cat.2)
c.2 <- CVector(Cat.2, Cat.1)
d.1 <- f.bcw[5, ]
r.1 <- sum(c.1*d.1)
LabelData(d.1, c.1, c.2)
length(c.1)

################################################
####    Loading the data
################################################
bcw <- read.table("breast-cancer-wisconsin.data", sep=",")
bcw.headers <- c(
  "id",          "thickness", 
  "unifmtySize", "unifmtyShape", 
  "adhesion",    "cellSize",
  "nuclei",      "chromatin",
  "nucleoli",    "mitoses",
  "class"
)
names(bcw) <- bcw.headers



################################################
####    Splitting the data
################################################
trainIndex <- createDataPartition(
                bcw$class, 
                times = 1,
                p = 0.6,
                list = FALSE)

## 60% is set as training set, 40% for test set
bcw.trn <- bcw[trainIndex, ]
bcw.tst <- bcw[-trainIndex, ]

folds <- createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)




## Class "4" (malignant) is the positive set
bcw.trn.PS <- subset(bcw.trn, class=="4")
bcw.trn.NS <- subset(bcw.trn, class=="2")

spyIndex <- createDataPartition(
              bcw.trn.PS$class, 
              times = 1,
              p = 0.6,
              list = FALSE)

bcw.trn.PS.unlabelled <- bcw.trn.PS[spyIndex, ]
bcw.trn.PS.labelled <- bcw.trn.PS[-spyIndex, ]


