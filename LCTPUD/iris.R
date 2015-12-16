libs <- c("caret", "e1071")
lapply(libs, require, character.only=TRUE)




################################################
####    Utility Functions
################################################
RocchioVectorBuilder <- function(DF1, DF2) {
  alpha <- 16
  beta <- 4
  
  ## Remove non-significant columns (e.g. id, class, rocLabel)
  DF1 <- DF1[ , cnst.features]
  DF2 <- DF2[ , cnst.features]
  
  norma.d.1 <- apply(DF1, 1, function(x){x/sqrt(sum(x^2))})
  DF1.size <- nrow(DF1)
  term.1 <- alpha * rowSums(norma.d.1)/DF1.size
  
  norma.d.2 <- apply(DF2, 1, function(x){x/sqrt(sum(x^2))})
  DF2.size <- nrow(DF2)
  term.2 <- beta * rowSums(norma.d.2)/DF2.size
  
  c <- term.1 + term.2
  return(c)
}



RocchioClassifer <- function(DF.row, vector1, vector2) {
  ## Remove non-significant columns
  DF.row <- DF.row[ , cnst.features]
  
  r1 <- sum(vector1 * DF.row)
  r2 <- sum(vector2 * DF.row)
  
  if (r1 >= r2) {
    rocLabel <- "positive"
  } else {
    rocLabel <- "negative"
  }
  
  return(rocLabel)
}



SvmClassifierBuilder <- function(posData, negData, uData) {
  ## Build initial classifier
  temp.0.classifier <- svm(label ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                           data = rbind(posData, negData),
                           type = "C-classification")
  
  ## Enter loop to build classifier iteratively
  temp.i.classifier <- temp.0.classifier
  temp.i <- 0 
  while (TRUE) {
    temp.i <- temp.i + 1
    
    uData$label <- predict(temp.i.classifier, uData)
    uData.w <- uData[uData$label == -1, ]
    
    if (nrow(uData.w) == 0) {
      break
    } else {
      uData <- uData[uData$label == 1, ]
      negData <- rbind(negData, uData.w)
      
      ## Build new classifier
      temp.i.classifier <- svm(label ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                               data = rbind(posData, negData),
                               type = "C-classification")
      
    }
  }
  
  ## Check final classifier using PS
  posData$svmlabel <- predict(temp.i.classifier, posData)
  posData.negCount <- nrow(posData[posData$svmLabel == -1, ])
  
  ## Selecting final classifier
  if (posData.negCount / nrow(posData) > 0.05) {
    temp.classifier <- temp.0.classifier
  } else {
    temp.classifier <- temp.i.classifier
  }
  
  return (temp.classifier)
}



calculateF <- function(data) {
  TP <- nrow(data[(data$class == 1  & data$predict == 1), ])
  TN <- nrow(data[(data$class == -1 & data$predict == -1), ])
  FP <- nrow(data[(data$class == -1 & data$predict == 1), ])
  FN <- nrow(data[(data$class == 1  & data$predict == -1), ])
  
  Precision <- TP / (TP+FP)
  Recall    <- TP / (TP+FN)
  
  F <- (2 * Precision * Recall) / (Precision + Recall)
  return (F)
}










################################################
####    Load data from Iris
################################################
data(iris)
cnst.features <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
cnst.a <- c(0.05, 0.15, 0.25, 0.35, 0.45, 0.55, 0.65)
cnst.class <- c("setosa", "versicolor", "virginica")



################################################
###     Set as one giant function
################################################

##  Create data frame to store results
results <- array (-1,
                  dim=c(
                    length(cnst.a),    ## Number of 'a'
                    5))                ## Number of algorithms  
results <- data.frame(results)
rownames(results) <- cnst.a
colnames(results) <- c("NB", "S-EM", "PEBL", "Roc-SVM", "Roc-Clu-SVM")


## Set up loop
f.pClass <- "setosa"
for (vary.a in 1:length(cnst.a)) {
  f.a <- cnst.a[vary.a]
    

################################################
####    Splitting the data
################################################
iris.positive <- subset(iris, iris$Species == f.pClass)
iris.negative <- subset(iris, iris$Species != f.pClass)

iris.positive$Species <- NULL
iris.positive$class <- 1

iris.negative$Species <- NULL
iris.negative$class <- -1

index <- 1:nrow(iris.positive)
temp <- as.numeric(sample(index, (f.a * length(index)), replace = FALSE))

## Save a% of positive documents as Positive Set
## Move the rest into Unlabeled Set
iris.PS <- iris.positive[temp, ]
iris.US <- iris.positive[-temp, ]



index <- 1:nrow(iris.negative)
temp <- as.numeric(sample(index, (f.a * length(index)), replace = FALSE))

## Save a% of negative documents as Negative Set
## Move the rest into Unlabeled Set
iris.NS <- iris.negative[temp, ]
iris.US <- rbind(iris.negative[-temp, ], iris.US)



################################################
####    Step 1: Find Reliably Negative
################################################

################################################
####    Rocchio Only
################################################
## Copy data for this algorithm
iris.Rocchio.PS <- iris.PS
iris.Rocchio.US <- iris.US

## US are all considered as negative
iris.Rocchio.PS$label <- "positive"
iris.Rocchio.US$label <- "negative"

rocchio.posVector <- RocchioVectorBuilder(iris.Rocchio.PS, iris.Rocchio.US)
rocchio.negVector <- RocchioVectorBuilder(iris.Rocchio.US, iris.Rocchio.PS)

iris.Rocchio.US$rocLabel <- -1
for (i in 1:nrow(iris.Rocchio.US)) {
  iris.Rocchio.US[i,]$rocLabel <- RocchioClassifer(iris.Rocchio.US[i,], rocchio.posVector, rocchio.negVector)
}

iris.Rocchio.RN <- iris.Rocchio.US[iris.Rocchio.US$rocLabel == "negative", ]



################################################
####    Rocchio with Clustering
################################################
iris.RocClu.PS <- iris.PS
iris.RocClu.US <- iris.US

iris.RocClu.RN <- iris.Rocchio.RN
iris.RocClu.RN$rocLabel <- 0


## k = 10, from the paper: "choice of k does not affect 
## classification results much as long as it is not too small"
temp <- kmeans(iris.RocClu.RN[, cnst.features], 10)
iris.RocClu.RN$cluster <- temp$cluster


rocCluster.posVectors <- data.frame(
                          "Sepal.Length"=numeric(0),
                          "Sepal.Width"=numeric(0), 
                          "Petal.Length"=numeric(0), 
                          "Petal.Width"=numeric(0))
rocCluster.negVectors <- data.frame(
                          "Sepal.Length"=numeric(0),
                          "Sepal.Width"=numeric(0), 
                          "Petal.Length"=numeric(0), 
                          "Petal.Width"=numeric(0))
for (j in 1:10) {
  rocCluster.posVectors <- rbind(rocCluster.posVectors,
                                 RocchioVectorBuilder(
                                  iris.RocClu.PS, 
                                  iris.RocClu.RN[iris.RocClu.RN$cluster == j, ]))
  rocCluster.negVectors <- rbind(rocCluster.negVectors,
                                 RocchioVectorBuilder(
                                  iris.RocClu.RN[iris.RocClu.RN$cluster == j, ],
                                  iris.RocClu.PS))
}

colnames(rocCluster.posVectors) <- cnst.features
colnames(rocCluster.negVectors) <- cnst.features


for (i in 1:nrow(iris.RocClu.RN)) {
  temp.row <- iris.RocClu.RN[i, ]
  temp.pSim <- numeric(0)
  temp.nSim <- numeric(0)
  
  for (j in 1:10) {
    temp.pSim <- c(temp.pSim, sum(rocCluster.posVectors[j, ] * iris.RocClu.RN[i , cnst.features]))
  }
  temp.pSim <- max(temp.pSim)
  
  for (j in 1:10) {
    temp.nSim <- sum(rocCluster.negVectors[j, ] * iris.RocClu.RN[i , cnst.features])
    
    if (temp.nSim > temp.pSim) {
      iris.RocClu.RN[i, ]$rocLabel <- "negative"
      break
    } else {
      iris.RocClu.RN[i, ]$rocLabel <- "positive"
    }
  }
}

iris.RocClu.RN <- iris.RocClu.RN[iris.RocClu.RN$rocLabel == "negative", ]



################################################
####    Step 2: Classifier Building
################################################

## Clean-up and re-label
iris.RocClu.RN$cluster <- NULL
iris.RocClu.RN$rocLabel <- NULL
iris.RocClu.RN$label <- -1

iris.Rocchio.RN$rocLabel <- NULL
iris.Rocchio.RN$label <- -1

iris.PS$label <- 1


## Build classifier for Roc-SVM
iris.Rocchio.US <- subset(iris.US, !(rownames(iris.US) %in% rownames(iris.Rocchio.RN)))
classifier.RocSVM <- SvmClassifierBuilder(iris.PS, iris.Rocchio.RN, iris.Rocchio.US)


## Build classifier for Roc-Clu-SVM
iris.RocClu.US <- subset(iris.US, !(rownames(iris.US) %in% rownames(iris.RocClu.RN)))
classifier.RocCluSVM <- SvmClassifierBuilder(iris.PS, iris.RocClu.RN, iris.RocClu.US)



################################################
####    Naive-Bayes
################################################
iris.nb.PS <- iris.PS
iris.nb.US <- iris.US

iris.nb.PS$label <- 1
iris.nb.US$label <- -1

classifier.NB <- naiveBayes(
  as.factor(label) ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
  data = (rbind(iris.nb.PS, iris.nb.US)),
  laplace = 0)



################################################
####    S-EM
################################################
#     iris.SEM.PS <- iris.PS
#     iris.SEM.US <- iris.US
#     
#     iris.SEM.US$label <- 0
#     iris.SEM.PS$isSpy <- FALSE
#     iris.SEM.US$isSpy <- FALSE
#     
#     ## Value of s is taken from page 3 of Liu, Dai, Li, Lee, Yu (2003)
#     cnst.s <- 0.15
#     index <- 1:nrow(iris.SEM.PS)
#     temp <- as.numeric(sample(index, (ceiling(cnst.s * length(index))), replace = FALSE))
#     
#     ## Put spies into US
#     temp.PS <- iris.SEM.PS[temp, ]
#     temp.PS$isSpy <- TRUE
#     iris.SEM.US <- rbind(temp.PS, iris.SEM.US)
#     iris.SEM.PS <- iris.SEM.PS[-temp, ]
#     
#     iris.SEM.PS$label <- 1
#     iris.SEM.US$label <- -1
#     iris.SEM.PS$label <- as.factor(iris.SEM.PS$label)
#     iris.SEM.US$label <- as.factor(iris.SEM.US$label)
#     
#     ## I-EM from Liu, Lee, Yu, Li (2002)
#     nbc <- naiveBayes(
#       label ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
#       data = (rbind(iris.SEM.PS, iris.SEM.US)),
#       laplace = 0)
#     
#     ## Loop till predicted results converge
#     ## 35 is a randomly chosen number that is sufficiently large enough
#     ## because results converge quite fast 
#     for (i in 1:35) {
#       iris.SEM.US$label <- predict(nbc, iris.SEM.US[, c("label", cnst.features)])
#       temp <- predict(nbc, iris.SEM.US[, c("label", cnst.features)], type="raw")
#       iris.SEM.US$Pr  <- temp[,1]
#       iris.SEM.US$PrN <- temp[,2]
#       
#       ## build new nbc
#       nbc <- naiveBayes(
#         label ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
#         data = rbind(iris.SEM.PS[, c("label", cnst.features)], iris.SEM.US[, c("label", cnst.features)]),
#         laplace = 0)
#     }
#     
#     ## Probability threshold
#     index <- which(iris.SEM.US$isSpy, TRUE)
#     cnst.th <- min(iris.SEM.US$Pr[index])
#     
#     ## RN documents <- Pr < cnst.th
#     iris.SEM.NEGATIVE <- iris.SEM.US[iris.SEM.US$Pr < cnst.th, ]
#     iris.SEM.POSITIVE <- iris.SEM.US[iris.SEM.US$Pr >= cnst.th, ]
#     
#     ## Convert factor to numeric for later use
#     iris.SEM.NEGATIVE$label <- as.numeric(levels(iris.SEM.NEGATIVE$label))[iris.SEM.NEGATIVE$label]
#     iris.SEM.POSITIVE$label <- as.numeric(levels(iris.SEM.POSITIVE$label))[iris.SEM.POSITIVE$label]
#     
#     ## Save for later use
#     iris.SEM.PS$Pr <- 1
#     iris.SEM.PS$PrN <- 0
#     iris.SEM.NS <- iris.SEM.NEGATIVE
#     iris.SEM.US <- iris.SEM.POSITIVE
#     
#     
#     ## Put Spy documents back in PS
#     iris.SEM.PS <- rbind(iris.SEM.PS,
#                             subset(iris.SEM.US, isSpy == TRUE),
#                             subset(iris.SEM.NS, isSpy == TRUE))
#     iris.SEM.US <- subset(iris.SEM.US, (isSpy == FALSE))
#     iris.SEM.NS <- subset(iris.SEM.NS, (isSpy == FALSE))
#     
#     ## Fix labeling and columns
#     iris.SEM.PS$label <- 1
#     iris.SEM.PS$Pr  <- 1
#     iris.SEM.PS$PrN <- 0
#     iris.SEM.PS$isFixed <- TRUE
#     
#     iris.SEM.NS$label <- -1
#     iris.SEM.NS$Pr  <- 0
#     iris.SEM.NS$PrN <- 1
#     iris.SEM.NS$isFixed <- FALSE
#     
#     iris.SEM.US$label <- 0
#     iris.SEM.US$isFixed <- FALSE
#     
#     ##  Run EM algorithm to build final classifier
#     nbc <- naiveBayes(
#       as.factor(label) ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#       data = (rbind(iris.SEM.PS, iris.SEM.NS)),
#       laplace = 0)
#     
#     iris.SEM.data <- rbind(iris.SEM.PS, iris.SEM.NS, iris.SEM.US)
#     iris.SEM.data$isSpy <- NULL
#     iris.SEM.data$spyLabel <- NULL
#     for (i in 1:35) {
#       iris.SEM.data$label <- predict(nbc, iris.SEM.data[, cnst.features])
#       temp <- predict(nbc, iris.SEM.data[, cnst.features], type="raw")
#       iris.SEM.data$Pr  <- temp[,1]
#       iris.SEM.data$PrN <- temp[,2]
#       
#       ## PS does not change, reset it to correct values
#       iris.SEM.data[iris.SEM.data$isFixed == TRUE, ]$label <- 1
#       iris.SEM.data[iris.SEM.data$isFixed == TRUE, ]$Pr  <- 1
#       iris.SEM.data[iris.SEM.data$isFixed == TRUE, ]$PrN <- 0
#       
#       ## build new nbc
#       nbc <- naiveBayes(
#         as.factor(label) ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#         data = iris.SEM.data,
#         laplace = 0)
#     }
#     
#     classifier.SEM <- nbc





################################################
####    Run on Test Set
################################################

## NB
iris.US$predict <- 0
iris.US$predict <- predict(classifier.NB, iris.US[, cnst.features])
results[vary.a, 1] <- calculateF(iris.US)

## S-EM
# iris.US$predict <- 0
# iris.US$predict <- predict(classifier.SEM, iris.US[, cnst.features])
# results[vary.a, 2] <- calculateF(iris.US)

## Roc-SVM
iris.US$predict <- 0
iris.US$predict <- predict(classifier.RocSVM, iris.US[, cnst.features])
results[vary.a, 4] <- calculateF(iris.US)

## Roc-Clu-SVM
iris.US$predict <- 0
iris.US$predict <- predict(classifier.RocCluSVM, iris.US[, cnst.features])
results[vary.a, 5] <- calculateF(iris.US)


## Bracket for for-loop of vary.a
}