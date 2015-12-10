libs <- c("e1071", "caret", "plyr")
install.packages(libs, dependencies=TRUE)
lapply(libs, require, character.only=TRUE)


################################################
####    Set Directory (please modify)
################################################
## TODO: Set to your own directory
setwd("C:/Users/DongWei/Documents/Projects/data-scooping/data/breast-cancer-wisconsin")



################################################
####    Loading the data
################################################
bcw <- read.table("breast-cancer-wisconsin.data", sep=",")
bcw.headers <- c( "id", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "class")
bcw.features <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9")
names(bcw) <- bcw.headers


## bcw has non-unique patient IDs
## We will replace all IDs to ensure uniqueness
bcw$id <- 1:length(bcw$id)


## bcw$V6 has missing values
## We will replace them with the mean of V6
index <- which(bcw$V6 %in% "?")

## Convert factor to numeric
bcw$V6 <- as.numeric(levels(bcw$V6))[bcw$V6]

temp <- 0
for (i in 1:length(bcw$V6)) {
  if (!(i %in% index)) {
    temp <- temp + as.numeric(bcw$V6[i])
  }
}
V6.mean <- floor(temp / length(bcw$V6))


## Replace missing V6 values with mean
bcw$V6[index] <- V6.mean
bcw$V6 <- as.integer(bcw$V6)


################################################
####    Set up loop for final data collection
################################################
f.globalSPUL <- numeric(0)
f.localSPUL <- numeric(0)
f.SEM <- numeric(0)
f.RocSVM <- numeric(0)

for (rep10 in 1:10) {

  
  
################################################
####    Splitting the data
################################################
temp <- createDataPartition(
                    bcw$class, 
                    times = 1,
                    p = 0.6,
                    list = FALSE)

## 60% is set as training set, 40% for test set
bcw.trn <- bcw[temp, ]
bcw.tst <- bcw[-temp, ]


## Class "4" (malignant) is the positive set
bcw.trn.positive <- subset(bcw.trn, class=="4")
bcw.trn.negative <- subset(bcw.trn, class=="2")

## Value of g is taken from page 1581 of this paper
cnst.g <- 0.15
temp <- createDataPartition(
                    bcw.trn.positive$class, 
                    times = 1,
                    p = cnst.g,
                    list = FALSE)

## Set up PS and US
bcw.trn.PS <- bcw.trn.positive[temp, ]
bcw.trn.US <- bcw.trn.positive[-temp, ]
bcw.trn.US <- rbind(bcw.trn.US, bcw.trn.negative)

## Delete variables that are never again used
## Prevents confusion in Global Env
#rm(bcw.trn.positive, bcw.trn.negative)



################################################
####    Spy-Technique
################################################
## Copying data
bcw.trn.spy.PS <- bcw.trn.PS
bcw.trn.spy.US <- bcw.trn.US

bcw.trn.spy.PS$isSpy <- FALSE
bcw.trn.spy.US$isSpy <- FALSE

## Value of s is taken from page 3 of Liu, Dai, Li, Lee, Yu (2003)
cnst.s <- 0.15
temp <- createDataPartition(
                    bcw.trn.spy.PS$class, 
                    times = 1,
                    p = cnst.s,
                    list = FALSE)

## Put spies into US
temp.PS <- bcw.trn.spy.PS[temp, ]
temp.PS$isSpy <- TRUE
bcw.trn.spy.US <- rbind(temp.PS,bcw.trn.spy.US)
bcw.trn.spy.PS <- bcw.trn.spy.PS[-temp, ]

bcw.trn.spy.PS$spyLabel <- 4
bcw.trn.spy.US$spyLabel <- 2
bcw.trn.spy.PS$spyLabel <- as.factor(bcw.trn.spy.PS$spyLabel)
bcw.trn.spy.US$spyLabel <- as.factor(bcw.trn.spy.US$spyLabel)

## I-EM from Liu, Lee, Yu, Li (2002)
nbc <- naiveBayes(
  spyLabel ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,
  data = (rbind(bcw.trn.spy.PS, bcw.trn.spy.US)),
  laplace = 0)

## Loop till predicted results converge
## 35 is a randomly chosen number that is sufficiently large enough
## because results converge quite fast for BCW data set
for (i in 1:35) {
  bcw.trn.spy.US$spyLabel <- predict(nbc, bcw.trn.spy.US[, bcw.features])
  temp <- predict(nbc, bcw.trn.spy.US[, bcw.features], type="raw")
  bcw.trn.spy.US$Pr  <- temp[,1]
  bcw.trn.spy.US$PrN <- temp[,2]
  
  ## build new nbc
  nbc <- naiveBayes(
    spyLabel ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,
    data = bcw.trn.spy.US,
    laplace = 0)
}

## Probability threshold
index <- which(bcw.trn.spy.US$isSpy, TRUE)
cnst.th <- min(bcw.trn.spy.US$Pr[index])

## RN documents <- Pr < cnst.th
bcw.trn.spy.NEGATIVE <- bcw.trn.spy.US[bcw.trn.spy.US$Pr < cnst.th, ]
bcw.trn.spy.POSITIVE <- bcw.trn.spy.US[bcw.trn.spy.US$Pr >= cnst.th, ]

## Convert factor to numeric for later use
bcw.trn.spy.NEGATIVE$spyLabel <- as.numeric(levels(bcw.trn.spy.NEGATIVE$spyLabel))[bcw.trn.spy.NEGATIVE$spyLabel]
bcw.trn.spy.POSITIVE$spyLabel <- as.numeric(levels(bcw.trn.spy.POSITIVE$spyLabel))[bcw.trn.spy.POSITIVE$spyLabel]

## Save for later use
bcw.trn.spy.PS$Pr <- 1
bcw.trn.spy.PS$PrN <- 0
bcw.trn.spy.NS <- bcw.trn.spy.NEGATIVE
bcw.trn.spy.US <- bcw.trn.spy.POSITIVE



################################################
####    Rocchio
################################################
## Copying data
bcw.trn.roc.PS <- bcw.trn.PS
bcw.trn.roc.US <- bcw.trn.US

BcwRocchioVectorBuilder <- function(DF1, DF2) {
  alpha <- 16
  beta <- 4
  
  ## Remove non-significant columns (e.g. id, class, rocLabel)
  DF1 <- DF1[ , bcw.features]
  DF2 <- DF2[ , bcw.features]
  
  norma.d.1 <- apply(DF1, 1, function(x){x/sqrt(sum(x^2))})
  DF1.size <- nrow(DF1)
  term.1 <- alpha * rowSums(norma.d.1)/DF1.size
  
  norma.d.2 <- apply(DF2, 1, function(x){x/sqrt(sum(x^2))})
  DF2.size <- nrow(DF2)
  term.2 <- beta * rowSums(norma.d.2)/DF2.size
  
  c <- term.1 + term.2
  return(c)
}

BcwRocchioClassifer <- function(DF.row, vector1, vector2) {
  ## Remove non-significant columns
  DF.row <- DF.row[ , bcw.features]
  
  r1 <- sum(vector1 * DF.row)
  r2 <- sum(vector2 * DF.row)
  
  if (r1 >= r2) {
    rocLabel <- 4
  } else {
    rocLabel <- 2
  }
  
  return(rocLabel)
}

rocchio.posVector <- BcwRocchioVectorBuilder(bcw.trn.roc.PS, bcw.trn.roc.US)
rocchio.negVector <- BcwRocchioVectorBuilder(bcw.trn.roc.US, bcw.trn.roc.PS)

bcw.trn.roc.US$rocLabel <- 0
for (i in 1:nrow(bcw.trn.roc.US)) {
  bcw.trn.roc.US[i,]$rocLabel <- BcwRocchioClassifer(bcw.trn.roc.US[i,], rocchio.posVector, rocchio.negVector)
}

## Extract data which is marked negative
bcw.trn.roc.NEGATIVE <- subset(bcw.trn.roc.US, rocLabel=="2")
bcw.trn.roc.POSITIVE <- subset(bcw.trn.roc.US, rocLabel=="4")



################################################
####    Find Reliably Negative
################################################

# bcw.trn.spy.NEGATIVE
# bcw.trn.roc.NEGATIVE

temp <- merge(bcw.trn.spy.NEGATIVE[ ,c(1:11, 13)], bcw.trn.roc.NEGATIVE[ ,c(1, 12)], by="id")
bcw.trn.step1.NS <- c()
for (i in 1:nrow(temp)) {
  if ((temp[i,]$spyLabel == 2) && (temp[i,]$rocLabel == 2)) {
    bcw.trn.step1.NS <- rbind(bcw.trn.step1.NS, temp[i, ])
  }
}

bcw.trn.step1.PS <- bcw.trn.PS
bcw.trn.step1.US <- subset(bcw.trn.US, !(bcw.trn.US$id %in% bcw.trn.step1.NS$id))



################################################
####    Clean up Global Env
################################################
#rm(bcw.trn.roc.POSITIVE, bcw.trn.roc.NEGATIVE) #, bcw.trn.roc.PS, bcw.trn.roc.US)   USED FOR Roc-SVM
#rm(bcw.trn.spy.POSITIVE, bcw.trn.spy.NEGATIVE) #, bcw.trn.spy.PS, bcw.trn.spy.US)   USED FOR Spy-EM
#rm(index, libs, V6.mean, nbc, rocchio.posVector, rocchio.negVector, temp.PS)

bcw.trn.step1.NS$rocLabel <- NULL
bcw.trn.step1.NS$spyLabel <- NULL



################################################
####    Clustering
################################################
cnst.t = 30
cnst.m = floor(cnst.t * nrow(bcw.trn.step1.NS) / (nrow(bcw.trn.step1.US) + nrow(bcw.trn.step1.NS)))

bcw.trn.step1.NS.fit <- kmeans(bcw.trn.step1.NS[, bcw.features], cnst.m)

## Label data with cluster number
bcw.trn.step1.NS <- data.frame(bcw.trn.step1.NS, bcw.trn.step1.NS.fit$cluster)
bcw.trn.step1.NS <- rename(bcw.trn.step1.NS, c("bcw.trn.step1.NS.fit.cluster" = "cluster"))



################################################
####    Build Representative Prototypes
################################################
temp <- matrix(0, ncol = 10, nrow = 0)
bcw.trn.step1.p.k <- data.frame(temp)
bcw.trn.step1.n.k <- data.frame(temp)

for (k in 1:cnst.m) {
  cluster.k <- subset(bcw.trn.step1.NS, cluster==k)
  
  p.k <- BcwRocchioVectorBuilder(bcw.trn.step1.PS, cluster.k)
  p.k <- c(k, p.k)
  bcw.trn.step1.p.k <- rbind(bcw.trn.step1.p.k, p.k)
  
  n.k <- BcwRocchioVectorBuilder(cluster.k, bcw.trn.step1.PS)
  n.k <- c(k, n.k)
  bcw.trn.step1.n.k <- rbind(bcw.trn.step1.n.k, n.k)
}
#rm(cluster.k, p.k, n.k)

names(bcw.trn.step1.p.k) <- c("k", bcw.features)
names(bcw.trn.step1.n.k) <- c("k", bcw.features)



################################################
####    Similarity Weight Generation
################################################
# Take value of 't' from above, t <- 30
cnst.r = floor(cnst.t * nrow(bcw.trn.step1.US) / (nrow(bcw.trn.step1.NS) + nrow(bcw.trn.step1.US)))

bcw.trn.step1.US.fit <- kmeans(bcw.trn.step1.US[, bcw.features], cnst.r)

## Label data with cluster number
bcw.trn.step1.US <- data.frame(bcw.trn.step1.US, bcw.trn.step1.US.fit$cluster)
bcw.trn.step1.US <- rename(bcw.trn.step1.US, c("bcw.trn.step1.US.fit.cluster" = "cluster"))



################################################
####    Weight Generation
################################################
BcwSimilarityValue <- function(DF.row, prototype1) {
  DF.row <- DF.row[ , bcw.features]
  prototype1 <- prototype1[ , bcw.features]
    
  norma.x   <- apply(DF.row, 1, function(x){sqrt(sum(x^2))})
  norma.p.k <- apply(prototype1, 1, function(x){sqrt(sum(x^2))})
  
  sim <- sum((DF.row * prototype1) / (norma.x * norma.p.k))
  
  return(sim)
}



################################################
####    Local Weight Generation
################################################
## Copy data
bcw.trn.localSPUL.US <- bcw.trn.step1.US
bcw.trn.localSPUL.PS <- bcw.trn.step1.PS
bcw.trn.localSPUL.NS <- bcw.trn.step1.NS

## Set up Local SPUL weights
bcw.trn.localSPUL.PS$m.plus  <- 1
bcw.trn.localSPUL.PS$m.minus <- 0

bcw.trn.localSPUL.NS$m.plus  <- 0
bcw.trn.localSPUL.NS$m.minus <- 1

bcw.trn.localSPUL.US$m.plus  <- -1
bcw.trn.localSPUL.US$m.minus <- -1

## For each cluster in US
for (j in 1:cnst.r) {
  cluster.j <- subset(bcw.trn.localSPUL.US, cluster==j)
  
  cluster.size <- nrow(cluster.j)
  cluster.lp <- 0
  cluster.ln <- 0
  
  ## For each document in cluster
  for (i in 1:nrow(cluster.j)) {
    cluster.j.pk <- numeric(0)
    cluster.j.nk <- numeric(0)
    
    ## Run this document with every positive vector
    for (k in 1:nrow(bcw.trn.step1.p.k)) { 
      temp <- BcwSimilarityValue(cluster.j[i, ], bcw.trn.step1.p.k[k, ])
      cluster.j.pk <- c(cluster.j.pk, temp)
    }
    
    ## Run this document with every negative vector
    for (k in 1:nrow(bcw.trn.step1.n.k)) { 
      temp <- BcwSimilarityValue(cluster.j[i, ], bcw.trn.step1.n.k[k, ])
      cluster.j.nk <- c(cluster.j.nk, temp)
    }
    
    ## Count number of documents that are closer to positive/negative prototypes
    if (max(cluster.j.pk) > max(cluster.j.nk)) {
      cluster.lp <- cluster.lp + 1
    } else {
      cluster.ln <- cluster.ln + 1
    }
  }
  
  ## Calculate m+ for entire cluster
  bcw.trn.localSPUL.US[bcw.trn.localSPUL.US$cluster==j, ]$m.plus  <- cluster.lp / cluster.size
  
  ## Calculate m- for entire cluster
  bcw.trn.localSPUL.US[bcw.trn.localSPUL.US$cluster==j, ]$m.minus <- cluster.ln / cluster.size
}

bcw.trn.localSPUL.US$cluster <- NULL
bcw.trn.localSPUL.NS$cluster <- NULL
bcw.trn.localSPUL <- rbind(bcw.trn.localSPUL.PS, bcw.trn.localSPUL.NS, bcw.trn.localSPUL.US)



################################################
####    Global Weight Generation
################################################
## Copy data
bcw.trn.globalSPUL.US <- bcw.trn.step1.US
bcw.trn.globalSPUL.PS <- bcw.trn.step1.PS
bcw.trn.globalSPUL.NS <- bcw.trn.step1.NS

## Set up Local SPUL weights
bcw.trn.globalSPUL.PS$m.plus  <- 1
bcw.trn.globalSPUL.PS$m.minus <- 0

bcw.trn.globalSPUL.NS$m.plus  <- 0
bcw.trn.globalSPUL.NS$m.minus <- 1

bcw.trn.globalSPUL.US$m.plus  <- -1
bcw.trn.globalSPUL.US$m.minus <- -1

## For each cluster in US
for (j in 1:cnst.r) {
  cluster.j <- subset(bcw.trn.globalSPUL.US, cluster==j)
  
  ## For each document in cluster
  for (i in 1:nrow(cluster.j)) {
    cluster.j.pk <- numeric(0)
    cluster.j.nk <- numeric(0)
    
    ## Run this document with every positive vector
    for (k in 1:nrow(bcw.trn.step1.p.k)) { 
      temp <- BcwSimilarityValue(cluster.j[i, ], bcw.trn.step1.p.k[k, ])
      cluster.j.pk <- c(cluster.j.pk, temp)
    }
    
    ## Run this document with every negative vector
    for (k in 1:nrow(bcw.trn.step1.n.k)) { 
      temp <- BcwSimilarityValue(cluster.j[i, ], bcw.trn.step1.n.k[k, ])
      cluster.j.nk <- c(cluster.j.nk, temp)
    }
    
    ## Calculate m+ for this document, save in globalSPUL
    temp <- sum(cluster.j.pk) / (sum(cluster.j.pk) + sum(cluster.j.nk))
    bcw.trn.globalSPUL.US[bcw.trn.globalSPUL.US$id == cluster.j[i, ]$id, ]$m.plus <- temp
    
    ## Calculate m- for this document, save in globalSPUL
    temp <- sum(cluster.j.nk) / (sum(cluster.j.pk) + sum(cluster.j.nk))
    bcw.trn.globalSPUL.US[bcw.trn.globalSPUL.US$id == cluster.j[i, ]$id, ]$m.minus <- temp
  }
}

# bcw.trn.globalSPUL.PS
# bcw.trn.globalSPUL.NS
# bcw.trn.globalSPUL.US


bcw.trn.globalSPUL.US$cluster <- NULL
bcw.trn.globalSPUL.NS$cluster <- NULL
bcw.trn.globalSPUL <- rbind(bcw.trn.globalSPUL.PS, bcw.trn.globalSPUL.NS, bcw.trn.globalSPUL.US)



################################################
####    Clean up Global Env
################################################
#rm(cluster.j, cluster.ln, cluster.lp, cluster.j.nk, cluster.j.pk, cluster.size)



################################################
####    Build SVM
################################################

svmp <- bcw.trn.globalSPUL.PS
svmu <- bcw.trn.globalSPUL.US
svmn <- bcw.trn.globalSPUL.NS


bcw.svm.s.positive <- rbind(svmp, svmu)
bcw.svm.s.negative <- rbind(svmn, svmu)
bcw.svm.s.star     <- rbind(svmp, svmn)



# SVMglobalSPUL
# SVMlocalSPUL




################################################
####    Spy-EM
################################################
## Put Spy documents back in PS
bcw.trn.SEM.PS <- rbind(bcw.trn.spy.PS, 
                        subset(bcw.trn.spy.US, isSpy == TRUE),
                        subset(bcw.trn.spy.NS, isSpy == TRUE))
bcw.trn.SEM.US <- subset(bcw.trn.spy.US, (isSpy == FALSE))
bcw.trn.SEM.NS <- subset(bcw.trn.spy.NS, (isSpy == FALSE))

## Fix labeling and columns
bcw.trn.SEM.PS$label <- 4
bcw.trn.SEM.PS$Pr  <- 1
bcw.trn.SEM.PS$PrN <- 0
bcw.trn.SEM.PS$isFixed <- TRUE

bcw.trn.SEM.NS$label <- 2
bcw.trn.SEM.NS$Pr  <- 0
bcw.trn.SEM.NS$PrN <- 1
bcw.trn.SEM.NS$isFixed <- FALSE

bcw.trn.SEM.US$label <- 0
bcw.trn.SEM.US$isFixed <- FALSE

##  Run EM algorithm to build final classifier
nbc <- naiveBayes(
  as.factor(label) ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,
  data = (rbind(bcw.trn.SEM.PS, bcw.trn.SEM.NS)),
  laplace = 0)

bcw.trn.SEM.data <- rbind(bcw.trn.SEM.PS, bcw.trn.SEM.NS, bcw.trn.SEM.US)
bcw.trn.SEM.data$isSpy <- NULL
bcw.trn.SEM.data$spyLabel <- NULL
for (i in 1:35) {
  bcw.trn.SEM.data$label <- predict(nbc, bcw.trn.SEM.data[, bcw.features])
  temp <- predict(nbc, bcw.trn.SEM.data[, bcw.features], type="raw")
  bcw.trn.SEM.data$Pr  <- temp[,1]
  bcw.trn.SEM.data$PrN <- temp[,2]
  
  ## PS does not change, reset it to correct values
  bcw.trn.SEM.data[bcw.trn.SEM.data$isFixed == TRUE, ]$label <- 4
  bcw.trn.SEM.data[bcw.trn.SEM.data$isFixed == TRUE, ]$Pr  <- 1
  bcw.trn.SEM.data[bcw.trn.SEM.data$isFixed == TRUE, ]$PrN <- 0
  
  ## build new nbc
  nbc <- naiveBayes(
    as.factor(label) ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,
    data = bcw.trn.SEM.data,
    laplace = 0)
}

bcw.trn.SEM.classifier <- nbc

## Cleaning up
rm(bcw.trn.SEM.PS, bcw.trn.SEM.NS, bcw.trn.SEM.US)



################################################
####    Roc-SVM
################################################
bcw.trn.RocSVM.PS <- bcw.trn.roc.PS
bcw.trn.RocSVM.US <- subset(bcw.trn.roc.US, rocLabel == 4)
bcw.trn.RocSVM.NS <- subset(bcw.trn.roc.US, rocLabel == 2)

bcw.trn.RocSVM.PS$label <- 4
bcw.trn.RocSVM.NS$label <- 2
bcw.trn.RocSVM.NS$rocLabel <- NULL
bcw.trn.RocSVM.US$rocLabel <- NULL
# bcw.trn.RocSVM.data <- rbind(bcw.trn.RocSVM.PS, bcw.trn.RocSVM.NS)

## Store as model.0 for comparison
bcw.trn.RocSVM.classifier.0 <- svm(label ~ V1+V2+V3+V4+V5+V6+V7+V8+V9, 
                              data = rbind(bcw.trn.RocSVM.PS, bcw.trn.RocSVM.NS),
                              type = "C-classification")
bcw.trn.RocSVM.classifier.i <- bcw.trn.RocSVM.classifier.0

bcw.trn.RocSVM.i <- 0
while (TRUE) {
  ## Count iterations
  bcw.trn.RocSVM.i <- bcw.trn.RocSVM.i + 1
  
  ## Retrieved negatively classified documents
  bcw.trn.RocSVM.US$label <- predict(bcw.trn.RocSVM.classifier.i, bcw.trn.RocSVM.US)
  bcw.trn.RocSVM.w <- bcw.trn.RocSVM.US[bcw.trn.RocSVM.US$label == 2, ]
  
  if (nrow(bcw.trn.RocSVM.w) == 0) {
    break
  } else {
    bcw.trn.RocSVM.US <- bcw.trn.RocSVM.US[bcw.trn.RocSVM.US$label == 4, ]
    bcw.trn.RocSVM.NS <- rbind(bcw.trn.RocSVM.NS, bcw.trn.RocSVM.w)
    
    ## Build another model
    bcw.trn.RocSVM.classifier.i <- svm(label ~ V1+V2+V3+V4+V5+V6+V7+V8+V9, 
                                  rbind(bcw.trn.RocSVM.PS, bcw.trn.RocSVM.NS),
                                  type = "C-classification")
  }
}

## Additional step: Use final classifier to check PS
bcw.trn.RocSVM.PS$svmlabel <- predict(bcw.trn.RocSVM.classifier.i, bcw.trn.RocSVM.PS)
bcw.trn.RocSVM.PSnegativeCount <- nrow(bcw.trn.RocSVM.PS[bcw.trn.RocSVM.PS$svmlabel == 2 , ])

## Selecting final classifier
if (bcw.trn.RocSVM.PSnegativeCount / nrow(bcw.trn.RocSVM.PS) > 0.05) {
  bcw.trn.RocSVM.classifier <- bcw.trn.RocSVM.classifier.0
} else {
  bcw.trn.RocSVM.classifier <- bcw.trn.RocSVM.classifier.i
}

## Cleaning up
#rm(bcw.trn.RocSVM.classifier.0, bcw.trn.RocSVM.classifier.i, bcw.trn.RocSVM.i, bcw.trn.RocSVM.w)
rm(bcw.trn.RocSVM.PS, bcw.trn.RocSVM.US, bcw.trn.RocSVM.NS, bcw.trn.RocSVM.PSnegativeCount)



################################################
####    F-measure
################################################
## True positives (TP) - Correctly labeled as positive
## True negatives (TN) - Correctly labeled as negative
## False positives (FP) - Positive labeled incorrectly
## False negatives (FN) - Negative labeled incorrectly
## Precision - P = TP/(TP+FP)
## Recall - R = TP/(TP+FN)
## F-score - F = (2 * P * R)/(P + R)
calculateF <- function(data) {
  TP <- nrow(data[(data$class == 4 & data$predict == 4), ])
  TN <- nrow(data[(data$class == 2 & data$predict == 2), ])
  FP <- nrow(data[(data$class == 2 & data$predict == 4), ])
  FN <- nrow(data[(data$class == 4 & data$predict == 2), ])
  
  Precision <- TP / (TP+FP)
  Recall    <- TP / (TP+FN)
  
  F <- (2 * Precision * Recall) / (Precision + Recall)
  return (F)
}


## Run for Spy-EM
bcw.tst.SEM.data <- bcw.tst
bcw.tst.SEM.data$predict <- predict(bcw.trn.SEM.classifier, bcw.tst[, bcw.features])
f.SEM <- c(f.SEM, calculateF(bcw.tst.SEM.data))

## Run for Roc-SVM
bcw.tst.RocSVM.data <- bcw.tst
bcw.tst.RocSVM.data$predict <- predict(bcw.trn.RocSVM.classifier, bcw.tst[, bcw.features])
f.RocSVM <- c(f.RocSVM, calculateF(bcw.tst.RocSVM.data))


## Closing bracket for looping 10 times to reduce sampling bias (Ctrl-F "rep10")
}



################################################
####    F-measure results
################################################
f.globalSPUL
f.localSPUL
f.SEM
f.RocSVM

f.raw <- data.frame(f.globalSPUL, f.localSPUL, f.SEM, f.RocSVM)
f.mean <- colMeans(f.raw)
