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
bcw.featureHeaders <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9")
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
V6.mean <- temp / length(bcw$V6)


## Replace missing V6 values with mean
bcw$V6[index] <- V6.mean



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
g = 0.15
temp <- createDataPartition(
                    bcw.trn.positive$class, 
                    times = 1,
                    p = g,
                    list = FALSE)

## Set up PS and US
bcw.trn.PS <- bcw.trn.positive[temp, ]
bcw.trn.US <- bcw.trn.positive[-temp, ]
bcw.trn.US <- rbind(bcw.trn.US, bcw.trn.negative)

## Delete variables that are never again used
## Prevents confusion in Global Env
rm(bcw.trn.positive, bcw.trn.negative)



################################################
####    Spy-EM
################################################
## Copying data
bcw.trn.spy.PS <- bcw.trn.PS
bcw.trn.spy.US <- bcw.trn.US

bcw.trn.spy.PS$isSpy <- FALSE
bcw.trn.spy.US$isSpy <- FALSE

## Value of s is taken from page 3 of Liu, Dai, Li, Lee, Yu (2003)
s = 0.15
temp <- createDataPartition(
                    bcw.trn.spy.PS$class, 
                    times = 1,
                    p = s,
                    list = FALSE)

## Put spies into US
temp <- bcw.trn.spy.PS[temp, ]
temp$isSpy <- TRUE
bcw.trn.spy.US <- rbind(temp,bcw.trn.spy.US)
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
  bcw.trn.spy.US$spyLabel <- predict(nbc, bcw.trn.spy.US[, 2:10])
  temp <- predict(nbc, bcw.trn.spy.US[, 2:10], type="raw")
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
th <- min(bcw.trn.spy.US$Pr[index])

## RN documents <- Pr < th
bcw.trn.spy.NEGATIVE <- bcw.trn.spy.US[bcw.trn.spy.US$Pr < th, ]
bcw.trn.spy.POSITIVE <- bcw.trn.spy.US[bcw.trn.spy.US$Pr >= th, ]

## Convert factor to numeric for later use

## Convert factor to numeric
bcw.trn.spy.NEGATIVE$spyLabel <- as.numeric(levels(bcw.trn.spy.NEGATIVE$spyLabel))[bcw.trn.spy.NEGATIVE$spyLabel]
bcw.trn.spy.POSITIVE$spyLabel <- as.numeric(levels(bcw.trn.spy.POSITIVE$spyLabel))[bcw.trn.spy.POSITIVE$spyLabel]



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
  DF1 <- DF1[ , bcw.featureHeaders]
  DF2 <- DF2[ , bcw.featureHeaders]
  
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
  DF.row <- DF.row[ , bcw.featureHeaders]
  
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
rm(bcw.trn.roc.POSITIVE, bcw.trn.roc.NEGATIVE, bcw.trn.roc.PS, bcw.trn.roc.US)
rm(bcw.trn.spy.POSITIVE, bcw.trn.spy.NEGATIVE, bcw.trn.spy.PS, bcw.trn.spy.US)
rm(index, libs, V6.mean, nbc, rocchio.posVector, rocchio.negVector)



################################################
####    Clustering
################################################
t = 30
m = floor(t * nrow(bcw.trn.step1.NS) / (nrow(bcw.trn.step1.US) + nrow(bcw.trn.step1.NS)))

bcw.trn.step1.NS.fit <- kmeans(bcw.trn.step1.NS[, bcw.featureHeaders], m)

## Label data with cluster number
bcw.trn.step1.NS <- data.frame(bcw.trn.step1.NS, bcw.trn.step1.NS.fit$cluster)
bcw.trn.step1.NS <- rename(bcw.trn.step1.NS, c("bcw.trn.step1.NS.fit.cluster" = "cluster"))



################################################
####    Build Representative Prototypes
################################################
temp <- matrix(0, ncol = 10, nrow = 0)
bcw.trn.step1.p.k <- data.frame(temp)
bcw.trn.step1.n.k <- data.frame(temp)

for (k in 1:m) {
  cluster.k <- subset(bcw.trn.step1.NS, cluster==k)
  
  p.k <- BcwRocchioVectorBuilder(bcw.trn.step1.PS, cluster.k)
  p.k <- c(k, p.k)
  bcw.trn.step1.p.k <- rbind(bcw.trn.step1.p.k, p.k)
  
  n.k <- BcwRocchioVectorBuilder(cluster.k, bcw.trn.step1.PS)
  n.k <- c(k, n.k)
  bcw.trn.step1.n.k <- rbind(bcw.trn.step1.n.k, n.k)
}
rm(cluster.k, p.k, n.k)

names(bcw.trn.step1.p.k) <- c("k", bcw.featureHeaders)
names(bcw.trn.step1.n.k) <- c("k", bcw.featureHeaders)

