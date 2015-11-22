libs <- c("e1071", "caret")
install.packages(libs, dependencies=TRUE)
lapply(libs, require, character.only=TRUE)


################################################
####    Set Directory (please modify)
################################################
setwd("C:/Users/DongWei/Documents/Projects/data-scooping/data/breast-cancer-wisconsin")





################################################
#### Create Utility functions
################################################




################################################
####    Loading the data
################################################
bcw <- read.table("breast-cancer-wisconsin.data", sep=",")
bcw.headers <- c(
  "id", "V1", 
  "V2", "V3", 
  "V4", "V5",
  "V6", "V7",
  "V8", "V9",
  "class"
)
names(bcw) <- bcw.headers

## V6 has missing values.
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


## Replace missing values with mean
bcw$V6[index] <- V6.mean

################################################
####    Splitting the data
################################################
partitionIndex <- createDataPartition(
                    bcw$class, 
                    times = 1,
                    p = 0.6,
                    list = FALSE)

## 60% is set as training set, 40% for test set
bcw.trn <- bcw[partitionIndex, ]
bcw.tst <- bcw[-partitionIndex, ]


## Class "4" (malignant) is the positive set
bcw.trn.positive <- subset(bcw.trn, class=="4")
bcw.trn.negative <- subset(bcw.trn, class=="2")

## Value of g is taken from page 1581 of this paper
g = 0.15
partitionIndex <- createDataPartition(
                    bcw.trn.positive$class, 
                    times = 1,
                    p = g,
                    list = FALSE)

## Set up PS and US
bcw.trn.PS <- bcw.trn.positive[partitionIndex, ]
bcw.trn.US <- bcw.trn.positive[-partitionIndex, ]
bcw.trn.US <- rbind(bcw.trn.US, bcw.trn.negative)



################################################
####    Spy-EM
################################################
## Copying data
bcw.trn.Spy.PS <- bcw.trn.PS
bcw.trn.Spy.US <- bcw.trn.US

bcw.trn.Spy.PS$isSpy <- FALSE
bcw.trn.Spy.US$isSpy <- FALSE

## Value of s is taken from page 3 of Liu, Dai, Li, Lee, Yu (2003)
s = 0.15
partitionIndex <- createDataPartition(
                    bcw.trn.Spy.PS$class, 
                    times = 1,
                    p = s,
                    list = FALSE)

## Put spies into US
temp <- bcw.trn.Spy.PS[partitionIndex, ]
temp$isSpy <- TRUE
bcw.trn.Spy.US <- rbind(temp,bcw.trn.Spy.US)
bcw.trn.Spy.PS <- bcw.trn.Spy.PS[-partitionIndex, ]

bcw.trn.Spy.PS$spyLabel <- 4
bcw.trn.Spy.US$spyLabel <- 2
bcw.trn.Spy.PS$spyLabel <- as.factor(bcw.trn.Spy.PS$spyLabel)
bcw.trn.Spy.US$spyLabel <- as.factor(bcw.trn.Spy.US$spyLabel)

## I-EM from Liu, Lee, Yu, Li (2002)
nbc <- naiveBayes(
  spyLabel ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,
  data = (rbind(bcw.trn.Spy.PS, bcw.trn.Spy.US)),
  laplace = 0)

## Loop till predicted results converge
## 35 is a randomly chosen number that is sufficiently large enough
## because results converge quite fast for BCW data set
for (i in 1:35) {
  bcw.trn.Spy.US$spyLabel <- predict(nbc, bcw.trn.Spy.US[, 2:10])
  temp <- predict(nbc, bcw.trn.Spy.US[, 2:10], type="raw")
  bcw.trn.Spy.US$Pr  <- temp[,1]
  bcw.trn.Spy.US$PrN <- temp[,2]
  
  ## build new nbc
  nbc <- naiveBayes(
    spyLabel ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,
    data = bcw.trn.Spy.US,
    laplace = 0)
}

## Probability threshold
index <- which(bcw.trn.Spy.US$isSpy, TRUE)
th <- min(bcw.trn.Spy.US$Pr[index])

## RN documents <- Pr < th
bcw.trn.sem.NEGATIVE <- bcw.trn.Spy.US[bcw.trn.Spy.US$Pr < th, ]

## Convert factor to numeric for later use

## Convert factor to numeric
bcw.trn.sem.NEGATIVE$spyLabel <- as.numeric(levels(bcw.trn.sem.NEGATIVE$spyLabel))[bcw.trn.sem.NEGATIVE$spyLabel]


################################################
####    Rocchio
################################################
## Copying data
bcw.trn.roc.PS <- bcw.trn.PS
bcw.trn.roc.US <- bcw.trn.US

CVector <- function(DF.1, DF.2) {
  alpha <- 16
  beta <- 4
  
  norma.d.1 <- apply(DF.1, 1, function(x){x/sqrt(sum(x^2))})
  DF.1.size <- nrow(DF.1)
  term.1 <- alpha * rowSums(norma.d.1)/DF.1.size
  
  norma.d.2 <- apply(DF.2, 1, function(x){x/sqrt(sum(x^2))})
  DF.2.size <- nrow(DF.2)
  term.2 <- beta * rowSums(norma.d.2)/DF.2.size
  
  c <- term.1 + term.2
  return(c)
}

LabelData <- function(d.1, c.1, c.2) {
  r.1 <- sum(c.1 * d.1)
  r.2 <- sum(c.2 * d.1)
  
  if (r.1 >= r.2) {
    rocLabel <- 4
  } else {
    rocLabel <- 2
  }
  
  return(rocLabel)
}

rocchio.positiveVector <- CVector(bcw.trn.roc.PS, bcw.trn.roc.US)
rocchio.negativeVector <- CVector(bcw.trn.roc.US, bcw.trn.roc.PS)

bcw.trn.roc.US$rocLabel <- 0
for (i in 1:nrow(bcw.trn.roc.US)) {
  bcw.trn.roc.US[i,]$rocLabel <- LabelData(bcw.trn.roc.US[i,], rocchio.positiveVector, rocchio.negativeVector)
}

## Extract data which is marked negative
bcw.trn.roc.NEGATIVE <- subset(bcw.trn.roc.US, rocLabel=="2")
bcw.trn.roc.POSITIVE <- subset(bcw.trn.roc.US, rocLabel=="4")



################################################
####    Find Reliably Negative
################################################

# bcw.trn.sem.NEGATIVE
# bcw.trn.roc.NEGATIVE

temp <- merge(bcw.trn.sem.NEGATIVE[ ,c(1:11, 13)],bcw.trn.roc.NEGATIVE[ ,c(1, 12)], by="id")
bcw.trn.NS <- c()
bcw.trn.US <- c()
for (i in 1:nrow(temp)) {
  if ((temp[i,]$spyLabel == 2) && (temp[i,]$rocLabel == 2)) {
    bcw.trn.NS <- rbind(bcw.trn.NS, temp[i, ])
  } else {
    bcw.trn.US <- rbind(bcw.trn.US, temp[i, ])
  }
}



################################################
####    Clustering
################################################
t = 30
m = t * nrow(bcw.trn.NS) / (nrow(bcw.trn.US) + nrow(bcw.trn.NS))

bcw.cluster <- kmeans(bcw.trn.US, m)
