numCenters <- 2
iter.max <- 3
norm <- function(x) {sqrt(sum(x^2))}

ngp.model.KMeans <- function(ngp.dtm, ngp.class, numCenters, iter.max) {

  numDocs <- nrow(ngp.dtm)
  numFeatures <- ncol(ngp.dtm)
  hasConverged <- FALSE

  ## Randomly choose k documents to use as starting centers
  ## Sample again if there are 2 documents with identical values
  cluster.centers <- ngp.dtm[sample.int(numDocs, numCenters, replace = FALSE), ]
  while(ngp.model.KMeans.hasRepeatCenters(cluster.centers)) {
    cluster.centers <- ngp.dtm[sample.int(numDocs, numCenters, replace = FALSE), ]
  }
  
  ## Find the center that each doc is closest to
  cluster.docs <- ngp.model.KMeans.modelPredict.initial(ngp.dtm, cluster.centers)
  ngp.class$cluster <- cluster.docs
  
  var.iter <- 0
  while(!hasConverged) {
    var.iter <- var.iter + 1
    cluster.docs.prev <- cluster.docs
    cluster.centers.prev <- cluster.centers
    
    utils.cat(paste("        KMeans:  thread #", var.j, ", iteration #", var.iter, "\n", sep=""))
    
    ## Re-calculate average for each center and update it
    for (i in 1:numCenters) {
      L.i <- rownames(ngp.class[ngp.class$cluster == i & ngp.class$label == 1, ])
      X.i <- rownames(ngp.class[ngp.class$cluster == i, ])
      L.i.dtm <- if (length(L.i)) ngp.dtm[L.i, ] else 0
      X.i.dtm <- if (length(X.i)) ngp.dtm[X.i, ] else 0
      
      ## Calculate Imp
      ADC.i <- (norm(L.i.dtm))^2 * ineq(X.i.dtm, type="Gini")
      Imp   <- ADC.i * entropy(X.i.dtm)
      
      X.L.i <- 0
      X.X.i <- 0
      if (length(L.i) > 0) {
        if (length(L.i) == 1) {
          X.L.i <- L.i.dtm
        } else {
          X.L.i <- apply(L.i.dtm, 2, function(x) sum(x))
        }
      }
      if (length(X.i) > 0) {
        if (length(X.i) == 1) {
          X.X.i <- X.i.dtm
        } else {
          X.X.i <- apply(X.i.dtm, 2, function(x) sum(x))
        }
      }
   
      cluster.centers[i, ] <- (X.X.i + X.L.i) / (norm(X.i.dtm) + norm(L.i.dtm) * Imp)
    }
    
    ## Find the center that each doc is closest to
    cluster.docs <- ngp.model.KMeans.modelPredict(ngp.dtm, ngp.class, cluster.centers)
    ngp.class$cluster <- cluster.docs
    
    
    if (var.iter == iter.max) {
      utils.cat(paste("            KMeans:  thread #", var.j, ", did not converge before iter.max"))
      break
    }
    if (identical(cluster.docs.prev, cluster.docs)) {
      hasConverged <- TRUE
    }
  }
  utils.cat(paste("        KMeans:  thread #", var.j, ", break at #", var.iter, "\n", sep=""))
  
  ngp.class$cluster <- cluster.docs
  return(list(cluster.centers, ngp.dtm, ngp.class))
}

ngp.model.KMeans.modelPredict.initial <- function (ngp.dtm, ngp.centers) {
  apply(ngp.dtm, 1, function(x) {
    dist.centers <- apply(ngp.centers, 1, function(u) {
      distance <- sum((x-u)^2)
    })
    return(which.min(dist.centers))
  })
}

ngp.model.KMeans.modelPredict <- function (ngp.dtm, ngp.class, ngp.centers) {
  docs.all <- rownames(ngp.dtm)
  numCenters <- nrow(ngp.centers)
  
  ## calculate Imp for all clusters
  Imp.i <- apply(as.matrix(1:numCenters), 1, function(i) {
    L.i <- rownames(ngp.class[ngp.class$cluster == i & ngp.class$label == 1, ])
    X.i <- rownames(ngp.class[ngp.class$cluster == i, ])
    L.i.dtm <- if (length(L.i)) ngp.dtm[L.i, ] else 0
    X.i.dtm <- if (length(X.i)) ngp.dtm[X.i, ] else 0
    
    ## Calculate Imp
    ADC.i <- (norm(L.i.dtm))^2 * ineq(X.i.dtm, type="Gini")
    Imp   <- ADC.i * entropy(X.i.dtm)
    return(Imp)
  })
  
  
  apply(as.matrix(docs.all), 1, function(x) {
    dist.centers <- apply(ngp.centers, 1, function(u) {
      distance <- sum((ngp.dtm[x, ] - u)^2)
    })
    
    if (ngp.class[x, "label"] == 1) {
      impurityFun <- dist.centers * Imp.i
    } else {
      impurityFun <- 0
    }
    
    return(which.min(dist.centers + impurityFun))
  })
}

ngp.model.KMeans.generateRandomCenters <- function (ngp.dtm, numCenters) {
  maxValue <- apply(ngp.dtm, 2, function(x) max(x))
  randomCenters <- sapply(maxValue, function(x) sample(0:x, numCenters, replace=TRUE))
  return(randomCenters)
}

ngp.model.KMeans.hasRepeatCenters <- function (cluster.centers) {
  for (i in 1:(nrow(cluster.centers) - 1)) {
    for (j in (i+1):nrow(cluster.centers)) {
      if (identical(cluster.centers[i, ], cluster.centers[j, ])) {
        return (TRUE)
      }
    }
  }
  return (FALSE)
}



ngp.model.KMeans.predict <- function (ngp.model, ngp.dtm) {
  cluster.centers <- ngp.model[[1]]
  ngp.trnDtm <- ngp.model[[2]]
  ngp.class <- ngp.model[[3]]
  
  utils.cat(paste("        KMeans.predict:  thread #", var.j, "\n", sep=""))
  
  ## Identify positive cluster
  cluster.Pcount <- apply(as.matrix(1:nrow(cluster.centers)), 1, function(i) {
    L.i <- rownames(ngp.class[ngp.class$cluster == i & ngp.class$label == 1, ])
    X.i <- rownames(ngp.class[ngp.class$cluster == i, ])
    Pcount <- length(L.i) / length(X.i)
    return (if(is.na(Pcount)) 0 else Pcount)
  })
  cluster.positive <- which(cluster.Pcount == max(cluster.Pcount))
  
  
  ## calculate Imp for all clusters
#   Imp.i <- apply(as.matrix(1:numCenters), 1, function(i) {
#     L.i <- rownames(ngp.class[ngp.class$cluster == i & ngp.class$label == 1, ])
#     X.i <- rownames(ngp.class[ngp.class$cluster == i, ])
#     L.i.dtm <- if (length(L.i) > 0) ngp.trnDtm[L.i, ] else 0
#     X.i.dtm <- if (length(X.i) > 0) ngp.trnDtm[X.i, ] else 0
#     
#     ## Calculate Imp
#     ADC.i <- (norm(L.i.dtm))^2 * ineq(X.i.dtm, type="Gini")
#     Imp   <- ADC.i * entropy(X.i.dtm)
#     return(Imp)
#   })
  
  docs.all <- rownames(ngp.dtm)
  docs.predict <- apply(as.matrix(docs.all), 1, function(x) {
    dist.centers <- apply(cluster.centers, 1, function(u) {
      distance <- sum((ngp.dtm[x, ] - u)^2)
      
      #TODO: Dongwei consider impurity on test document
      distance <- distance + distance * Imp
    })
    return(which.min(dist.centers))
  })
  
  docs.output <- rep(0, length(docs.predict))
  for (i in 1:length(docs.predict)) {
    if (docs.predict[i] == cluster.positive) {
      docs.output[i] <- 1
    } else {
      docs.output[i] <- -1
    }
  }
  
  return(as.factor(docs.output))
}
