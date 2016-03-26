numCenters <- 10
iter.max <- 10

kmeans <- function(ngp.trnMatrix, ngp.class, numCenters, iter.max) {

  numDocs <- nrow(ngp.trnMatrix)
  numFeatures <- ncol(ngp.trnMatrix)
  hasConverged <- FALSE
  
  ## Randomly choose k documents to use as starting centers
  cluster.centers <- ngp.trnMatrix[sample.int(numDocs, numCenters), ]
  
  ## Find the center that each doc is closest to
  cluster.docs <- apply(ngp.trnMatrix, 1, function(x) {
    dist.centers <- apply(centers, 1, function(u) sum((x-u)^2))
    return(which.min(dist.centers))
  })
  
  iter.count <- 0
  while(!hasConverged) {
    iter.count <- iter.count + 1
    cluster.docs.prev <- cluster.docs

    ## Re-calculate average for each center and update it
    for (i in 1:10) {
      temp = names(cluster.docs[cluster.docs == i])
      temp = ngp.trnMatrix[temp, ]
      
      cluster.centers[i, ] <- apply(temp, 2, function(column) mean(column))
    }
    
    
    ## Find the center that each doc is closest to
    cluster.docs <- apply(ngp.trnMatrix, 1, function(x) {
      dist.centers <- apply(centers, 1, function(u) sum((x-u)^2))
      return(which.min(dist.centers))
    })
    
    if (iter.count == iter.max) {
      utils.cat(paste("        K-Means did not converge before iter.max"))
      print("NOT CONVERGED")
      break
    }
    
    if (identical(cluster.docs.prev, cluster.docs)) {
      print("CONVERGED")
      print(iter.count)
      hasConverged <- TRUE
    }
  }
}