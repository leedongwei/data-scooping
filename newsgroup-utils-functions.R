utils.createParallelCluster <- function() {
  cat("Creating parallel cluster...\n")
  numberOfCores <- max(1, detectCores() - 1)
  cluster <- makeCluster(numberOfCores,
                         outfile = "")

  utils.cat(paste("Created parallel cluster with ", numberOfCores, " cores\n", sep=""))
  return(cluster)
}

utils.convertFactorToNumeric <- function(factor) {
  as.numeric(levels(factor))[factor]
}

utils.plotGraph <- function(ngp.results, nameOfGraph) {
  xrange <- range(rownames(ngp.results))
  yrange <- range(c(min(ngp.results), max(ngp.results)))

  plot(xrange, yrange, type = "n", xlab = "% of training set", ylab=nameOfGraph)
  colors <- rainbow(ncol(ngp.results))
  linetype <- c(1:ncol(ngp.results))
  plotchar <- seq(18,18+length(rownames(ngp.results)),1)

  for (j in 1:ncol(ngp.results)) {
      singleCol <- ngp.results[,j]
      lines(rownames(ngp.results), as.numeric(singleCol), type="b", lwd=1.5,
            lty=linetype[j], col=colors[j], pch=plotchar[j])
  }

  legend("bottomright", colnames(ngp.results), cex=0.8, col=colors,
         pch=plotchar, lty=linetype, title=paste(nameOfGraph, "Graph"))
}

utils.cat <- function(message) {
  cat(message, file="newsgroup-output.txt", append=TRUE)
}