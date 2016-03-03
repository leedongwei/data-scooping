library("e1071")


ngp.getNaiveBayesClassifier <- function(ngp.PS, ngp.US) {
  ngp.PS$label <- 1
  ngp.US$label <- -1

  ngp.train <- rbind(ngp.PS, ngp.US)

  classifier.nb <- naiveBayes(
    as.matrix(ngp.train),
    as.factor(ngp.train$label),
    laplace = 0.1)

  return (classifier.nb)
}
