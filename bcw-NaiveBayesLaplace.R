library("e1071")


bcw.getNaiveBayesLaplaceClassifier <- function(bcw.PS, bcw,US) {
  bcw.PS$label <- 4
  bcw.US$label <- 2

  classifier.nb <- naiveBayes(
    as.factor(label) ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,
    data = (rbind(bcw.PS, bcw.US)),
    laplace = 0.1)

  return (classifier.nb)
}
