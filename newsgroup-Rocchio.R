ngp.model.Rocchio <- function (ngp.dtm, ngp.class) {

  PS <- rownames(ngp.class[ngp.class$label == 1, ])
  US <- rownames(ngp.class[ngp.class$label == -1, ])

  ngp.positiveVector <- ngp.model.Rocchio.RocchioVectorBuilder(ngp.dtm[PS, ], ngp.dtm[US, ])
  ngp.negativeVector <- ngp.model.Rocchio.RocchioVectorBuilder(ngp.dtm[US, ], ngp.dtm[PS, ])

  model.Rocchio <- list(positive=ngp.positiveVector, negative=ngp.negativeVector)

  return(model.Rocchio)
}


ngp.model.RocchioClassifer <- function(model.Rocchio, ngp.dtm) {

  rocResults <- apply(ngp.dtm, 1, function(doc) {
    r1 <- sum(model.Rocchio[["positive"]] * doc)
    r2 <- sum(model.Rocchio[["negative"]] * doc)

    if (r1 >= r2) {
      rocLabel <- 1
    } else {
      rocLabel <- -1
    }
  })

  return(as.factor(rocResults))
}

ngp.model.Rocchio.RocchioVectorBuilder <- function(DF1, DF2) {
  alpha <- 16
  beta <- 4

  norma.d.1 <- apply(DF1, 1, function(x){x/sqrt(sum(x^2))})
  norma.d.1 <- apply(norma.d.1, 1:2, function(x){if(is.na(x)){0}else{x}})
  DF1.size <- nrow(DF1)
  term.1 <- alpha * rowSums(norma.d.1)/DF1.size

  norma.d.2 <- apply(DF2, 1, function(x){x/sqrt(sum(x^2))})
  norma.d.2 <- apply(norma.d.2, 1:2, function(x){if(is.na(x)){0}else{x}})
  DF2.size <- nrow(DF2)
  term.2 <- beta * rowSums(norma.d.2)/DF2.size

  c <- term.1 + term.2
  return(c)
}



