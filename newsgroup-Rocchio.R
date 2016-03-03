ngp.model.rocchioVectorBuilder <- function(DF1, DF2) {
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


ngp.model.rocchioClassifer <- function(DF.row, vector1, vector2) {
  r1 <- sum(vector1 * DF.row)
  r2 <- sum(vector2 * DF.row)

  if (r1 >= r2) {
    rocLabel <- 1
  } else {
    rocLabel <- -1
  }

  return(rocLabel)
}