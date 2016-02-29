bcw.rocchioVectorBuilder <- function(DF1, DF2) {
  alpha <- 16
  beta <- 4

  norma.d.1 <- apply(DF1, 1, function(x){x/sqrt(sum(x^2))})
  DF1.size <- nrow(DF1)
  term.1 <- alpha * rowSums(norma.d.1)/DF1.size

  norma.d.2 <- apply(DF2, 1, function(x){x/sqrt(sum(x^2))})
  DF2.size <- nrow(DF2)
  term.2 <- beta * rowSums(norma.d.2)/DF2.size

  c <- term.1 + term.2
  return(c)
}


bcw.rocchioClassifer <- function(DF.row, vector1, vector2) {
  r1 <- sum(vector1 * DF.row)
  r2 <- sum(vector2 * DF.row)

  if (r1 >= r2) {
    rocLabel <- 4
  } else {
    rocLabel <- 2
  }

  return(rocLabel)
}