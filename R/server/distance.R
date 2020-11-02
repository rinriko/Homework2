distance <- function(x1, x2, type = 1) {
  x1 <- matrix(x1, length(x1), 1)
  x2 <- matrix(x2, length(x2), 1)
#   Euclidean (type=1)
  if (type == 1) {
    sqrt(t(x1 - x2)%*%(x1 - x2))
  } 
#   Mahalanobis (type=2)
  else if (type == 2) {
    m <- x1/sqrt(eigenvalues[1:length(x1)])
    n <- x2/sqrt(eigenvalues[1:length(x1)])
    sqrt(t(m - n)%*%(m - n))
  
  } 
#   Manhattan (type=3)
  else if (type == 3) {
    sum(abs(x1 - x2))
  }
}