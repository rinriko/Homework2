# Ref: https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=628712&casa_token=9A9Ef7E32W8AAAAA:kbwv6a5cUnKB3wgyWaE62vXhdSnmuz8uNS5WOha4guH6uyKsQ8zPvpfgHnuLgD0FKJIYnwMW&tag=1
# Ref: http://www.scholarpedia.org/article/Eigenfaces
pca_eigenfaces <- function(data,
                           center = TRUE,
                           scale. = FALSE,
                           threshold_percent = 90,
                           showall = FALSE,
                           ...)
{
  chkDots(...)
  data <- as.matrix(data)
  scaled <- scale(data, center = center, scale = scale.)
  mean <- attr(scaled, "scaled:center")
  sc <- attr(scaled, "scaled:scale")
  
  # Calculate Covariance Matrix
  Covariance = scaled %*% t(scaled) / (nrow(scaled) - 1)
  #   Covariance = cov(scaled)
  # Calculate Correlation Matrix
  # Correlation = cor(scaled)
  # Calculate EigenVectors and EvenenValues
  eig          <- eigen(Covariance)
  Eigenvalues  <- eig$values
  Eigenvectors <- eig$vectors
  # dimnames(Eigenvectors) <-
  #   list(colnames(t(data)), paste0("PC", seq_len(ncol(Eigenvectors))))
  # Choosing components and forming a feature vector.
  prop.var <- Eigenvalues / sum(Eigenvalues)
  cum.var  <- cumsum(Eigenvalues) / sum(Eigenvalues)
  percentage  <-
    round(cumsum(Eigenvalues) / sum(Eigenvalues) * 100, digits = 2)
  ## we get rank at least one even for a 0 matrix.
  rank <- as.numeric(min(which(percentage >= threshold_percent)))
  Eigenfaces <- Eigenvectors
  scaling    <-
    diag(Eigenvalues ^ (-1 / 2)) / (sqrt(nrow(scaled) - 1))
  Eigenfaces <- t(scaled) %*% Eigenvectors %*% scaling
  if (!as.logical(showall)) {
    if (rank < ncol(Eigenvectors)) {
      if (rank == 1) {
        Eigenvectors <- matrix(Eigenvectors[, 1:rank])
        scaling    <-
          matrix(Eigenvalues[1:rank] ^ (-1 / 2)) / (sqrt(nrow(scaled) - 1))
        
        Eigenfaces <- t(scaled) %*% Eigenvectors %*% scaling
      }
      else{
        scaling    <-
          diag(Eigenvalues[1:rank] ^ (-1 / 2)) / (sqrt(nrow(scaled) - 1))
        
        Eigenfaces <-
          t(scaled) %*% Eigenvectors[, 1:rank] %*% scaling
      }
      
      # print(Eigenvectors)
      # print(scaling)
      # print(Eigenvectors %*% scaling)
      # print(t(scaled) %*% Eigenvectors %*% scaling)
    }
  }
  # print(dim(scaled))
  # print(dim(Eigenfaces))
  finalData = scaled %*% Eigenfaces
  
  # print(dim(finalData))
  # print(dim(scaled))
  # print(dim(Eigenfaces))
  # print(as.data.frame(finalData))
  r <-
    list(
      eigenvalues = Eigenvalues,
      eigenvectors = Eigenvectors,
      eigenfaces = Eigenfaces,
      averagefaces = mean,
      finalData = as.data.frame(finalData),
      covariance = Covariance,
      # correlation = Correlation,
      center = if (is.null(mean))
        FALSE
      else
        mean,
      scale = if (is.null(sc))
        FALSE
      else
        sc,
      summary = as.data.frame(summary),
      summary.cumvar = cum.var,
      summary.propvar = prop.var,
      summary.percent = cum.var * 100
      
    )
  r
}