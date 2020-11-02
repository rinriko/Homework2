eigenfaces <- function(data,
                center = TRUE,
                scale. = FALSE,
                threshold_percent = 90,
                ...)
{
  chkDots(...)
  data <- na.omit(data[,sapply(data,is.numeric)])
  data <- as.matrix(data)
  scaled <-
    scale(data, center = center, scale = scale.)
  mean <- attr(scaled, "scaled:center")
  sc <- attr(scaled, "scaled:scale")
  
  # Calculate Covariance Matrix
  Covariance = t(scaled)%*%(scaled) / (nrow(scaled)-1)
#   Covariance = cov(scaled)
  # Calculate Correlation Matrix
  Correlation = cor(scaled)
  # Calculate EigenVectors and EvenenValues
  eig          <- eigen(Covariance)
  Eigenvalues  <- eig$values
  Eigenvectors <- eig$vectors
  dimnames(Eigenvectors) <-
    list(colnames(data), paste0("PC", seq_len(ncol(Eigenvectors))))
  # Choosing components and forming a feature vector.
  prop.var <- Eigenvalues / sum(Eigenvalues)
  cum.var  <- cumsum(Eigenvalues) / sum(Eigenvalues)
  percentage  <-
    round(cumsum(Eigenvalues) / sum(Eigenvalues) * 100, digits = 2)
  
  Eigenfaces <- Eigenvectors
  if (!showall) {
    ## we get rank at least one even for a 0 matrix.
    rank <- min(which(percentage >= threshold_percent))
    if (rank < ncol(data)) {
      Eigenfaces <- Eigenfaces[, 1L:rank]
    }
  }
  # Deriving the new data set.
  finalData = scaled %*% Eigenfaces

  r <-
    list(
        sdev = sqrt(Eigenvalues), 
        rotation = Eigenvectors,
      x = as.matrix(finalData),
      eigenvalues = Eigenvalues,
      eigenvectors = Eigenvectors,
      eigenfaces = Eigenfaces,
      finalData = as.data.frame(finalData),
      covariance = Covariance,
      correlation = Correlation,
      center = if (is.null(mean))
        FALSE
      else
        mean,
      scale = if (is.null(sc))
        FALSE
      else
        sc
      
    )
  r
}