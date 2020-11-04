# Ref: https://rpubs.com/dherrero12/543854
# Ref: https://aaronschlegel.me/principal-component-analysis-r-example.html
# Ref: https://github.com/RInterested/PCA/blob/master/PCA%20SEMICONDUCTORS
# Ref: https://stats.stackexchange.com/questions/134282/relationship-between-svd-and-pca-how-to-use-svd-to-perform-pca
# Ref: https://stats.stackexchange.com/questions/2691/making-sense-of-principal-component-analysis-eigenvectors-eigenvalues
# Ref: http://www.statistics4u.com/fundstat_eng/cc_pca_loadscore.html#:~:text=The%20matrix%20V%20is%20usually,in%20a%20rotated%20coordinate%20system.
# Ref: https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/prcomp.R
# Ref: http://statmath.wu.ac.at/~hornik/QFS1/principal_component-vignette.pdf
# Ref: http://www.stat.ucla.edu/~rgould/252w02/moreprincomp.pdf


pca <- function(data,
                center = TRUE,
                scale. = FALSE,
                threshold_percent = 90,
                showall = FALSE,
                ...)
{
  chkDots(...)
  data <- na.omit(data[, sapply(data, is.numeric)])
  data <- as.matrix(data)
  scaled <-
    scale(data, center = center, scale = scale.)
  mean <- attr(scaled, "scaled:center")
  sc <- attr(scaled, "scaled:scale")
  
  # Calculate Covariance Matrix
  Covariance = t(scaled) %*% (scaled) / (nrow(scaled) - 1)
  #   Covariance = cov(scaled)
  # Calculate Correlation Matrix
  # Correlation = cor(scaled)
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
  
  featureVector <- Eigenvectors
  if (!as.logical(showall)) {
    ## we get rank at least one even for a 0 matrix.
    rank <- min(which(percentage >= threshold_percent))
    if (rank < ncol(data)) {
      featureVector <- featureVector[, 1:rank]
    }
  }
  # Deriving the new data set.
  finalData = scaled %*% featureVector
  dimnames(finalData) <-
    list(seq_len(nrow(finalData)), paste0("PC", seq_len(ncol(finalData))))
  
  # Summary
  summary <- rbind(
    "Standard deviation" = sqrt(Eigenvalues),
    "Proportion of Variance" = prop.var,
    "Cumulative Proportion" = cum.var,
    "Percentage Cumulative" = cum.var * 100
  )
  colnames(summary) <- paste0("PC", seq_len(ncol(summary)))
  
  r <-
    list(
      sdev = sqrt(Eigenvalues),
      rotation = Eigenvectors,
      x = as.matrix(finalData),
      eigenvalues = Eigenvalues,
      eigenvectors = Eigenvectors,
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
