# Ref: https://github.com/sinhrks/ggfortify
# Ref: https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
autoplot_pca <- function(object, data = NULL,
                                scale = 1.0, x = 1, y = 2,
                                variance_percentage = TRUE, ...) {
  plot.data <- fortify_pca(object, data = data)
  plot.data$rownames <- rownames(plot.data)

    ve <- object$summary.propvar
    PC <- paste0("PC", c(x, y))
    x.column <- PC[1]
    y.column <- PC[2]
    loadings.column <- 'rotation'

    lam <- object$sdev[c(x, y)]
    lam <- lam * sqrt(nrow(plot.data))


  # scaling
  if (scale != 0) {
    lam <- lam ^ scale
    plot.data[, c(x.column, y.column)] <- t(t(plot.data[, c(x.column, y.column)]) / lam)
  }

  # move target columns to 1st and 2nd
  plot.columns <- unique(c(x.column, y.column, colnames(plot.data)))
  plot.data <- plot.data[, plot.columns]

  if (!is.null(loadings.column)) {
    loadings.data <-as.data.frame(object[[loadings.column]][, ])
    loadings.data$rownames <- rownames(loadings.data)

    loadings.columns <- unique(c(x.column, y.column, colnames(loadings.data)))
    loadings.data <- loadings.data[, loadings.columns]
  } else {
    loadings.data <- NULL
  }

  #Make labels
  if (is.null(ve) | !variance_percentage) {
    labs <- PC
  } else {
    ve <- ve[c(x, y)]
    labs <- paste0(PC, " (", round(ve * 100, 2), "%)")
  }
  xlab <- labs[1]
  ylab <- labs[2]

  p <- ggbiplot(plot.data = plot.data,
                loadings.data = loadings.data,
                xlab = xlab,
                ylab = ylab, ...)
  return(p)
}

fortify_pca <- function(model, data = NULL, ...) {

    d <- as.data.frame(model$x)
    values <- model$x %*% t(model$rotation)


  values <- ggfortify::unscale(values, center = model$center,
                               scale = model$scale)
  values <- cbind_wraps_pca(data, values)
  d <- cbind_wraps_pca(values, d)
  post_fortify_pca(d)
}

cbind_wraps_pca <- function(df1, df2) {
  if (is.null(df1)) {
    return(df2)
  } else if (!is.data.frame(df1)) {
    df1 <- ggplot2::fortify(df1)
  }
  if (is.null(df2)) {
    return(df1)
  } else if (!is.data.frame(df2)) {
    df2 <- ggplot2::fortify(df2)
  }
  # prioritize df1 columns
  dots <- names(df2)[! colnames(df2) %in% colnames(df1)]
  if (length(dots) != length(colnames(df2))) {
    df2 <- dplyr::select(df2, all_of(dots))
  }
  return(cbind(df1, df2))
}


post_fortify_pca <- function(data, klass = NULL) {
  if (is(data, 'tbl_df')) {
    data <- as.data.frame(data)
  }
  if (!is.data.frame(data)) {
    stop('data must be a data.frame')
  }
  if (!is.null(klass)) {
    attr(data, 'base_class') <- class(klass)
  }
  as.data.frame(data)
}
