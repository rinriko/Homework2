# Ref: https://rpubs.com/dherrero12/543854

classify <- function(train, test, type, eigenvalues) {
  classes <- aggregate(train, list(train$labels), mean)
  class_label <- classes[, 1]
  
  test_label  <- test$labels
  
  classes[, c(1, 2)] <- NULL
  #Group.1 labels
  
  test[, 1] <- NULL
  #labels
  
  classes <- as.matrix(classes)
  
  
  measure_of_the_distance <- matrix(0, nrow(classes), nrow(test))
  for (j in 1:nrow(test)) {
    for (i in 1:nrow(classes)) {
      measure_of_the_distance[i, j] <-
        distance(unlist(test[j, ]), classes[i, ], type, eigenvalues)
    }
  }
  
  colnames(measure_of_the_distance) <- test_label
  rownames(measure_of_the_distance) <- class_label
  
  # print(measure_of_the_distance)
  
  min.err <- min(apply(measure_of_the_distance, 2, min))
  max.err <- max(apply(measure_of_the_distance, 2, min))
  accuracy <- 0
  for (i in 1:ncol(measure_of_the_distance)) {
    pred <-
      names(which(
        min(measure_of_the_distance[, i]) == measure_of_the_distance[, i]
      ))
    if (pred == colnames(measure_of_the_distance)[i]) {
      accuracy <- accuracy + 1
    }
  }
  return(accuracy / ncol(measure_of_the_distance))
  
  # threshold <- seq(min.err, max.err, by = (max.err - min.err)/200)
  # acc.thres <- vector("numeric", length(threshold))
  # for (j in 1:length(threshold)) {
  #   accuracy <- 0
  
  #   for (i in 1:ncol(measure_of_the_distance)) {
  #     pred <- ""
  #     if (min(measure_of_the_distance[,i]) > threshold[j]) {
  #       pred <- "0"
  #     } else {
  #       pred <- names(which(min(measure_of_the_distance[,i]) == measure_of_the_distance[,i]))
  #     }
  #     if (pred == colnames(measure_of_the_distance)[i]) {
  #       accuracy <- accuracy + 1
  #     }
  #   }
  
  #   acc.thres[j] <- accuracy/ncol(measure_of_the_distance)
  # }
  
  # aux <- which(max(acc.thres) == acc.thres)[1]
  # return(c(acc.thres[aux], threshold[aux]))
}