# Ref: https://rpubs.com/dherrero12/543854

recognize <- function(train, test,type,eigenvalues) {
 classes <- aggregate(train, list(train$labels), mean)
  class_label <- classes[,1]

  test_label  <- test$labels
  
  classes[,c(1,2)] <- NULL
  #Group.1 labels

  test[,1] <- NULL
  #labels

  classes <- as.matrix(classes)
  measure_of_the_distance <- matrix(0, nrow = nrow(classes), ncol = nrow(test))

  for (j in 1:nrow(test)) {
    for (i in 1:nrow(classes)) {
      measure_of_the_distance[i,j] <- distance(unlist(test[j,]), classes[i,], type,eigenvalues)
    }
  }
  
colnames(measure_of_the_distance) <- test_label
  rownames(measure_of_the_distance) <- class_label

  predictions <- c()
   for (i in 1:ncol(measure_of_the_distance)){
        pred <- names(which(min(measure_of_the_distance[,i]) == measure_of_the_distance[,i]))
      predictions[i] <- pred
  }

  return(predictions)
}