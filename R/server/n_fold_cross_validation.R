n_fold_cross_validation <- function(data, n_fold, n_repetition, seed_input, center,scale.,threshold ) {
      df <- data$df
      img <- as.matrix(data$img)
      label <- data$label
      accuracy <- matrix(0, nrow = 3, ncol = n_repetition)
      for(i in 1:n_repetition){
          set.seed(seed_input+i)
          list_index <- sample(nrow(img), nrow(img))
          index_test <- matrix(list_index, ncol = n_fold, byrow = T)
          accuracy_for_each_iteration <- matrix(0, nrow = n_fold, ncol = 3)
          for(j in 1:n_fold){
              index <- sort(c(index_test[,j]))
              test <- img[index, ]
              train <- img[-index, ]
              test_label <- label[index, ]
              train_label <- label[-index, ]
            #   print(dim(test))
            #   print(dim(train))
              pca_result <- pca_eigenfaces(train,center = center,scale. = scale.,threshold_percent = threshold,showall = FALSE)
              pca_result$new_train <- data.frame(labels = train_label, data = pca_result$finalData)
              data_new_test <- scale(test, center = pca_result$center, scale = pca_result$scale) %*% pca_result$eigenfaces
              pca_result$new_test <- data.frame(labels = test_label, data = data_new_test)
              for(k in 1:3){
                  accuracy_for_each_iteration[j,k] <- classify(pca_result$new_train,pca_result$new_test,k,pca_result$eigenvalues)*100
              }
          }
          scaled <- scale(accuracy_for_each_iteration)
           mean <- attr(scaled, "scaled:center")
           accuracy[,i] <- mean
      }
      accuracy <- data.frame(accuracy, row.names = c("Euclidean","Mahalanobis","Manhattan"))
      colnames(accuracy) = paste0("rep.", seq_len(ncol(accuracy))) 
      return(accuracy)

}