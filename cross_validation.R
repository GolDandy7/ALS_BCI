

confusion_summary <- function(cm) {
  
  accuracy <- round((cm["1", "1"] + cm["-1", "-1"]) / 
                    (cm["1", "1"] + cm["1", "-1"] + cm["-1", "-1"] + cm["-1", "1"]), 4)
  
  TPR_target <- round(((cm["1", "1"]) / (cm["1", "1"] + cm["-1", "1"])), 4)
  TPR_notarget <- round(((cm["-1", "-1"]) / (cm["-1", "-1"] + cm["1", "-1"])), 4)
  
  return(c(accuracy, TPR_target, TPR_notarget))
}



cross_validation <- function(data, classifier, ...) {
  
  k = 10
  
  results <- matrix(0, nrow = k, ncol = 2)
  colnames(results) <- c("Run","Accuracy")
  # genera k coppie training-set / validation-set
  folders_list <- kfold(data, k) 
  train_list <- folders_list$train
  test_list <- folders_list$test
  
  for (i in seq_len(k)) {
    
    train <- train_list[[i]]
    test <- test_list[[i]]
    
    # training
    last_col <- ncol(train)
    x <- train[, -c(1, last_col)]
    y <- train[, last_col]
    # addestramento
    model <- classifier(x, y, ...)
    
    #test
    #results[i, ] <- test_accuracy(model, test)
    results[i, ] <- c(i, test_accuracy(model, test))
  }
  
  return(results)
}


# Genera una lista contenente k coppie training set - validation set 
# selezionati con il metodo k-fold a partire dal training-set data
kfold <- function(data, k) {
  
  nchar <- nrow(data) / CHAR_ROWS
  char_indexes <- sample(c(1:nchar))
  dim_folder <- floor(nchar / k)
  
  result_list <- list()
  train_list <- list()
  test_list <- list()
  run = 1
  
  for (i in seq(1, nchar - (dim_folder + nchar %% k - 1), by = dim_folder)) {
    test <- data.frame()
    test_rows <- vector()
    for (j in 0:(dim_folder - 1)) {
      char <- char_indexes[i + j]
      start <- ((char - 1) * CHAR_ROWS) + 1
      end <- char * CHAR_ROWS
      slice <- data[c(start:end), ]
      test <- rbind(test, slice)
      test_rows <- c(test_rows, c(start:end))
    }
    train <- data[-test_rows, ]
    
    train_list[[run]] <- train
    test_list[[run]] <- test
    run <- run + 1
  }
  result_list[[1]] <- train_list
  result_list[[2]] <- test_list
  names(result_list) <- c("train", "test")
  return(result_list)
}
