library("R.utils")



confusion_summary <- function(cm) {
  
  accuracy <- round((cm["1", "1"] + cm["-1", "-1"]) / 
                    (cm["1", "1"] + cm["1", "-1"] + cm["-1", "-1"] + cm["-1", "1"]), 4)
  
  TPR_target <- round(((cm["1", "1"]) / (cm["1", "1"] + cm["-1", "1"])), 4)
  TPR_notarget <- round(((cm["-1", "-1"]) / (cm["-1", "-1"] + cm["1", "-1"])), 4)
  
  return(c(accuracy, TPR_target, TPR_notarget))
}



cross_validation <- function(data) {
  
  set.seed(123)
  
  k = 10
  nchar <- nrow(data) / 120
  char_indexes <- sample(c(1:nchar))
  dim_folder <- floor(nchar / k)
  
  results <- matrix(0, nrow = k, ncol = 3)
  colnames(results) <- c("Avg_Accuracy", "Avg_TPR1", "Avg_TPR-1")
  run <- 1
  for (i in seq(1, nchar - (dim_folder + nchar %% k - 1), by = dim_folder)) {
    printf("run %d\n", run)
    test <- data.frame()
    test_rows <- vector()
    print("test characters:")
    for (j in 0:(dim_folder - 1)) {
      char <- char_indexes[i + j]
      printf("%d\n", char)
      start <- ((char - 1) * 120) + 1
      end <- char * 120
      slice <- data[c(start:end), ]
      test <- rbind(test, slice)
      test_rows <- c(test_rows, c(start:end))
    }
    train <- data[-test_rows, ]
    
    last_col <- ncol(train)
    x <- train[, -c(1, last_col)]
    y <- train[, last_col]
    model <- LiblineaR(data = x, target = y, type = 7, cost = 0.01, bias = TRUE, verbose = FALSE)
    
    last_col <- ncol(test)
    x <- test[, -c(1, last_col)]
    y <- test[, last_col]
    prediction <- predict(model, x, decisionValues = TRUE)
    #print("Prediction")
    #print(as.array(prediction$decisionValues))
    confusion_matrix <- table(predicted = prediction$predictions, observation = y)
    print("Confusion Matrix:")
    print(confusion_matrix)
    results[run,] <- confusion_summary(confusion_matrix)
    run <- run + 1
  }
  print(results)
  
  mean_results <- apply(results, 2, mean)
  print(mean_results)
  
  return(model)
}
