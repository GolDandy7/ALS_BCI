library("R.utils")

confusion_summary <- function(cm) {
  
  accuracy <- round((cm["1", "1"] + cm["-1", "-1"]) / 
                      (cm["1", "1"] + cm["1", "-1"] + cm["-1", "-1"] + cm["-1", "1"]), 4)
  
  TPR_target <- round(((cm["1", "1"]) / (cm["1", "1"] + cm["-1", "1"])), 4)
  TPR_notarget <- round(((cm["-1", "-1"]) / (cm["-1", "-1"] + cm["1", "-1"])), 4)
  
  return(c(accuracy, TPR_target, TPR_notarget))
}



cross_CT_validation <- function(data,loss,c) {
  #loss rappresenta il type
  
  set.seed(123)
  
  #sink("output.txt",append=TRUE)
  #print("\n+++++++++++++++++++++++++++++++++++++++++++++++++++\n")
  #printf("\nCross Validation con valori di loss=%s e C=%s\n",loss,c)
  k = 10
  nchar <- nrow(data) / 120
  char_indexes <- sample(c(1:nchar))
  dim_folder <- floor(nchar / k)
  
  results <- matrix(0, nrow = k, ncol = 3)
  colnames(results) <- c("Avg_Accuracy", "Avg_TPR1", "Avg_TPR-1")
  run <- 1
  for (i in seq(1, nchar - (dim_folder + nchar %% k - 1), by = dim_folder)) {
    test <- data.frame()
    test_rows <- vector()
    for (j in 0:(dim_folder - 1)) {
      char <- char_indexes[i + j]
      start <- ((char - 1) * 120) + 1
      end <- char * 120
      slice <- data[c(start:end), ]
      test <- rbind(test, slice)
      test_rows <- c(test_rows, c(start:end))
    }
    train <- data[-test_rows, ]
    
    last_col <- ncol(train)
    x <- train[, -last_col]
    y <- train[, last_col]
    #model <- LiblineaR(data = x, target = y, type =loss, cost = c, bias = TRUE, verbose = FALSE)
    model <- LiblineaR(data = data[, -c(1, last_col)], target = data[, last_col], type = loss, cost = c, bias = TRUE, verbose = FALSE)
    
    last_col <- ncol(test)
    x <- test[, -last_col]
    y <- test[, last_col]
    #prediction <- predict(model, x, decisionValues = TRUE)
    #confusion_matrix <- table(predicted = prediction$predictions, observation = y)
    #print("Confusion Matrix:")
    #print(confusion_matrix)
    results[run,] <- test_accuracy(model,x)
    run <- run + 1
  }
  
  mean_results <- apply(results, 2, mean)
  #print("\nMean Results\n")
  #print(mean_results)
  #print("\n+++++++++++++++++++++++ FINE ++++++++++++++++++\n")
  #sink()
  return(mean_results)
}