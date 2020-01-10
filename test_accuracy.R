test_accuracy <-function(model, test) {
  
  last_col <- ncol(test)
  c <- test[, 1]
  x <- test[, -c(1, last_col)]
  y <- test[, last_col]
  
  prediction <- max_predict(model, x, c)
  confusion_matrix <- table(predicted = prediction, observation = y)
  print("Confusion Matrix:")
  print(confusion_matrix)
  
  return(confusion_summary(confusion_matrix))
}