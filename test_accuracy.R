test_accuracy <-function(test){
  set.seed(123)
  
  last_col <- ncol(test)
  x <- test[, -last_col]
  y <- test[, last_col]
  model <- LiblineaR(data = x, target = y, type =7, cost = 0.01, bias = TRUE, verbose = FALSE)
  
  prediction <- predict(model, x, decisionValues = TRUE)
  confusion_matrix <- table(predicted = prediction$predictions, observation = y)
  #print("Confusion Matrix:")
  #print(confusion_matrix)
   results<- confusion_summary(confusion_matrix)
}