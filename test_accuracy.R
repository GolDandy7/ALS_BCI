test_accuracy <-function(test){
  set.seed(123)
  
  last_col <- ncol(test)
  c<-test[,1]
  x <- test[, -c(1, last_col)]
  y <- test[, last_col]
  model <- LiblineaR(data = x, target = y, type =7, cost = 0.01, bias = TRUE, verbose = FALSE)
  
  prediction <- predict(model, x, decisionValues = TRUE)
  
  new_y<-decision_function(prediction$decisionValues[,1],c)
  print("lunghezza decision value")
  print(length(prediction$decisionValues[,1]))
  print("Dim New Y")
  print(length(new_y))
  print("Y trget")
  print(length(y))
  #confusion_matrix <- table(predicted = prediction$predictions, observation = y)
  confusion_matrix <- table(predicted = new_y, observation = y)
  #print("Confusion Matrix:")
  #print(confusion_matrix)
   results<- confusion_summary(confusion_matrix)
}