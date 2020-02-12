test_accuracy <-function(model, test) {
  
  last_col <- ncol(test)
  c <- test[, 1]
  x <- test[, -c(1, last_col)]
  y <- test[, last_col]
  
  prediction <- max_predict(model, x, c)
  confusion_matrix <- table(predicted = prediction, observation = y)
  print("Confusion Matrix:")
  print(confusion_matrix)
  char_acc <- char_accuracy(prediction,y)
  print("Confusion Summary \n")
  print(confusion_summary(confusion_matrix))
  return(char_acc)
}

char_accuracy <- function(yp,yt){
  
  num_char <- length(yp)/CHAR_ROWS
  res <- 0
  for(i in seq_len(num_char)){
    start<-((i-1)*CHAR_ROWS)+1
    end<-i*CHAR_ROWS
    res<- res + partial_accuracy(yp[c(start:end)],yt[c(start:end)])
  }
  
  return(res/num_char)
}

partial_accuracy <- function(yp,yt){
  
  len <- length(yp)
  count <- 0
  for(i in seq_len(len)){
    if(yp[i]==yt[i]){
      count <- count+1
    }
    
  }
  if(len == count){
    return(1)
  }
  else{
    return(0)
  }
}