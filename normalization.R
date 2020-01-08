
normalize <- function(data) {
  #separazione istanze dall'etichetta della classe
  c_train <- data$train[ , 1]
  data_train <- data$train[ , 2:(ncol(data$train) - 1)]
  label_train <- data$train[ , ncol(data$train)]
  c_test <- data$test[ , 1]
  data_test <- data$test[ , 2:(ncol(data$test) - 1)]
  label_test <- data$test[ , ncol(data$test)]
  
  scaled_training <- scale(data_train, center = T, scale = T)
  #il test viene scalato usando media e dev std dei dati di training (perché stesso campione)
  scaled_test <- scale(data_test, attr(scaled_training, "scaled:center"), attr(scaled_training, "scaled:scale"))
  
  scaled_data <- list(cbind(c_train, scaled_training, label_train), cbind(c_test, scaled_test, label_test))
  names(scaled_data) <- c("train", "test")
  return(scaled_data)
}