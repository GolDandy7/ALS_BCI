
normalize <- function(training_data, test_data) {
  #separazione istanze dall'etichetta della classe
  num_col <- ncol(training_data)
  c_train <- training_data[ , 1]
  data_train <- training_data[ , 2:(num_col - 1)]
  label_train <- training_data[ , num_col]
  c_test <- test_data[ , 1]
  data_test <- test_data[ , 2:(num_col - 1)]
  label_test <- test_data[ , num_col]
  
  scaled_training <- scale(data_train, center = T, scale = T)
  #il test viene scalato usando media e dev std dei dati di training (perché stesso campione)
  scaled_test <- scale(data_test, attr(scaled_training, "scaled:center"), attr(scaled_training, "scaled:scale"))
  
  scaled_data <- list(cbind(c_train, scaled_training, label_train), cbind(c_test, scaled_test, label_test))
  names(scaled_data) <- c("train", "test")
  return(scaled_data)
}