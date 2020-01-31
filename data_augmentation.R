generate_data <- function(data, portion, func, ...) {
  
  sample_size <- floor(portion * nrow(data) / CHAR_ROWS)
  char_indexes <- sample(c(1:sample_size))
  
  new_data <- data.frame()
  
  extraction_indexes <- vector(length = sample_size * CHAR_ROWS)
  
  for (i in seq_len(sample_size)) {
    start <- ((char_indexes[i] - 1) * CHAR_ROWS) + 1
    end <- (char_indexes[i]) * CHAR_ROWS
    extraction_indexes <- c(extraction_indexes, c(start:end))
  }
  
  new_data <- data[extraction_indexes, ]
  
  num_col <- ncol(new_data)
  c <- new_data[, 1]
  x <- new_data[, -c(1, num_col)]
  y <- new_data[, num_col]
  # somma white noise alle righe di
  x <- func(x, ...)
  #x <- as.data.frame(t(apply(x, 1, add_wn)))
  new_data <- cbind(c, x, y)
  names(new_data) <- names(data)
  return(new_data)
}




put_noise <- function(data) {
  print("put_noise function")
  as.data.frame(t(apply(data, 1, function(row) {
    wn <- as.vector(arima.sim(list(order=c(0,0,0)), n=length(row)))
    row + wn
  })))
}

add_wn <- function(row) {
  # generazione di un white noise di lunghezza pari a 204
  wn <- as.vector(arima.sim(list(order=c(0,0,0)), n=length(row)))
  return(row + wn)
}


# Sfasamento del segnale: shift a destra di "shift_size" posizioni dei valori 
# della righe del dataframe e rimpiazza i primi "shift_size" elementi con i valori medi 
right_shift <- function(data, shift_size) {
  #print("right shift function")
  #printf("shift_size = %d\n", shift_size)
  replace_data <- apply(data[, 1:shift_size], 2, mean)
  as.data.frame(t(apply(data, 1, function(row, replace_data) {
    c(replace_data, row[1:(length(row) - shift_size)])
  }, replace_data)))
}