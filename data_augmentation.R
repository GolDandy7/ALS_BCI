augment_data <- function(data, portion) {
  
  char_rows = 120
  num_channels = 8
  sample_size <- floor(portion * nrow(data) / char_rows)
  char_indexes <- sample(c(1:sample_size))
  
  new_data <- data.frame()
  
  extraction_indexes <- vector(length = sample_size * char_rows)
  
  for (i in seq_len(sample_size)) {
    start <- ((char_indexes[i] - 1) * char_rows) + 1
    end <- (char_indexes[i]) * char_rows
    extraction_indexes <- c(extraction_indexes, c(start:end))
  }
  
  new_data <- data[extraction_indexes, ]
  
  num_col <- ncol(new_data)
  c <- new_data[, 1]
  x <- new_data[, -c(1, num_col)]
  y <- new_data[, num_col]
  # somma white noise alle righe di
  x_wn <- as.data.frame(t(apply(x, 1, add_wn)))
  new_data <- cbind(c, x_wn, y)
  names(new_data) <- names(data)
  data <- rbind(data, new_data)
}


add_wn <- function(row) {
  
  # generazione di un white noise di lunghezza pari a 204
  wn <- arima.sim(list(order=c(0,0,0)), n=204)
  return(row + as.vector(wn))
}