generate_data <- function(data, portion, func, ...) {
  
  # numero di caratteri da cui vengono genrati i nuovi dati
  sample_size <- floor(portion * nrow(data) / CHAR_ROWS)
  new_data <- data.frame()
  
  extraction_indexes <- c(1:(sample_size * CHAR_ROWS))

  extracted_data <- data[extraction_indexes, ]
  new_data <- func(extracted_data, ...)
  names(new_data) <- names(data)
  
  return(new_data)
}


# genera nuovi caratteri facendo la media di 'n' caratteri
meanchar_gen <- function(data, n) {
  num_chars <- nrow(data) / CHAR_ROWS
  if (n >= num_chars) {
    print("numero di caratteri di partenza troppo grande")
    stop()
  }
  # aggiungi il primo carattere alla fine del dataset per ottenere esattamente n nuovi caratteri
  data <- rbind(data, data[1:(CHAR_ROWS * (n - 1)), ])
  new_data <- data.frame()
  
  for (i in seq_len(num_chars)) {
    start <- (i - 1) * CHAR_ROWS + 1
    end <- start + n * CHAR_ROWS
    selected_chars <- data[start:end, ]
    new_char <- avg_char(selected_chars, n)
    new_data <- rbind(new_data, new_char)
  }
  
  return(new_data)
}

avg_char <- function(chars_data, n) {
  
  new_char <- data.frame()
  for (i in seq_len(NUM_ITERATIONS)) {
    selected_rows <- select_rows(chars_data, i, n)
    new_iter_data <- avg_iter(selected_rows, n)
    new_char <- rbind(new_char, new_iter_data)
  }
  return(new_char)
}

select_rows <- function(iteration_data, i, n) {
  
  selected_rows <- data.frame()
  for (char_index in seq_len(n)) {
    char_offset <- (char_index - 1) * CHAR_ROWS
    start <- char_offset + (i - 1) * (NUM_ROWS + NUM_COLS) + 1
    end <- char_offset + (i) * (NUM_ROWS + NUM_COLS)
    char_iter_data <- iteration_data[start:end, ]
    selected_rows <- rbind(selected_rows, char_iter_data)
  }
  return(selected_rows)
}
    
avg_iter <- function(iter_rows, n) {
  
    new_c <- c(1, 7, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12)
  
    new_target <- data.frame()
    new_nontarget <- data.frame()
    
    target <- iter_rows[iter_rows$label == 1, ]
    nontarget <- iter_rows[iter_rows$label == -1, ]
    
    ordered_target <- target[order(target$c), ]
    ordered_target$c <- NULL  # delete c column
    new_target <- apply(ordered_target, 2, grouped_mean, n=n)
    ordered_nontarget <- nontarget[order(nontarget$c), ]
    ordered_nontarget$c <- NULL  # delete c column
    new_nontarget <- apply(ordered_nontarget, 2, grouped_mean, n=n)
    
    new_iter <- cbind(new_c, rbind(new_target, new_nontarget))
    new_iter <- new_iter[sample(nrow(new_iter)), ]
    return(new_iter)
}

grouped_mean <- function(column, n) {
  len_col = length(column)
  if ((len_col %% n) != 0) {
    print("la lunghezza della colonna non è un multiplo di n")
    stop()
  }
  result <- vector(length = len_col / n)
  k = 1
  for (i in seq(1, len_col, by = n)) {
    sum <- 0
    for (j in seq_len(n) - 1) {
      sum <- sum + column[i + j]
    }
    result[k] <- sum / n
    k = k + 1
  }
  return(result)
}


# aggiunta di rumore bianco
put_noise <- function(data) {
  print("put_noise function")
  num_col <- ncol(data)
  c <- data[, 1]
  x <- data[, -c(1, num_col)]
  y <- data[, num_col]
  as.data.frame(t(apply(x, 1, function(row) {
    wn <- as.vector(arima.sim(list(order=c(0,0,0)), n=length(row)))
    row + wn
  })))
  
  new_data <- cbind(c, x, y)
  return(new_data)
}


# Sfasamento del segnale: shift a destra di "shift_size" posizioni dei valori 
# della righe del dataframe e rimpiazza i primi "shift_size" elementi con i valori medi 
right_shift <- function(data, shift_size) {
  #print("right shift function")
  #printf("shift_size = %d\n", shift_size)
  num_col <- ncol(data)
  c <- data[, 1]
  x <- data[, -c(1, num_col)]
  y <- data[, num_col]
  
  replace_data <- apply(x[, 1:shift_size], 2, mean)
  as.data.frame(t(apply(x, 1, function(row, replace_data) {
    c(replace_data, row[1:(length(row) - shift_size)])
  }, replace_data)))
  
  new_data <- cbind(c, x, y)
  return(new_data)
}