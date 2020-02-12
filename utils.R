apply_labels <- function(df) {
  
  ncol = ncol(df)
  labels = vector(length = ncol)
  
  # generate labels
  for (i in seq_len(NUM_CHANNELS)) {
    for (j in seq_len(SAMPLE_POINTS)) {
      labels[(i - 1) * SAMPLE_POINTS + j] <- paste(CHANNELS[i], j, sep = "_")
    }
  }
  
  # apply labels and return dataframe
  names(df) <- labels
  return(df)
}

get_xdf <- function(data) {
  last_col <- ncol(data)

  return(as.data.frame(data[, -c(1, last_col)]))
}

get_ydf <- function(data) {
  last_col <- ncol(data)
  y <- as.data.frame(data[,last_col])
  names(y) <- "label"
  
  return(y)
}

get_cdf <- function(data) {
  c <- as.data.frame(data[,1])
  names(c) <- "c"
  return(c)
}

get_xydf <- function(data) {
  return(as.data.frame(data[, -1]))
}
