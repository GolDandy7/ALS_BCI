

apply_labels <- function(df) {
  
  electrodes = c("Fz", "Cz", "Pz", "Oz", "P3", "P4", "PO7", "PO8")
  nsamples = 204
  ncol = ncol(df)
  labels = vector(length = ncol)
  
  # generate labels
  for (i in seq_len(length(electrodes))) {
    for (j in seq_len(nsamples)) {
      labels[(i - 1) * nsamples + j] <- paste(electrodes[i], j, sep = "_")
    }
  }
  
  # apply labels and return dataframe
  names(df) <- labels
  return(df)
}