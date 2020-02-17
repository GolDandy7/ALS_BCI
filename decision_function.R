# funzione di predizione basata sul massimo 
max_predict <- function(model, x, c) {
  sign_prediction <- predict(model, x, decisionValues = TRUE)
  decision_function(sign_prediction$decisionValues[, 1], c)
}



decision_function<-function(dvalues,c){
  
  #dvalues: colonna delle decision values
  #c colonna degli indici riga/colonna (df_c)
  
  num_row<-length(dvalues)
  dim<-12
  iterations<-10
  slice_size<-dim*iterations
  characters<-num_row/(slice_size)
  
  cdvalues<-cbind(c,dvalues)
  
  y<-rep(-1,num_row)
  for(i in seq_len(characters)){
    start<-((i-1)*slice_size)+1
    end<-i*slice_size
    slice<-cdvalues[c(start:end),]
    #ottengo indici riga colonna
    rc<-max_values(slice,iterations)
    #indici corrispondenti alla riga e colonna selezionate
    indexes<-which(slice[,1]==rc[1] | slice[,1]==rc[2])
    #assegno il valore uno a tutte le righe corrispondenti agli indici indexes
    y[(start-1)+indexes]<-1
  }
  return(y)
}

max_values<-function(char_data,iterations){
  
  #partitions: lista di vettori di dimensione pari a 12
  results<-matrix(nrow=iterations,ncol = 2)
  iteration_size<-12
  for(i in seq_len(iterations)){
    start<-((i-1)*iteration_size)+1
    end<-i*iteration_size
    iteration_slice<-char_data[c(start:end),]
    results[i,]<-max_rc(iteration_slice)
    
  }
  #mode<-apply(results,2,weighted_mode)
  mode<-apply(results,2,getmode)
  
  return(mode)
}

#iteration_data è una slice di 12 righe contentente dvalues e c
#ritorna un vettore contentente l'indice di riga e colonna con dvalues max
max_rc<-function(iteration_data){
  
  max_row=-Inf
  max_col=-Inf
  
  max_v<-vector(length = 2)

  for(i in seq_len(nrow(iteration_data))){
    if(iteration_data[i,1]<7){
      if(iteration_data[i,2]>max_row){
         max_row<-iteration_data[i,2]
         max_v[1]<-iteration_data[i,1]
      }
    }
    else{
      if(iteration_data[i,2]>max_col){
        max_col<-iteration_data[i,2]
        max_v[2]<-iteration_data[i,1]
      }
    }
  }
  return(max_v)
}


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# calcola la moda pesata con pesi decrescenti
weighted_mode <- function(v) {
  uniqv <- unique(v)
  n <- length(v)
  l <- length(uniqv)
  w <- vector(length = l)
  m <- mean(c(1:l))
  for (i in seq_len(l)) {
    match_indexes <- which(v == uniqv[i])
    for (j in match_indexes) {
      #w[i] = w[i] + (1 + 2 / (n - j + 1))
      #w[i] = w[i] + (2 - abs(j - m) / m)
      w[i] = w[i] + (1 + 2 / (j + 1))
      
    }
  }
  uniqv[which.max(w)]
}