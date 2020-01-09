decision_function<-function(dvalues,c){
  
  #dvalues: colonna delle decision values
  #c colonna degli indici riga/colonna (df_c)
  
  num_row<-length(dvalues)
  print("Nrow")
  print(nrow)
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
    print("Slice character")
    print(slice)
    rc<-max_values(slice,iterations)
    #indexes<-which(cdvalues[,1]==rc[1] | cdvalues[,1]==rc[2])
    indexes<-which(slice[,1]==rc[1] | slice[,1]==rc[2])
    
    y[(start-1)+indexes]<-1
    print("Y da start a end")
    print(y[start:end])
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
    #print("iter slice")
    #print(iteration_slice)
    results[i,]<-max_rc(iteration_slice)
    #print("Result i")
    #print(results[i,])
  }
  mode<-apply(results,2,getmode)
  print("mode")
  print(mode)
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

