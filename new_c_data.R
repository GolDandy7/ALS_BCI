new_c_data<-function(c_data){
  set.seed(123)
  
  num_row=nrow(c_data)
  num_col=12
  my_c_matrix_data<-matrix(0,ncol=12,nrow=num_row)
  for(i in c(1:num_row)){
    for(j in c(1:num_col)){
      if(c_data[i,]==j){
        my_c_matrix_data[i,j]=1
      }
    }
  }
  
  return(my_c_matrix_data)
}