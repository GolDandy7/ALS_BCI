choosing_cross_validation <-function(data){
  set.seed(123)
  
  #valori di C
  cross_values<- c(0.01,0.1,1)
  #valori di Loss
  type_values<- c(1,2,7)
  #inizializzo il dataframe
  outcomes<- data_frame()
  
  for(i in c(1:length(cross_values))){
    
    for(j in c(1:length(type_values))){
      result<- cross_CT_validation(data,type_values[j],cross_values[i])
      outcomes<- rbind(outcomes, result)
    }
  }
  
  colnames(outcomes)<- c("Avg_Accuracy", "Avg_TPR1", "Avg_TPR -1")
  rownames(outcomes)<- c("C=0.01,T=1","C=0.01,T=2","C=0.01,T=7","C=0.1,T=1","C=0.1,T=2","C=0.1,T=7","C=1,T=1",
           "C=1,T=2","C=1,T=7")
  return(outcomes)
}