data_split<-function(data){
  set.seed(123)
#splittare il dataframe separando le istanze target e non target
  indexes<-c(1:(nrow(data)/120))
  sample_size<-floor(0.70*length(indexes))
  
  indexes<- sample(indexes)
  train_data<-data.frame()
  test_data<-data.frame()
  
  for(i in seq_len(sample_size)){
    
    start<-((indexes[i]-1)*120)+1
    end<-(indexes[i])*120
    slice<-data[c(start:end),]
    train_data<-rbind(train_data,slice)
  }
  for(i in c((sample_size+1):length(indexes))){
    
    start<-((indexes[i]-1)*120)+1
    end<-(indexes[i])*120
    slice<-data[c(start:end),]
    test_data<-rbind(test_data,slice)
  }

  split<- list(train_data, test_data)
  names(split)<-c("train","test")
  return(split)
}