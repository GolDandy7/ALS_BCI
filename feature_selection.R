rising_time <- function(values) {
  
  total_time = 0
  k<-length(values)
  k<-k-1
  for (i in seq_len(k)) {
    if (values[i] >= values[i + 1]) {
      total_time = total_time + 1
    }
  }
  return(total_time)
}

#numero di elementi positivi per riga di uno specifico segnale
positive_values <- function(values) {
  
  counter = 0
  k<-length(values)
  k<-k-1
  for (i in seq_len(k)) {
    if (values[i] >= 0) {
      counter = counter + 1
    }
  }
  return(counter)
}
compute_area<-function(channel_data){
  
  #determino la dimensione del channel(nel nostro caso deve essere di 204)
  
  dim<-length(channel_data)
  area<-AUC(x=c(1:dim),y=abs(channel_data),method = "trapezoid")
  return(area)
}

new_features<-function(df_x){
  
  channel_area_names<-c("Fz_Area", "Cz_Area", "Pz_Area", "Oz_Area", "P3_Area", "P4_Area", "PO7_Area", "PO8_Area")
  channel_rt_names<-c("Fz_RT", "Cz_RT", "Pz_RT", "Oz_RT", "P3_RT", "P4_RT", "PO7_RT", "PO8_RT")
  
  num_channel<-8
  size<-ncol(df_x)/num_channel
  features<-dataFrame(matrix(ncol = 16, nrow = 0))
  for(i in seq_len(num_channel)){
    
    start<-((i-1)*size)+1
    end<-i*size
    channel<-df_x[,c(start:end)]
    feature_area<-apply(channel, 1,compute_area)
    feature_area<-as.data.frame(feature_area)
    colnames(feature_area)<-channel_area_names[i]
    features<-cbind(features,feature_area)
    feature_rt<-apply(channel, 1, rising_time)
    feature_rt<-as.data.frame(feature_rt)
    names(feature_rt)<-channel_rt_names[i]
    
  }
  return(features)
  
}

features_positive<-function(df_x){
  
  channel_label<-c("Fz_+", "Cz_+", "Pz_+", "Oz_+", "P3_+", "P4_+", "PO7_+", "PO8_+")
  num_channel<-8
  size<-ncol(df_x)/num_channel
  features_positives<-(matrix(ncol = num_channel,nrow = nrow(df_x)))
  
  
  for(i in seq_len(num_channel)){
    printf("Iterazione: %d\n",i)
    start<-((i-1)*size)+1
    end<-i*size
    channel<-df_x[,c(start:end)]
    feature_positive<-apply(channel, 1,positive_values)
    features_positives[,i]<-feature_positive
  }
  colnames(features_positives)<-channel_label
  features_positives<-as.data.frame(features_positives)
  return(features_positives)
}
features_rt_channel<-function(df_x){
  
  channel_rt_names<-c("Fz_RT", "Cz_RT", "Pz_RT", "Oz_RT", "P3_RT", "P4_RT", "PO7_RT", "PO8_RT")
  num_channel<-8
  size<-ncol(df_x)/num_channel
  features_rt<-(matrix(ncol = num_channel,nrow = nrow(df_x)))
  
  
  for(i in seq_len(num_channel)){
    printf("Iterazione: %d\n",i)
    start<-((i-1)*size)+1
    end<-i*size
    channel<-df_x[,c(start:end)]
    feature_rt<-apply(channel, 1,rising_time)
    features_rt[,i]<-feature_rt
  }
  colnames(features_rt)<-channel_rt_names
  features_rt<-as.data.frame(features_rt)
  return(features_rt)
}

features_area_channel<-function(df_x){
  
  channel_label<-c("Fz_Area", "Cz_Area", "Pz_Area", "Oz_Area", "P3_Area", "P4_Area", "PO7_Area", "PO8_Area")
  num_channel<-8
  size<-ncol(df_x)/num_channel
  features_area<-(matrix(ncol = num_channel,nrow = nrow(df_x)))
  
  
  for(i in seq_len(num_channel)){
   printf("Iterazione: %d\n",i)
   start<-((i-1)*size)+1
   end<-i*size
   channel<-df_x[,c(start:end)]
   feature_area<-apply(channel, 1,compute_area)
   features_area[,i]<-feature_area
 }
  colnames(features_area)<-channel_label
  features_area<-as.data.frame(features_area)
  return(features_area)
}