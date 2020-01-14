#misura il tempo in cui il segnale è crescente
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

#conta il numero di occorrenze per zero della funzione
crossing_zero <- function(values) {
  
  counter = 0
  k<-length(values)
  k<-k-1
  for (i in seq_len(k)) {
    if (values[i] >=0) {
      if(values[i+1]<0){
        counter = counter + 1
      }
    }else{
      if(values[i+1]>0){
        counter = counter + 1
      }
    }
  }
  return(counter)
  
}

#misura l'area totale del segnale
compute_area<-function(channel_data){
  
  #determino la dimensione del channel(nel nostro caso deve essere di 204)
  
  dim<-length(channel_data)
  area<-AUC(x=c(1:dim),y=abs(channel_data),method = "trapezoid")
  return(area)
}

#misura l'area negativa del segnale
compute_negative_area <-function(channel_data){
  
  #determino la dimensione del channel(nel nostro caso deve essere di 204)
  dim<-length(channel_data)
  area<-AUC(x=c(1:dim),y=pmin(0,channel_data),method = "trapezoid")
  return(area)
  
}

#misura l'area positiva del segnale
compute_positive_area <-function(channel_data){
  
  #determino la dimensione del channel(nel nostro caso deve essere di 204)
  dim<-length(channel_data)
  area<-AUC(x=c(1:dim),y=pmax(0,(channel_data)),method = "trapezoid")
  return(area)
  
}

#conta il numero di valori positivi --> non serve
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
  features_positives <-as.data.frame(features_positives)
  return(features_positives)
}

#misura  il tempo di salita del segnale
features_rt_channel<-function(df_x){
  
  channel_rt_names<-c("Fz_RT", "Cz_RT", "Pz_RT", "Oz_RT", "P3_RT", "P4_RT", "PO7_RT", "PO8_RT")
  num_channel<-8
  size<-ncol(df_x)/num_channel
  features_rt<-(matrix(ncol = num_channel,nrow = nrow(df_x)))
  
  
  for(i in seq_len(num_channel)){
    #printf("Iterazione: %d\n",i)
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

#calcola il valore del modulo dell'area
features_area_channel<-function(df_x){
  
  channel_label<-c("Fz_Area", "Cz_Area", "Pz_Area", "Oz_Area", "P3_Area", "P4_Area", "PO7_Area", "PO8_Area")
  num_channel<-8
  size<-ncol(df_x)/num_channel
  features_area<-(matrix(ncol = num_channel,nrow = nrow(df_x)))
  
  
  for(i in seq_len(num_channel)){
   #printf("Iterazione: %d\n",i)
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

#calcola il valore dell'area positiva
features_positive_area_channel<-function(df_x){
  
  channel_label<-c("Fz_Area+", "Cz_Area+", "Pz_Area+", "Oz_Area+", "P3_Area+", "P4_Area+", "PO7_Area+", "PO8_Area+")
  num_channel<-8
  size<-ncol(df_x)/num_channel
  features_area<-(matrix(ncol = num_channel,nrow = nrow(df_x)))
  
  
  for(i in seq_len(num_channel)){
    #printf("Iterazione: %d\n",i)
    start<-((i-1)*size)+1
    end<-i*size
    channel<-df_x[,c(start:end)]
    feature_area<-apply(channel, 1,compute_positive_area)
    features_area[,i]<-feature_area
  }
  colnames(features_area)<-channel_label
  features_area<-as.data.frame(features_area)
  return(features_area)
}

#calcola il valore dell'area negativa
features_negative_area_channel<-function(df_x){
  
  channel_label<-c("Fz_Area+", "Cz_Area-", "Pz_Area-", "Oz_Area-", "P3_Area-", "P4_Area-", "PO7_Area-", "PO8_Area-")
  num_channel<-8
  size<-ncol(df_x)/num_channel
  features_area<-(matrix(ncol = num_channel,nrow = nrow(df_x)))
  
  
  for(i in seq_len(num_channel)){
    #printf("Iterazione: %d\n",i)
    start<-((i-1)*size)+1
    end<-i*size
    channel<-df_x[,c(start:end)]
    feature_area<-apply(channel, 1,compute_negative_area)
    features_area[,i]<-feature_area
  }
  colnames(features_area)<-channel_label
  features_area<-as.data.frame(features_area)
  return(features_area)
}
#calcola il valore di picco del segnale x canale
features_peak<-function(df_x){
  
  channel_label<-c("Fz_peak", "Cz_peak", "Pz_peak", "Oz_peak", "P3_peak", "P4_peak", "PO7_peak", "PO8_peak")
  num_channel<-8
  size<-ncol(df_x)/num_channel
  features_peak_values<-(matrix(ncol = num_channel,nrow = nrow(df_x)))
  
  
  for(i in seq_len(num_channel)){
    #printf("Iterazione: %d\n",i)
    start<-((i-1)*size)+1
    end<-i*size
    channel<-df_x[,c(start:end)]
    feature<-apply(channel, 1,max)
    features_peak_values[,i]<-feature
  }
  colnames(features_peak_values)<-channel_label
  features_peak_values <-as.data.frame(features_peak_values)
  return(features_peak_values)
}

#calcola il valore di picco picco del segnale x canale
features_peak_to_peak<-function(df_x){
  
  channel_label<-c("Fz_PPeak", "Cz_PPeak", "Pz_PPeak", "Oz_PPeak", "P3_PPeak", "P4_PPeak", "PO7_PPeak", "PO8_PPeak")
  num_channel<-8
  size<-ncol(df_x)/num_channel
  features_Ppeak_values<-(matrix(ncol = num_channel,nrow = nrow(df_x)))
  
  
  for(i in seq_len(num_channel)){
    #printf("Iterazione: %d\n",i)
    start<-((i-1)*size)+1
    end<-i*size
    channel<-df_x[,c(start:end)]
    max_peak<- apply(channel, 1,max)
    min_peak<- apply(channel,1,min)
    features_Ppeak_values[,i]<-(max_peak-min_peak)
  }
  colnames(features_Ppeak_values)<-channel_label
  features_Ppeak_values <-as.data.frame(features_Ppeak_values)
  return(features_Ppeak_values)
}

#calcola quante volte il segnale passa per zero
features_crossing_zero<-function(df_x){
  
  channel_label<-c("Fz_Cross0", "Cz_Cross0", "Pz_Cross0", "Oz_Cross0", "P3_Cross0", "P4_Cross0", "PO7_Cross0", "PO8_Cross0")
  num_channel<-8
  size<-ncol(df_x)/num_channel
  features_czero_values<-(matrix(ncol = num_channel,nrow = nrow(df_x)))
  
  
  for(i in seq_len(num_channel)){
    #printf("Iterazione: %d\n",i)
    start<-((i-1)*size)+1
    end<-i*size
    channel<-df_x[,c(start:end)]
    c_zero<- apply(channel,1,crossing_zero)
    features_czero_values[,i]<-c_zero
  }
  colnames(features_czero_values)<-channel_label
  features_czero_values <-as.data.frame(features_czero_values)
  return(features_czero_values)
}