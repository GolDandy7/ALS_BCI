source("utils.R")

feature_selection <- function(data, p300) {
  
  df_c <- get_cdf(data)
  df_x <- get_xdf(data)
  df_y <- get_ydf(data)
  
  
  #------------features---------------------
  #trasformo le C in una matrice  di 0 e 1 perchè la numerazione mi crea ordinamento
  #feature_c_bin <- to_categorical(df_c)
  feature_P300 <- feature_corr_P300(cbind(df_x, df_y), p300)
  feature_area <- features_area_channel(df_x)
  #feature_area_negative <- features_negative_area_channel(df_x)
  #feature_area_positive <- features_positive_area_channel(df_x)
  #feature_rt <- features_rt_channel(df_x)
  #feature_powsign <- features_signal_power(df_x)
  feature_cz <- features_crossing_zero(df_x)
  #feature_pp <- features_peak_to_peak(df_x)
  #feature_tw <- features_time_windowPP(df_x)
  
  #-----------------------------------------
  featured_data<-cbind(df_c, df_x)
  
  #featured_data <- cbind(featured_data,feature_c_bin)
  featured_data <- cbind(featured_data,feature_P300)
  featured_data <- cbind(featured_data,feature_area)
  #featured_data <- cbind(featured_data,feature_area_negative)
  #featured_data <- cbind(featured_data,feature_area_positive)
  #featured_data <- cbind(featured_data,feature_rt)
  #featured_data <- cbind(featured_data,feature_powsign)
  featured_data <- cbind(featured_data,feature_cz)
  #featured_data <- cbind(featured_data,feature_pp)
  #featured_data <- cbind(featured_data,feature_tw)
  
  featured_data<-cbind(featured_data,df_y)
}


use_relief <- function(data){
  
  ReliefFNumNeighbour<- 70 #suggestion: if ReliefFType == ReliefFequalK  than 10, else 70.
  ReliefFType<- "ReliefFexpRank"
  #numero iterazione cioè m=5
  NumIterations<-30
  n <- ncol(data[,-c(1:1633)])
  relief_att<-attrEval(n, data[,-c(1:1633)], estimator=ReliefFType,
               kNearestExpRank=ReliefFNumNeighbour,
               ReliefIterations=NumIterations)
  #FS_ordered<- sort(FS, decreasing = T)
  ra_ordered <- sort(relief_att, decreasing = T)
  #barplot(relief_att,main="Features",ylab="Eval",las=2,ylim =c(min(relief_att),max(relief_att)))
  #print(ra_ordered)
  barplot(ra_ordered,main="Features",ylab="Eval",las=2,ylim=c(min(relief_att),max(relief_att)))
  return(relief_att)
}


#trasforma il vettore data in una matrice 'one hot'
to_categorical<-function(data){
  
  num_row=nrow(data)
  num_col=12
  matrix_data<-matrix(0,ncol=12,nrow=num_row)
  for(i in c(1:num_row)){
    for(j in c(1:num_col)){
      if(data[i,]==j){
        matrix_data[i,j]=1
      }
    }
  }
  
  return(matrix_data)
}


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


#potenza del segnale
signal_power <- function(values){
  power = 0
  k<-length(values)
  k<-k-1
  for (i in seq_len(k)) {
    power=power+(values[i]*values[i])
  }
  
  return(power)
  
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


#conta il numero di valori positivi 
features_positive<-function(df_x){
  
  channel_label<-c("Fz_+", "Cz_+", "Pz_+", "Oz_+", "P3_+", "P4_+", "PO7_+", "PO8_+")
  size<-ncol(df_x)/NUM_CHANNELS
  features_positives<-(matrix(ncol = NUM_CHANNELS,nrow = nrow(df_x)))
  
  
  for(i in seq_len(NUM_CHANNELS)){
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
  size<-ncol(df_x)/NUM_CHANNELS
  features_rt<-(matrix(ncol = NUM_CHANNELS,nrow = nrow(df_x)))
  
  
  for(i in seq_len(NUM_CHANNELS)){
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
  
  print("Calcolo feature Area ...")
  channel_label<-c("Fz_Area", "Cz_Area", "Pz_Area", "Oz_Area", "P3_Area", "P4_Area", "PO7_Area", "PO8_Area")
  size<-ncol(df_x)/NUM_CHANNELS
  features_area<-(matrix(ncol = NUM_CHANNELS,nrow = nrow(df_x)))
  
  
  for(i in seq_len(NUM_CHANNELS)){
   #printf("Iterazione: %d\n",i)
   start<-((i-1)*size)+1
   end<-i*size
   channel<-df_x[,c(start:end)]
   feature_area<-apply(channel, 1,compute_area)
   features_area[,i]<-feature_area
 }
  colnames(features_area)<-channel_label
  features_area<-as.data.frame(features_area)
  print("Calcolo feature Area completato")
  return(features_area)
}


#calcola il valore dell'area positiva
features_positive_area_channel<-function(df_x){
  
  channel_label<-c("Fz_Area+", "Cz_Area+", "Pz_Area+", "Oz_Area+", "P3_Area+", "P4_Area+", "PO7_Area+", "PO8_Area+")
  size<-ncol(df_x)/NUM_CHANNELS
  features_area<-(matrix(ncol = NUM_CHANNELS,nrow = nrow(df_x)))
  
  
  for(i in seq_len(NUM_CHANNELS)){
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
  size<-ncol(df_x)/NUM_CHANNELS
  features_area<-(matrix(ncol = NUM_CHANNELS,nrow = nrow(df_x)))
  
  for(i in seq_len(NUM_CHANNELS)){
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
  size<-ncol(df_x)/NUM_CHANNELS
  features_peak_values<-(matrix(ncol = NUM_CHANNELS,nrow = nrow(df_x)))
  
  for(i in seq_len(NUM_CHANNELS)){
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
  size<-ncol(df_x)/NUM_CHANNELS
  features_Ppeak_values<-(matrix(ncol = NUM_CHANNELS,nrow = nrow(df_x)))
  
  for(i in seq_len(NUM_CHANNELS)){
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


#calcola il valore di picco del segnale per canale
features_peak<-function(df_x){
  
  channel_label<-c("Fz_peak", "Cz_peak", "Pz_peak", "Oz_peak", "P3_peak", "P4_peak", "PO7_peak", "PO8_peak")
  size<-ncol(df_x)/NUM_CHANNELS
  features_peak_values<-(matrix(ncol = NUM_CHANNELS,nrow = nrow(df_x)))
  
  for(i in seq_len(NUM_CHANNELS)){
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


#calcola la lunghezza della finestra temporale di picco picco del segnale per canale
features_time_windowPP<-function(df_x){
  
  channel_label<-c("Fz_PPeak", "Cz_TW", "Pz_TW", "Oz_TW", "P3_TW", "P4_TW", "PO7_TW", "PO8_TW")
  size<-ncol(df_x)/NUM_CHANNELS
  features_time_window<-(matrix(ncol = NUM_CHANNELS,nrow = nrow(df_x)))
  
  for(i in seq_len(NUM_CHANNELS)){
    #printf("Iterazione: %d\n",i)
    start<-((i-1)*size)+1
    end<-i*size
    channel<-df_x[,c(start:end)]
    max_time<- apply(channel, 1,which.max)
    min_time<- apply(channel,1,which.min)
    features_time_window[,i]<-(abs(max_time-min_time))
  }
  colnames(features_time_window)<-channel_label
  features_time_window <-as.data.frame(features_time_window)
  return(features_time_window)
}


#calcola quante volte il segnale passa per zero
features_crossing_zero<-function(df_x){
  print("Calcolo feature cz ...")
  
  channel_label<-c("Fz_Cross0", "Cz_Cross0", "Pz_Cross0", "Oz_Cross0", "P3_Cross0", "P4_Cross0", "PO7_Cross0", "PO8_Cross0")
  size<-ncol(df_x)/NUM_CHANNELS
  features_czero_values<-(matrix(ncol = NUM_CHANNELS,nrow = nrow(df_x)))
  
  for(i in seq_len(NUM_CHANNELS)){
    #printf("Iterazione: %d\n",i)
    start<-((i-1)*size)+1
    end<-i*size
    channel<-df_x[,c(start:end)]
    c_zero<- apply(channel,1,crossing_zero)
    features_czero_values[,i]<-c_zero
  }
  colnames(features_czero_values)<-channel_label
  features_czero_values <-as.data.frame(features_czero_values)
  print("Calcolo feature cz completato")
  return(features_czero_values)
}


#feature che calcola la potenza del segnale
features_signal_power<-function(df_x){
  
  channel_label<-c("Fz_Pow", "Cz_Pow", "Pz_Pow", "Oz_Pow", "P3_Pow", "P4_Pow", "PO7_Pow", "PO8_Pow")
  size<-ncol(df_x)/NUM_CHANNELS
  features_power<-(matrix(ncol = NUM_CHANNELS,nrow = nrow(df_x)))
  
  for(i in seq_len(NUM_CHANNELS)){
    #printf("Iterazione: %d\n",i)
    start<-((i-1)*size)+1
    end<-i*size
    channel<-df_x[,c(start:end)]
    pow<- apply(channel,1,signal_power)
    features_power[,i]<-pow
  }
  colnames(features_power)<-channel_label
  features_power <-as.data.frame(features_power)
  return(features_power)
}

#estrazione di un vettore contenente i valori relati alla P300 ideale
extract_P300 <- function(data) {
  
  channels <- c(1:NUM_CHANNELS)
  x <- seq(1:SAMPLE_POINTS)
  m <- data.frame()
  
  for (i in channels - 1) {
    start <- (i * SAMPLE_POINTS) + 1
    end <- SAMPLE_POINTS * (i + 1)
    target_data <- data[data$label == 1, c(start:end)]
    nontarget_data <- data[data$label == -1, c(start:end)]
    
    target_avg <- apply(target_data, 2, mean)
    nontarget_avg <- apply(nontarget_data, 2, mean)
    m <- rbind(m, target_avg)
  }
  
  average <- apply(m, 2, mean)
  matplot(x, average, type="l", col="red")
  return(as.numeric(average))
}


#Calcolo della correlazione tra la riga di dataframe e la P300 ideale
feature_corr_P300 <- function(df, p300){
  
  print("Calcolo feature Corr P300 ...")
  num_rows <- nrow(df)
  size<-ncol(df)/NUM_CHANNELS
  matrix_corr <- matrix(nrow = num_rows,ncol = NUM_CHANNELS)
  channel_label<-c("Fz_Corr", "Cz_Corr", "Pz_Corr", "Oz_Corr", "P3_Corr", "P4_Corr", "PO7_Corr", "PO8_Corr")
  for(i in seq_len(num_rows)){
    for(j in seq_len(NUM_CHANNELS)){
      start<-((j-1)*size)+1
      end<-j*size
      channel_row <- as.numeric(df[i,c(start:end)]) 
      matrix_corr[i,j] <- cor(p300,channel_row)
    }
  }
  colnames(matrix_corr)<-channel_label
  
  print("Calcolo feature Corr P300 completato")
  return(as.data.frame(matrix_corr))
}

# Metodo che dato un segnale in ingresso, mostra in uscita lo stesso
# segnale ma smussato.
feature_smooth <- function(df_x){
  
  rows <- nrow(df_x)
  matrix_smooth <-matrix(nrow = rows,ncol=ncol(df_x))
  
  for(i in seq_len(rows)){
    for(j in seq_len(NUM_CHANNELS)){
      start<-((j-1)*SAMPLE_POINTS)+1
      end<-j*SAMPLE_POINTS
      row_channel<-df_x[i,c(start:end)]
      matrix_smooth[i,c(start:end)] <- smooth(as.numeric(row_channel))
    }
  }
  return(as.data.frame(matrix_smooth))
}


# Dal dataset viene calcolato il segnale medio target e non target
# e come valore di output la differenza  tra i due segnali.
extract_Noise <- function(data) {
  
  channels <- c(1:NUM_CHANNELS)
  x <- seq(1:SAMPLE_POINTS)
  m <- data.frame()
  n <- data.frame()
  for (i in channels - 1) {
    start <- (i * SAMPLE_POINTS) + 1
    end <- SAMPLE_POINTS * (i + 1)
    target_data <- data[data$label == 1, c(start:end)]
    nontarget_data <- data[data$label == -1, c(start:end)]
    
    target_avg <- apply(target_data, 2, mean)
    nontarget_avg <- apply(nontarget_data, 2, mean)
    m <- rbind(m, target_avg)
    n <- rbind(n,nontarget_avg)
  }
  
  target <- apply(m, 2, mean)
  notarget <- apply(n, 2, mean)
  #matplot(x, average, type="l", col="red")
  return(as.numeric(target-notarget))
}

#Metodo che toglie dal dataset il rumore calcolato nella funzione precedente
feature_delete_noise <-function(df_x,noise){
  
  rows <- nrow(df_x)
  matrix_df <-matrix(nrow = rows,ncol=ncol(df_x))
  
  for(i in seq_len(rows)){
    for(j in seq_len(NUM_CHANNELS)){
      start<-((j-1)*SAMPLE_POINTS)+1
      end<-j*SAMPLE_POINTS
      row_channel<-df_x[i,c(start:end)]
      matrix_delete_noise[i,c(start:end)] <-row_channel-noise
    }
  }
  return(as.data.frame(matrix_delete_noise))
}