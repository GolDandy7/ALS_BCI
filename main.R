library("dplyr")
library("LiblineaR")
library("e1071")
library(CORElearn)
library("R.utils")
library(DescTools)
library(Bolstad2)

source("data_split.R")
source("utils.R")
source("new_c_data.R")
source("normalization.R")
source("cross_validation.R")
source("cross_CT_validation.R")
source("choosing_cross_validation.R")
source("test_accuracy.R")
source("decision_function.R")
source("feature_selection.R")

#Caricamento Dati

df_x <- read.table("X.txt",header = FALSE)
df_c <- read.table("C.txt",header = FALSE)
df_y <- read.table("Y.txt",header = FALSE)

#Applicazione delle etichette sulle colonne del df
df_x <- apply_labels(df_x)

#set del seme per la ripetibilità dell'esperimento
set.seed(123)


#------------features---------------------
features_area<-features_area_channel(df_x)
features_area_positive <-features_positive_area_channel(df_x)
features_area_negative <-features_negative_area_channel(df_x)
features_rt<-features_rt_channel(df_x)
features_cz<-features_crossing_zero(df_x)
feature_pp<-features_peak_to_peak(df_x)

#se aggiungo feature_p peggioro in cross validation e rimango uguale sul test
#-----------------------------------------
#trasformo le C in una matrice  di 0 e 1 perchè la numerazione mi crea ordinamento
my_c_data<-new_c_data(df_c)
new_data<-cbind(df_c, df_x, my_c_data)
new_data<-cbind(new_data,features_area)
new_data<-cbind(new_data,features_rt)
new_data<-cbind(new_data,features_cz)
new_data<-cbind(new_data,features_area_negative)
new_data<-cbind(new_data,features_area_positive)
new_data<-cbind(new_data,feature_pp)
new_data<-cbind(new_data,df_y)

#new_data<-cbind(df_x, df_y)


#split dei dati in training e test: data$train e data$test 
data<-data_split(new_data)

#normalizzazione
scaled_data = normalize(data)

#---------crossvalidazione-------------
#scegliamo il valore migliore di loss e c
#outcome<- choosing_cross_validation(scaled_data$train)
#rifacciamo cross_validation sul train
model <- cross_validation(scaled_data$train)

accuracy <- test_accuracy(model, scaled_data$test)
accuracy
