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
source("data_augmentation.R")

#Caricamento Dati

df_x <- read.table("X.txt",header = FALSE)
df_c <- read.table("C.txt",header = FALSE)
df_y <- read.table("Y.txt",header = FALSE)

#Applicazione delle etichette sulle colonne del df
df_x <- apply_labels(df_x)


#set del seme per la ripetibilità dell'esperimento
set.seed(123)

df_cxy <- cbind(df_c, df_x, df_y)

#data augmentation
df_cxy <- augment_data(df_cxy, 0.3)
df_c <- as.data.frame(df_cxy[, 1])
df_x <- as.data.frame(df_cxy[, -c(1, ncol(df_cxy))])
df_y <- as.data.frame(df_cxy[, ncol(df_cxy)])

#------------features---------------------
features_area<-features_area_channel(df_x)
features_rt<-features_rt_channel(df_x)
feature_p<-features_positive(df_x)
#se aggiungo feature_p peggioro in cross validation e rimango uguale sul test
#-----------------------------------------
#trasformo le C in una matrice  di 0 e 1 perchè la numerazione mi crea ordinamento
c_bin <- new_c_data(df_c)
featured_data <- cbind(df_c, df_x, c_bin, features_area, features_rt, feature_p, df_y)


#split dei dati in training e test: data$train e data$test 
splitted_data <- data_split(featured_data)

#normalizzazione
scaled_data = normalize(splitted_data)

#---------crossvalidazione-------------
#scegliamo il valore migliore di loss e c
#outcome<- choosing_cross_validation(scaled_data$train)
#rifacciamo cross_validation sul train
model <- cross_validation(scaled_data$train)

accuracy <- test_accuracy(model, scaled_data$test)
accuracy
