library("dplyr")
library("LiblineaR")
library("e1071")
library(CORElearn)
library("R.utils")
library(DescTools)
library(Bolstad2)

source("constants.R")
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
dfx <- read.table("X.txt", header = FALSE)
dfc <- read.table("C.txt", header = FALSE)
dfy <- read.table("Y.txt", header = FALSE)

#Applicazione delle etichette sulle colonne del df
dfx <- apply_labels(dfx)
colnames(dfy) <- "label"

#set del seme per la ripetibilità dell'esperimento
set.seed(123)

dfxy <- cbind(dfx, dfy)
dfcxy <- cbind(dfc, dfx, dfy)
df_cxy <- dfcxy

#data augmentation
#df_cxy <- rbind(dfcxy, generate_data(dfcxy, 0.3, put_noise))
#df_cxy <- rbind(dfcxy, generate_data(dfcxy, 0.3, right_shift, shift_size = floor(SAMPLE_POINTS * 0.1)))

df_c <- as.data.frame(df_cxy[, 1])
df_x <- as.data.frame(df_cxy[, -c(1, ncol(df_cxy))])
df_y <- as.data.frame(df_cxy[, ncol(df_cxy)])

#------------features---------------------
features_area<-features_area_channel(df_x)
features_rt<-features_rt_channel(df_x)
#se aggiungo feature_p peggioro in cross validation e rimango uguale sul test
#-----------------------------------------
#trasformo le C in una matrice  di 0 e 1 perchè la numerazione mi crea ordinamento
c_bin <- new_c_data(df_c)
featured_data <- cbind(df_c, df_x, c_bin, features_area, features_rt, df_y)
#featured_data <- cbind(df_c, df_x, c_bin, df_y)


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
