library("dplyr")
library("LiblineaR")
library("e1071")
library(CORElearn)

source("data_split.R")
source("utils.R")
source("new_c_data.R")
source("cross_validation.R")
source("cross_CT_validation.R")
source("choosing_cross_validation.R")
source("test_accuracy.R")

#Caricamento Dati

df_x <- read.table("X.txt",header = FALSE)
df_c <- read.table("C.txt",header = FALSE)
df_y <- read.table("Y.txt",header = FALSE)

#Applicazione delle etichette sulle colonne del df
df_x <- apply_labels(df_x)

#trasformo le C in una matrice  di 0 e 1 perchè la numerazione mi crea ordinamento
my_c_data<-new_c_data(df_c)
#new_data<-cbind(df_x, my_c_data, df_y)
new_data<-cbind(df_x, df_y)

#my_data contenente i dati splittati
data<-data_split(new_data)

#---------crossvalidazione-------------
#scegliamo il valore migliore di loss e c
#outcome<- choosing_cross_validation(data$train)
#rifacciamo cross_validation sul train
v <- cross_validation(data$train)
accuracy <- test_accuracy(data$test)
accuracy
