library("dplyr")
library("LiblineaR")
library("e1071")
library(CORElearn)

source("data_split.R")
source("utils.R")


#Caricamento Dati

df_x <- read.table("X.txt",header = FALSE)
df_c <- read.table("C.txt",header = FALSE)
df_y <- read.table("Y.txt",header = FALSE)

#Applicazione delle etichette sulle colonne del df
df_x <- apply_labels(df_x)

#trasformo le C in una matrice  di 0 e 1 perchè la numerazione mi crea ordinamento
my_c_data<-new_c_data(df_c)
new_data<-cbind(df_x, my_c_data, df_y)

#my_data contenente i dati splittati
my_data<-data_split(new_data)

#scegliamo il classificatore in cross-validation
#dati già normalizzati in ingresso
#cross_validation_output<-choosing_classifier(my_data)
