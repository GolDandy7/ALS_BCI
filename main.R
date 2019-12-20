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

my_data<-data_split(df_x)
