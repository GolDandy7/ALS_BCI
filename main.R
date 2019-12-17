library("dplyr")
library("LiblineaR")
library("e1071")
library(CORElearn)

source("data_split.R")

#Caricamento Dati

df_x <- read.table("X.txt",header = FALSE)
df_c <- read.table("C.txt",header = FALSE)
df_y <- read.table("Y.txt",header = FALSE)


my_data<-data_split(df_x)
