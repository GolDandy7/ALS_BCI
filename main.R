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
source("data_understanding.R")


#Caricamento Dati
dfx <- read.table("X.txt", header = FALSE)
dfc <- read.table("C.txt", header = FALSE)
dfy <- read.table("Y.txt", header = FALSE)

#Applicazione delle etichette sulle colonne del df
dfx <- apply_labels(dfx)
colnames(dfy) <- "label"

#set del seme per la ripetibilità dell'esperimento
set.seed(456)

dfxy <- cbind(dfx, dfy)
dfcxy <- cbind(dfc, dfx, dfy)


#split dei dati in training e test: $train e $test 
splitted_data <- data_split(dfcxy)

training_set <- splitted_data$train
test_set <- splitted_data$test


#data augmentation
#df_cxy <- rbind(dfcxy, generate_data(dfcxy, 0.3, put_noise))
#df_cxy <- rbind(dfcxy, generate_data(dfcxy, 0.3, right_shift, shift_size = floor(SAMPLE_POINTS * 0.1)))


#feature selection
p300 <- extract_P300(get_xydf(training_set))
featured_train <- feature_selection(training_set, p300)
featured_test <- feature_selection(test_set, p300)


#normalizzazione
scaled_data = normalize(featured_train, featured_test)


#---------crossvalidazione-------------
#scegliamo il valore migliore di loss e c
#outcome<- choosing_cross_validation(scaled_data$train)
#rifacciamo cross_validation sul train
model <- cross_validation(scaled_data$train)


#test
accuracy <- test_accuracy(model, scaled_data$test)
accuracy
