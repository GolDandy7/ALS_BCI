library("dplyr")
library("LiblineaR")
library("e1071")
library(CORElearn)
library("R.utils")
library(Bolstad2)
library(DescTools)
source("constants.R")
source("data_split.R")
source("utils.R")
source("new_c_data.R")
source("normalization.R")
source("cross_validation.R")
source("cross_validation_param.R")
source("choosing_cross_validation.R")
source("test_accuracy.R")
source("decision_function.R")
source("feature_selection.R")
source("data_augmentation.R")
source("data_understanding.R")


#Caricamento Dati
dfx_training <- read.table("X.txt", header = FALSE)
dfc_training <- read.table("C.txt", header = FALSE)
dfy_training <- read.table("Y.txt", header = FALSE)
#dfx_test <- read.table("X_test.txt", header = FALSE)
#dfc_test <- read.table("C_test.txt", header = FALSE)
#dfy_test <- read.table("Y_test.txt", header = FALSE)

#Applicazione delle etichette sulle colonne del df
#dfx_training <- feature_smooth(dfx_training)
dfx_training <- apply_labels(dfx_training)
colnames(dfc_training) <- "c"
colnames(dfy_training) <- "label"

#set del seme per la ripetibilità dell'esperimento
set.seed(123)

#eliminazione canali
#fourth_ccol <- c(((4-1) * SAMPLE_POINTS + 1):(4 * SAMPLE_POINTS))
#seventh_ccol <- c(((7-1) * sample_pointS + 1):(7 * SAMPLE_POINTS))
#dfx <- dfx_training[, -c(fourth_ccol, seventh_ccol)]

# bind delle istanze con le corrispondenti etichette
dfxy <- cbind(dfx_training, dfy_training)
dfcxy <- cbind(dfc_training, dfx_training, dfy_training)



#split dei dati in training e test: $train e $test 
splitted_data <- data_split(dfcxy)

training_set <- splitted_data$train
test_set <- splitted_data$test


#data augmentation
#augmented_train <- rbind(dfcxy, generate_data(dfcxy, 0.3, put_noise))
#augmented_train <- rbind(dfcxy, generate_data(dfcxy, 0.3, right_shift, shift_size = floor(SAMPLE_POINTS * 0.1)))
augmented_train <- rbind(training_set, 
                         generate_data(training_set, 0.3, meanchar_gen, n=2))
#augmented_train <- training_set


#feature selection
p300 <- extract_P300(get_xydf(augmented_train))
featured_train <- feature_selection(augmented_train, p300)
featured_test <- feature_selection(test_set, p300)


#normalizzazione
scaled_data = normalize(featured_train, featured_test)


#training
model <- LiblineaR(data = get_xdf(scaled_data$train),
                   target = get_ydf(scaled_data$train),
                   type = 7, cost = 0.01, bias = TRUE, verbose = FALSE)

#---------crossvalidazione-------------
#scegliamo il valore migliore di loss e c
#outcome<- choosing_cross_validation(scaled_data$train)
#rifacciamo cross_validation sul train
cv_results <- cross_validation(scaled_data$train, 
                               classifier = LiblineaR, 
                               type = 7, cost = 0.01, bias = TRUE, verbose = FALSE)
print(cv_results)


#test
accuracy <- test_accuracy(model, scaled_data$test)
printf("Caratteri predetti correttamente: %.2f %%", accuracy * 100)
