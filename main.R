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


#Applicazione delle etichette sulle colonne del df
#dfx_training <- feature_smooth(dfx_training)
dfx_training <- apply_labels(dfx_training)
colnames(dfc_training) <- "c"
colnames(dfy_training) <- "label"


#set del seme per la ripetibilità dell'esperimento
set.seed(123)


# bind delle istanze con le corrispondenti etichette
dfxy <- cbind(dfx_training, dfy_training)
dfcxy <- cbind(dfc_training, dfx_training, dfy_training)



#split dei dati in training e test: $train e $test 
splitted_data <- data_split(dfcxy)

training_set <- splitted_data$train
test_set <- splitted_data$test


#data augmentation
augmented_train <- rbind(training_set, 
                         generate_data(training_set, 0.6, meanchar_gen, n=2))


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


#crossvalidazione
#scelta del valore migliore di loss e c
#outcome<- choosing_cross_validation(scaled_data$train)
cv_results <- cross_validation(scaled_data$train, 
                               classifier = LiblineaR, 
                               type = 7, cost = 0.01, bias = TRUE, verbose = FALSE)
print(cv_results)


#test
accuracy <- test_accuracy(model, scaled_data$test)
printf("Caratteri predetti correttamente: %.2f %%", accuracy * 100)
