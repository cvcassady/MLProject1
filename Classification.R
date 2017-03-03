## ML Project 1
## Lev Zadvinskiy
## Tyler Worthington
## Colin Cassady

### Classification (part 1)
setwd("C:\\Users\\hexel\\Documents\\SYS 6016\\Project 1\\MLProject1")

# Load required packages
suppressPackageStartupMessages({
  library(readr)
  require(RCurl)
  library(lubridate)
  library(dplyr)
  library(caret)
  library(klaR)
  library(randomForest)
})



# The purpose of below classifiers is to establish a link between mental illness and other factors 
# contributing to fatal shooting. By fine tuning classifiers using various algorithms we want to
# definitely infer the ability of a police officer to recognize mental illness in a suspect. Our null
# hypothesis is that police cannot detect the signs of mental illness in a suspect. The alternative
# hypothesis is that police can detect the signs of mental illnes. 



# Load clean data frame
fps <- read_rds("fps.rds")

# Normalize continuous variables within the data frame
# Divide the data into y (response) and x (predictors)
y <- fps$signs_of_mental_illness
x <- fps[,c("armed","age","gender","race","threat_level","flee","body_camera")]
normalization <- preProcess(x)
x <- predict(normalization, x)
x <- as.data.frame(x)



### Naive Bayes model selection

set.seed(123) 
# set summary function to output binary classification statistics for Naive Bayes classifier
nbFuncs$summary <- twoClassSummary

# establish iterrative feature selection controlled parameters
ctrlNB <- rfeControl(functions = nbFuncs,
                     method = "repeatedcv", #train using repeated cross-validation
                     number = 10, # 10 folds
                     repeats = 3, # repeated 3 times
                     verbose = FALSE, # suppress output
                     saveDetails = TRUE, # save predictions and variable importance stats
                     returnResamp = "all") # save all resampled summary metrics

# run iterative feature selection process using Naive Bayes model
nbProfile <- rfe(x, y,
                 sizes = c(1:5,10,15,20,25), # run through various subset sizes
                 method = "nb", # call method
                 metric = "ROC", # use area under the curve as model selection metric
                 rfeControl = ctrlNB) # call traning control settings

# review the output
nbProfile

# plot the best subset models based on the number of predictor variables
trellis.par.set(caretTheme())
plot(nbProfile, type = c("g", "o"), main="AUC Score of Best Subset Models (Naive Bayes)")

# calculate variable importance based on contribution to ROC metric
nb_imp_vars <- varImp(nbProfile)
nb_imp_vars

# calculate confusion matrix and various performance statistics
cmNB <- confusionMatrix.train(nbProfile)
sensNB <- cmNB$table[1]/(cmNB$table[1]+cmNB$table[2])
specNB <- cmNB$table[4]/(cmNB$table[3]+cmNB$table[4])
precNB <- cmNB$table[1]/(cmNB$table[1]+cmNB$table[3])
recNB <- sensNB
bcrNB <- (sensNB+specNB)/2
F_NB <- 2*(precNB*recNB)/(precNB+recNB)



### Random Forest

set.seed(123)
# set summary function to output binary classification statistics for Random Forest classifier
rfFuncs$summary <- twoClassSummary

# establish iterrative feature selection controlled parameters
ctrlRF <- rfeControl(functions = rfFuncs,
                     method = "repeatedcv", #train using repeated cross-validation
                     number = 10, # 10 folds
                     repeats = 3, # repeated 3 times
                     verbose = FALSE, # suppress output
                     saveDetails = TRUE, # save predictions and variable importance stats
                     returnResamp = "all") # save all resampled summary metrics

# run iterative feature selection process using Random Forest model
rfProfile <- rfe(x, y,
                 sizes = c(1:5,10,15,20,25), # run through various subset sizes
                 method = "rf", # call method
                 metric = "ROC", # use area under the curve as model selection metric
                 rfeControl = ctrlRF) # call traning control settings

# review the output
rfProfile

# plot the best subset models based on the number of predictor variables
trellis.par.set(caretTheme())
plot(rfProfile, type = c("g", "o"), main="AUC Score of Best Subset Models (Random Forest)")

# calculate variable importance based on contribution to ROC metric
rf_imp_vars <- varImp(rfProfile)
rf_imp_vars

# calculate confusion matrix and various performance statistics
cmRF <- confusionMatrix.train(rfProfile)
sensRF <- cmRF$table[1]/(cmRF$table[1]+cmRF$table[2])
specRF <- cmRF$table[4]/(cmRF$table[3]+cmRF$table[4])
precRF <- cmRF$table[1]/(cmRF$table[1]+cmRF$table[3])
recRF <- sensRF
bcrRF <- (sensRF+specRF)/2
F_RF <- 2*(precRF*recRF)/(precRF+recRF)
