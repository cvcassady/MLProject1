## ML Project 1
## Lev Zadvinskiy
## Tyler Worthington
## Colin Cassady

### Classification
setwd("C:\\Users\\hexel\\Documents\\SYS 6016\\Project 1\\MLProject1")

# Load required packages
suppressPackageStartupMessages({
  library(readr)
  require(RCurl)
  library(lubridate)
  library(dplyr)
  library(caret)
  library(kernlab)
  library(klaR)
  library(randomForest)
  library(nnet)
  library(kernlab)
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
                 sizes = c(1:7), # run through combinations of predictors
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
                 sizes = c(1:7), # run through combinations of predictors
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



# The following non-tree-based methods require additional data preparation

### Start of data prep 

# Select predictors from the overall dataframe
fps <- fps[,c(11,5,6,7,8,12,13,14)]

# Normalize non-categorical variables
normalization <- preProcess(fps, method=c("scale", "center"))
fps <- as.data.frame(predict(normalization, fps))

# Create dummy variables for categorical predictors
dummies <- dummyVars(signs_of_mental_illness ~ ., data=fps)
predictors <- as.data.frame(predict(dummies, newdata=fps))

# Remove linear dependencies
comboInfo <- findLinearCombos(predictors)
predictors <- predictors[, -comboInfo$remove]

# Checking for correlation between predictors, remove those above .75 threshold
descr_cor <- cor(predictors)
summary(descr_cor[upper.tri(descr_cor)])
highlyCorDescr <- findCorrelation(descr_cor, cutoff=.75)
# High negative correlation between threat_level attack and threat_level other
# Algorithm suggests we remove threat_level attack, but for purposes of police shootings data analysis, we remove threat_level other
predictors <- predictors[,-14]

# Combine reponse and predictors into one data frame
df <- data.frame(fps$signs_of_mental_illness, predictors)
colnames(df)[1] <- "signs_of_mental_illness"

### End of data prep



### ANN

set.seed(123)
# set summary function to output binary classification statistics for ANN classifier
caretFuncs$summary <- twoClassSummary

# establish iterrative feature selection controlled parameters
ctrlANN <- rfeControl(functions = caretFuncs,
                     method = "repeatedcv", #train using repeated cross-validation
                     number = 10, # 10 folds
                     repeats = 3, # repeated 3 times
                     verbose = FALSE, # suppress output
                     saveDetails = TRUE, # save predictions and variable importance stats
                     returnResamp = "all") # save all resampled summary metrics

# run iterative feature selection process using ANN model
annProfile <- rfe(signs_of_mental_illness ~ ., 
                  data = df,
                  sizes = c(1,5,10,15:17), # run through combinations of predictors
                  method = "nnet", # call method
                  metric = "ROC", # use area under the curve as model selection metric
                  rfeControl = ctrlANN, # call traning control settings
                  trace = FALSE, # suppress output to console
                  linout = FALSE) # contrain sigmoidal function output to binary

# review the output
annProfile

# plot the best subset models based on the number of predictor variables
trellis.par.set(caretTheme())
plot(annProfile, type = c("g", "o"), main="AUC Score of Best Subset Models (ANN)")

# calculate variable importance based on contribution to ROC metric
ann_imp_vars <- varImp(annProfile)
ann_imp_vars

# calculate confusion matrix and various performance statistics
cmANN <- confusionMatrix.train(annProfile)
sensANN <- cmANN$table[1]/(cmANN$table[1]+cmANN$table[2])
specANN <- cmANN$table[4]/(cmANN$table[3]+cmANN$table[4])
precANN <- cmANN$table[1]/(cmANN$table[1]+cmANN$table[3])
recANN <- sensANN
bcrANN <- (sensANN+specANN)/2
F_ANN <- 2*(precANN*recANN)/(precANN+recANN)



### SVM

set.seed(123)
# set summary function to output binary classification statistics for SVM classifier
caretFuncs$summary <- twoClassSummary

# establish iterrative feature selection controlled parameters
ctrlSVM <- rfeControl(functions = caretFuncs,
                      method = "repeatedcv", #train using repeated cross-validation
                      number = 10, # 10 folds
                      repeats = 3, # repeated 3 times
                      verbose = FALSE, # suppress output
                      saveDetails = TRUE, # save predictions and variable importance stats
                      returnResamp = "all") # save all resampled summary metrics

# run iterative feature selection process using ANN model
svmProfile <- rfe(signs_of_mental_illness ~ ., 
                  data = df,
                  sizes = c(1,5,10,15:17), # run through combinations of predictors
                  method = "svmRadial", # call method
                  metric = "ROC", # use area under the curve as model selection metric
                  rfeControl = ctrlSVM, # call traning control settings
                  trace = FALSE, # suppress output to console
                  linout = FALSE) # contrain sigmoidal function output to binary

# review the output
svmProfile

# plot the best subset models based on the number of predictor variables
trellis.par.set(caretTheme())
plot(svmProfile, type = c("g", "o"), main="AUC Score of Best Subset Models (SVM)")

# calculate variable importance based on contribution to ROC metric
svm_imp_vars <- varImp(svmProfile)
svm_imp_vars

# calculate confusion matrix and various performance statistics
cmSVM <- confusionMatrix.train(svmProfile)
sensSVM <- cmSVM$table[1]/(cmSVM$table[1]+cmSVM$table[2])
specSVM <- cmSVM$table[4]/(cmSVM$table[3]+cmSVM$table[4])
precSVM <- cmSVM$table[1]/(cmSVM$table[1]+cmSVM$table[3])
recSVM <- sensSVM
bcrSVM <- (sensSVM+specSVM)/2
F_SVM <- 2*(precSVM*recSVM)/(precSVM+recSVM)

