## ML Project 1
## Lev Zadvinskiy
## Tyler Worthington
## Colin Cassady

### Classification (part 2)
setwd("C:\\Users\\hexel\\Documents\\SYS 6016\\Project 1\\MLProject1")

# Load required packages
suppressPackageStartupMessages({
  library(readr)
  require(RCurl)
  library(lubridate)
  library(dplyr)
  library(caret)
  library(nnet)
  library(kernlab)
})



# The porpose of below classifiers is to explore non-tree-based methods of classification of
# signs of mental illness. Our null hypothesis is that police cannot detect the signs of mental
# illness in a suspect. The alternative hypothesis is that police can detect the signs of mental illnes. 



### Start of data prep 

# Load in the fatal police shootings data frame
fps <- read_rds("fps.rds")

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



### Split the data into train and validation sets

set.seed(123)
trainIndex <- createDataPartition(df$signs_of_mental_illness, p=.75, list=FALSE)
train <- df[trainIndex,]
test <- df[-trainIndex,]



### ANN

# set training parameters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
my.grid <- expand.grid(.decay=c(0.5, 0.1), .size=1:17)
# train the model
set.seed(123)
ANN.fit <- train(signs_of_mental_illness ~ ., data=train, method="nnet", 
                 maxit=1000, trControl=control, tuneGrid=my.grid, trace=FALSE, linout=FALSE) 
# find predictions
predsANN <- predict(ANN.fit, newdata=test[,-1])
# calculate confusion matrix and various performance statistics
cmANN <- confusionMatrix(test[,1],predsANN)
sensANN <- cmANN$table[1]/(cmANN$table[1]+cmANN$table[2])
specANN <- cmANN$table[4]/(cmANN$table[3]+cmANN$table[4])
precANN <- cmANN$table[1]/(cmANN$table[1]+cmANN$table[3])
recANN <- sensANN
bcrANN <- (sensANN+specANN)/2
F_ANN <- 2*(precANN*recANN)/(precANN+recANN)



### SVM

# set training parameters
ctrlSVM <- trainControl(method="repeatedcv", number=10, repeats=3, 
                        savePredictions=TRUE, classProbs=TRUE, summaryFunction=twoClassSummary)
# train the model
set.seed(123)
svm.fit <- train(signs_of_mental_illness ~ ., data=train, method="svmRadial",
                  tuneLength=9,	metric="ROC", trControl=ctrlSVM)
svm.fit
# find predictions
predsSVM <- predict(svm.fit, newdata=test[,-1])
# calculate confusion matrix and various performance statistics
cmSVM <- confusionMatrix(test[,1],predsSVM)
sensSVM <- cmSVM$table[1]/(cmSVM$table[1]+cmSVM$table[2])
specSVM <- cmSVM$table[4]/(cmSVM$table[3]+cmSVM$table[4])
precSVM <- cmSVM$table[1]/(cmSVM$table[1]+cmSVM$table[3])
recSVM <- sensSVM
bcrSVM <- (sensSVM+specSVM)/2
F_SVM <- 2*(precSVM*recSVM)/(precSVM+recSVM)
