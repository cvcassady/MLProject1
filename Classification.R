# Classification
setwd("C:\\Users\\hexel\\Documents\\SYS 6016\\Project 1\\")

suppressPackageStartupMessages({
  library(readr)
  library(doParallel)
  library(lubridate)
  library(dplyr)
  library(ggplot2)
  library(caret)
  library(klaR)
  library(MLmetrics)
  library(rpart)
  library(rpart.plot)
  library(rattle)
})

fps <- read_rds("fps.rds")

y <- fps$signs_of_mental_illness
x <- fps[,c("armed","age","gender","race","threat_level","flee","body_camera")]
normalization <- preProcess(x)
x <- predict(normalization, x)
x <- as.data.frame(x)

subsets <- c(1:7)
# Naive Bayes model selection
set.seed(123)

nbFuncs$summary <- twoClassSummary

ctrlNB <- rfeControl(functions = nbFuncs,
                     method = "repeatedcv",
                     repeats = 10,
                     verbose = FALSE)

nbProfile <- rfe(x, y,
                 sizes = subsets,
                 method = "nb",
                 metric= "ROC",
                 rfeControl = ctrlNB)

nbProfile

head(nbProfile$resample)

trellis.par.set(caretTheme())
plot(nbProfile, type = c("g", "o"), main="AUC Score of Best Subset Models")

# Naive Bayes model with all 7 predictors
ctrl <- trainControl(method="repeatedcv", repeats=10, number=10, savePredictions=TRUE,
                     summaryFunction=twoClassSummary, classProbs=TRUE)

set.seed(123)
modNB1 <- train(x=x, y=y, method="nb", trControl=ctrl, metric="ROC", verbose=FALSE)
# AUC for the model
AUC1 <- max(modNB1$results$ROC)
# Confusion matrix
CM1 <- confusionMatrix.train(modNB1)
# Some metrics
sens <- CM1$table[1]/(CM1$table[1]+CM1$table[2])
spec <- CM1$table[4]/(CM1$table[3]+CM1$table[4])
prec <- CM1$table[1]/(CM1$table[1]+CM1$table[3])
recall <- sens
bal_acc <- (sens+spec)/2
# Variable importance graph
imp_vars <- varImp(modNB1, scale=FALSE)
plot(imp_vars, main="ROC Curve Variable Importance")

# Naive Bayes model without 'gender' predictor
x2 <- x[,-3]
set.seed(123)
modNB2 <- train(x=x2, y=y, method="nb", trControl=ctrl, metric="ROC", verbose=FALSE)
# AUC for the model
AUC2 <- max(modNB2$results$ROC)
# Confusion matrix
CM2 <- confusionMatrix.train(modNB2)
# Some metrics
sens2 <- CM2$table[1]/(CM2$table[1]+CM2$table[2])
spec2 <- CM2$table[4]/(CM2$table[3]+CM2$table[4])
prec2 <- CM2$table[1]/(CM2$table[1]+CM2$table[3])
recall2 <- sens2
bal_acc2 <- (sens2+spec2)/2


### Other classifiers

# ## C5.0 decision tree
# library(C50)
# 
# set.seed(123)
# 
# C50ctrl <- trainControl(method="repeatedcv", repeats=10, number=10, savePredictions=TRUE,
#                      summaryFunction=twoClassSummary, classProbs=TRUE)
# 
# tree <- train(x=x, y=y, method="C5.0", metric="ROC", control = C5.0Control(earlyStopping = FALSE), 
#               trControl=C50ctrl, verbose=FALSE)
# 
# out <- summary(tree)
# 
# confusionMatrix(tree)


## Rpart decision tree
library(rpart)
library(rpart.plot)
library(rattle)

set.seed(123)

rpartctrl <- trainControl(method="repeatedcv", repeats=10, number=10, savePredictions=TRUE,
                        summaryFunction=twoClassSummary, classProbs=TRUE)

tree2 <- train(x=x, y=y, metric="ROC", method="rpart", trControl=rpartctrl)

out2 <- summary(tree2)

confusionMatrix(tree2)

fancyRpartPlot(tree2$finalModel)


## Random Forest
library(randomForest)

x2 <- x[,-1]

set.seed(123)

rfFuncs$summary <- twoClassSummary

ctrlRF <- rfeControl(functions = rfFuncs,
                     method = "repeatedcv",
                     repeats = 5,
                     verbose = FALSE)

rfProfile <- rfe(x2, y,
                 sizes = subsets,
                 method = "rf",
                 metric= "ROC",
                 rfeControl = ctrlRF)

rfProfile

head(rfProfile$resample)

trellis.par.set(caretTheme())
plot(rfProfile, type = c("g", "o"), main="AUC Score of Best Subset Models (Random FOrest)")



# ## xgBoost
# library(xgboost)
# 
# 
# xgbTree <- train(x, y, method="xgbTree", metric="Accuracy", trControl=trainControl(method="cv", number=10), nthread =4)



# ## Boosted Logit 
# library(caTools)
# 
# set.seed(123)
# 
# caretFuncs$summary <- twoClassSummary
# 
# ctrlLB <- rfeControl(method = "repeatedcv",
#                      repeats = 10,
#                      verbose = FALSE)
# 
# lbProfile <- rfe(x, y,
#                  sizes = subsets,
#                  method = "LogitBoost",
#                  metric= "ROC",
#                  rfeControl = ctrlLB)
# 
# lbProfile
# 
# lb <- train(x, y, method="LogitBoost", metric="Accuracy", trControl=trainControl(method="cv", number=10), nIter=10)



# ## kNN
# set.seed(123)
# 
# caretFuncs$summary <- twoClassSummary
# 
# ctrlKNN <- rfeControl(functions = caretFuncs,
#                       method = "repeatedcv",
#                       repeats = 10,
#                       verbose = FALSE)
# 
# knnProfile <- rfe(x, y,
#                  sizes = subsets,
#                  method = "knn",
#                  metric= "ROC",
#                  rfeControl = ctrlKNN)
# 
# knnProfile
# 
# knn <- train(x, y, method="knn", metric="ROC", trControl=trainControl(method="cv", number=10, 
#                                                                      savePredictions=TRUE,
#                                                                      summaryFunction=twoClassSummary, 
#                                                                      classProbs=TRUE))



### NB model selection for body_camera predictions
ytl <- fps$body_camera
xtl <- fps[,c("armed","age","gender","race","signs_of_mental_illness","flee","threat_level")]
normalization <- preProcess(xtl)
xtl <- predict(normalization, xtl)
xtl <- as.data.frame(xtl)

subsets <- c(1:7)
# Naive Bayes model selection
set.seed(123)

nbFuncs$summary <- twoClassSummary

ctrlNB <- rfeControl(functions = nbFuncs,
                     method = "repeatedcv",
                     repeats = 10,
                     verbose = FALSE)

nbProfile2 <- rfe(xtl, ytl,
                 sizes = subsets,
                 method = "nb",
                 metric= "ROC",
                 rfeControl = ctrlNB)

nbProfile2

head(nbProfile2$resample)

trellis.par.set(caretTheme())
plot(nbProfile2, type = c("g", "o"), main="AUC Score of Best Subset Models")




### Logit
set.seed(123)
training.indices <- sample(1:nrow(fps), as.integer(nrow(fps) * 0.75))
training.set <- fps[training.indices,]
testing.set <- fps[-training.indices,]
training.set <- training.set[,c("signs_of_mental_illness","armed","age","gender","race","threat_level","flee","body_camera")]
testing.set <- testing.set[,c("signs_of_mental_illness","armed","age","gender","race","threat_level","flee","body_camera")]

model <- glm(signs_of_mental_illness ~., data=training.set, family=binomial(link='logit'))

fitted.results <- predict(model, testing.set[,-1], type="response")
