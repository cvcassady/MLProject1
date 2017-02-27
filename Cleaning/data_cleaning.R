# Data cleaning 

suppressPackageStartupMessages( {
  library(lubridate)
  library(dplyr)
  library(ggplot2)
  library(e1071)
  library(caret)
})

setwd("C:\\Users\\hexel\\Documents\\SYS 6016\\Project 1\\")

fps <- read.csv("data-police-shootings\\fatal-police-shootings-data.csv", header=T)

summary(fps)

## id is good

## make name a character vector
fps$name <- as.character(fps$name)

## make dates dates
fps$date <- ymd(fps$date)

## manner_of_death is good, though pointless

## set NA's in armed to undetermined
fps$armed <- as.character(fps$armed)
fps$armed <- factor(ifelse(fps$armed=="", "undetermined", fps$armed))

## age has 48 NA's, median imputation is possible
fps$age[is.na(fps$age)] <- median(fps$age, na.rm=T)

## no gender, name or race info id 2346, remove from data frame
fps <- fps[!(fps$gender==""),]
# re-factor gender variable
fps$gender <- factor(as.character(fps$gender))

## 127 missing obs in race
levels(fps$race)
fps$name[fps$race==""]
# can't really impute the race if the race is unknown
# we should probably remove the NA's for race 
fps <- fps[!(fps$race==""),]

## make city a character vector
fps$city <- as.character(fps$city)

## make state a character vector
fps$state <- as.character(fps$state)

## signs_of_mental_illness is good

## thread_level is good 
levels(fps$threat_level)
# can probably combine other and undetermined together
fps$threat_level <- as.character(fps$threat_level)
fps$threat_level <- factor(ifelse(fps$threat_level=="other" | fps$threat_level=="undetermined", "other", fps$threat_level))

## set missing flee observations to other
fps$flee <- as.character(fps$flee)
fps$flee <- factor(ifelse(fps$flee=="", "Other", fps$flee))

## body_camera is good

## Subset to 2015 and 2016 police shootings only
fps <- fps[fps$date<="2016-12-31",]

## Basic Naive Bayes implementation
# train/test set division
set.seed(123)
indices = sample(1:nrow(fps), as.integer(nrow(fps)*0.75))
train = fps[indices,]
test= fps[-indices,]
# NB on signs_of_mental_illness
modNB <- naiveBayes(signs_of_mental_illness ~ armed+age+gender+race+threat_level+flee+body_camera, data=train)
pred <- predict(modNB, test)
confusionMatrix(pred, test$signs_of_mental_illness)
# NB on threat_level
modNB2 <- naiveBayes(threat_level ~ armed+age+gender+race+signs_of_mental_illness+flee+body_camera, data=train)
pred2 <- predict(modNB2, test)
confusionMatrix(pred2, test$threat_level)



# some other findings
summary(fps$race)
nrow(fps[fps$signs_of_mental_illness=="True" & fps$threat_level=="attack",]) # 288
nrow(fps[fps$signs_of_mental_illness=="True",]) # 471
nrow(fps[fps$signs_of_mental_illness=="False" & fps$threat_level=="attack",]) # 928
nrow(fps[fps$signs_of_mental_illness=="False",]) # 1392


# Obvious improvement would be to run this model through CV
# We can try SVM, Random Forest and ANN
