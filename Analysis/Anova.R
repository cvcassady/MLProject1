#ANOVA Analysis on Data
#Is there a statistical difference in victims when a body camera is present
library(readr)
require(RCurl)
library(plyr)

#read in data 
shootings <-read.csv(text=getURL("https://raw.githubusercontent.com/cvcassady/MLProject1/master/Data/fatal-police-shootings-data.csv"),header=T)

#target attribute
summary((shootings$body_camera))

######Testing Age
#boxplots
boxplot(age~body_camera,data=shootings, main="Distribution of Age", 
        xlab="Body Camera", ylab="Age")
#anova
a1 <- aov(shootings$age ~ shootings$body_camera)
summary(a1)

######Testing Race
shootings$race <- mapvalues(shootings$race, from = c("A", "B", "H", "N", "O", "W"), to = c("Asian", "Black", "Hispanic", "Native American", "Other", "White"))
race <- table(shootings$race, shootings$body_camera)
prop.table(race, 2)
chisq.test(race)

######threat level
threat <- table(shootings$threat, shootings$body_camera)
prop.table(threat, 2)
chisq.test(threat)

######mental illness
ill <- table(shootings$signs_of_mental_illness, shootings$body_camera)
prop.table(ill, 2)
chisq.test(ill)

######gender
gender <- table(shootings$gender, shootings$body_camera)
prop.table(gender, 2)
chisq.test(gender)

######manner of death
death <- table(shootings$manner_of_death, shootings$body_camera)
prop.table(death, 2)
chisq.test(death)

######flee
flee <- table(shootings$flee, shootings$body_camera)
prop.table(flee, 2)
chisq.test(flee)

#####armed
armed <- table(shootings$armed, shootings$body_camera)
prop.table(armed, 2)
chisq.test(armed)

#####Region
shootings$region <- mapvalues(shootings$state, from = c("ME","NH","VT","MA","RI","CT","NY","PA","NJ"), to = c("Northeast", "Northeast","Northeast", "Northeast","Northeast", "Northeast","Northeast", "Northeast","Northeast"))



