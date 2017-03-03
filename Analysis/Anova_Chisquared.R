#ANOVA Analysis on Data
#Is there a statistical difference in victims when a body camera is present
library(readr)
require(RCurl)
library(plyr)

#read in data 
shootings <-read.csv(text=getURL("https://raw.githubusercontent.com/cvcassady/MLProject1/master/Data/fatal-police-shootings-data.csv"),header=T)
census <-read.csv(text=getURL("https://raw.githubusercontent.com/cvcassady/MLProject1/master/Data/shootings_census.csv"), header = T)

#target attribute
summary((shootings$body_camera))

#Data Cleaning
#condensing states in to regions to reduce number of levels
shootings$region<- ifelse(shootings$state %in% c("ME","NH","VT","MA","RI","CT","NY","PA","NJ"),"Northeast",
                          ifelse(shootings$state %in% c("MD","DE","DC","VA","WV","NC","SC","KY","TN","GA","FL","AL","MS","AR","OK","TX","LA"),"South",
                                 ifelse(shootings$state %in% c("ND","SD","NE","KS","MN","IA","WI","IL","IN","OH","MI","MO"),"Midwest","West")))

#codensing armed type into condensed categories
shootings$armed <- as.character(shootings$armed)
shootings$armed <- ifelse(shootings$armed %in% c("ax","baseball bat","baseball bat and fireplace poker","baton","bayonet","beer bottle","blunt object",
                                                 "box cutter","brick","carjack","chain","chain saw","contractor's level","cordless drill","crowbar",
                                                 "flagpole","flashlight","garden tool","glass shard","hammer","hand torch","hatchet","knife",
                                                 "lawn mower blade","machete","meat cleaver","metal hand tool","metal object","metal pipe","metal pole",
                                                 "metal rake","metal stick","oar","pick-axe","piece of wood","pipe","pitchfork","pole","pole and knife",
                                                 "rock","scissors","screwdriver","sharp object","shovel","spear","stapler","straight edge razor","sword",
                                                 "Taser","tire iron","toy weapon"),"melee weapon",
                          ifelse(shootings$armed %in% c("bean-bag gun","crossbow","gun","gun and knife","guns and explosives","hatchet and gun",
                                                        "machete and gun","nail gun"),"ranged weapon",
                                 ifelse(shootings$armed %in% c("motorcycle","vehicle"),"vehicle",
                                        ifelse(shootings$armed %in% c("undetermined","unknown weapon",""),"undetermined","unarmed"))))

#mapping race from letter to race
shootings$race <- mapvalues(shootings$race, from = c("A", "B", "H", "N", "O", "W"), to = c("Asian", "Black", "Hispanic", "Native American", "Other", "White"))


######Testing Age
#boxplots
boxplot(age~body_camera,data=shootings, main="Distribution of Age", 
        xlab="Body Camera", ylab="Age")
#anova
a1 <- aov(shootings$age ~ shootings$body_camera)
summary(a1)

######Testing Race
race <- table(shootings$race, shootings$body_camera)
prop.table(race, 2)
chisq.test(race)

######threat level
threat <- table(shootings$threat, shootings$body_camera)
prop.table(threat, 2)
chisq.test(threat)
chisq.test(threat)$expected

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
shootings$armed <- factor(shootings$armed)
armed <- table(shootings$armed, shootings$body_camera)
prop.table(armed, 2)
chisq.test(armed)

#####Region
region <- table(shootings$region, shootings$body_camera)
prop.table(region, 2)
chisq.test(region)

######Population
#boxplots
boxplot(as.integer(Population.per.square.mile..2010)~body_camera,data=census, main="Distribution of Population", 
        xlab="Body Camera", ylab="Population per Square Mile")
#anova
a2 <- aov(as.integer(census$Population.per.square.mile..2010) ~ census$body_camera)
summary(a2)

######Poverty
#boxplots
boxplot(as.integer(Persons.in.poverty..percent)~body_camera,data=census, main="Distribution of Poverty", 
        xlab="Body Camera", ylab="Poverty Percent")
#anova
a3 <- aov(as.integer(census$Persons.in.poverty..percent) ~ census$body_camera)
summary(a3)
