## ML Project 1
## Lev Zadvinskiy
## Tyler Worthington
## Colin Cassady

### Data cleaning 
setwd("C:\\Users\\hexel\\Documents\\SYS 6016\\Project 1\\MLProject1")

# Load required packages
suppressPackageStartupMessages({
  library(readr)
  require(RCurl)
  library(lubridate)
  library(dplyr)
})

# Load the data into environment
fps <- read.csv(text=getURL("https://raw.githubusercontent.com/cvcassady/MLProject1/master/Data/fatal-police-shootings-data.csv"),header=T)

# Review the data
summary(fps)

### Data cleaning process:

# id is good

# make name a character vector
fps$name <- as.character(fps$name)

# make dates dates
fps$date <- ymd(fps$date)

# manner_of_death is good, though unnecessary considering dataset does not contain non-lethal shootings

# set NA's in armed to undetermined and bin levels into 5 general categories based on weapon used:
fps$armed <- as.character(fps$armed)
fps$armed <- ifelse(fps$armed %in% c("ax","baseball bat","baseball bat and fireplace poker","baton","bayonet","beer bottle","blunt object",
                                     "box cutter","brick","carjack","chain","chain saw","contractor's level","cordless drill","crowbar",
                                     "flagpole","flashlight","garden tool","glass shard","hammer","hand torch","hatchet","knife",
                                     "lawn mower blade","machete","meat cleaver","metal hand tool","metal object","metal pipe","metal pole",
                                     "metal rake","metal stick","oar","pick-axe","piece of wood","pipe","pitchfork","pole","pole and knife",
                                     "rock","scissors","screwdriver","sharp object","shovel","spear","stapler","straight edge razor","sword",
                                     "Taser","tire iron","toy weapon"),"melee weapon", 
                    ifelse(fps$armed %in% c("bean-bag gun","crossbow","gun","gun and knife","guns and explosives","hatchet and gun",
                                            "machete and gun","nail gun"),"ranged weapon", 
                           ifelse(fps$armed %in% c("motorcycle","vehicle"),"vehicle", 
                                  ifelse(fps$armed %in% c("undetermined","unknown weapon",""),"undetermined","unarmed"))))
fps$armed <- factor(fps$armed)

# age has 48 NA's, median imputation is possible
fps$age[is.na(fps$age)] <- median(fps$age, na.rm=T)

# no gender, name or race info for id 2346, remove observation from data frame
fps <- fps[!(fps$gender==""),]
# re-factor gender variable to show 2 levels only
fps$gender <- factor(as.character(fps$gender))

# 127 missing obs in race
levels(fps$race)
fps$name[fps$race==""]
# can't really impute the race if the race is unknown, remove these observations
fps <- fps[!(fps$race==""),]
fps$race <- factor(as.character(fps$race))

# make city a character vector
fps$city <- as.character(fps$city)

# make state a character vector
fps$state <- as.character(fps$state)

# signs_of_mental_illness is good, reordering factors
fps$signs_of_mental_illness <- factor(as.character(fps$signs_of_mental_illness), levels=c("True", "False"))

# thread_level is good 
# levels(fps$threat_level)
# can probably combine other and undetermined together
# fps$threat_level <- as.character(fps$threat_level)
# fps$threat_level <- factor(ifelse(fps$threat_level=="other" | fps$threat_level=="undetermined", "other", fps$threat_level))

# set missing flee observations to other
fps$flee <- as.character(fps$flee)
fps$flee <- factor(ifelse(fps$flee=="", "Other", fps$flee))

# body_camera is good, reordering factors
fps$body_camera <- factor(as.character(fps$body_camera), levels=c("True", "False"))

# subset the data frame to 2015 and 2016 police shootings only
fps <- fps[fps$date<="2016-12-31",]

### Save the resulting data frame
saveRDS(fps, "fps.rds")
