# Forest Fires in Portugal 2015

#load the data set
fr <- read.csv("fires2015_train.csv", header = TRUE, na.strings = "?", sep = ",")

#search in data
head(fr)
summary(fr)

fr["cause_type"]
fr["origin"]
#clean de special chars

#attri. firstInterv_date, first_interval_hour
