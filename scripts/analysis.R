library(ggplot2)
library(dplyr)

surveyData   = read.csv('../data/SURVEY.csv')
personData   = read.csv('../data/PERSON.csv')
logEventData = read.csv('../data/LOG_EVENT.csv')

# with(personData, ...) has no effect to the outside
personData$M_GROUP[personData$M_GROUP == 0] = NA
logEventData$EXTRA_STRING[logEventData$EXTRA_STRING == ''] = NA
