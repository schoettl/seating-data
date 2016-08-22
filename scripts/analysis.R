library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(xtable)
library(knitr)

source('seat_info.R')

getCsvFileName = function(baseName) {
    paste0('../data/', baseName, '.csv')
}

readCsvFile = function(baseName) {
    read.csv(getCsvFileName(baseName))
}

surveyRawData   = readCsvFile('SURVEY')
personRawData   = readCsvFile('PERSON')
logEventRawData = readCsvFile('LOG_EVENT')

surveyData   = surveyRawData
personData   = personRawData
logEventData = logEventRawData

## Preprocess data

# Add TIME column to surveyData

logEventData$TIME = as.character(logEventData$TIME)

surveyStartTimes = logEventData %>%
    filter(EVENT_TYPE == 'INITIALIZATION_END') %>%
    group_by(SURVEY) %>%
    summarize(TIME = max(TIME)) %>%
    select(SURVEY, TIME)

surveyData = surveyData %>% left_join(surveyStartTimes, by = c('ID' = 'SURVEY'))


# Fix column types and NA values

# with(personData, ...) has no effect to the outside
# does not work:
# personData = transform(personData, M_GROUP[M_GROUP == 0] = NA)
# Change foreign key NULL 0 values to NA
surveyData$AGENT[surveyData$AGENT == 0] = NA
personData$M_GROUP[personData$M_GROUP == 0] = NA
logEventData$PERSON[logEventData$PERSON == 0] = NA
# Change empty extra string to NA
logEventData$EXTRA_STRING[logEventData$EXTRA_STRING == ''] = NA
# Change some column types
logEventData = mutate(logEventData,
    TIME = hms(as.character(TIME)),
    EXTRA_STRING = as.character(EXTRA_STRING))

surveyData = mutate(surveyData,
    DATE = as.character(DATE),
    DATETIME = paste(DATE, TIME))

surveyData = mutate(surveyData,
    DATE = ymd(DATE),
    TIME = hms(TIME),
    DATETIME = ymd_hms(DATETIME))
