library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(xtable)
library(knitr)

source('seat_info.R')
source('state.R')

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

# - Change foreign key NULL 0 values to NA
# - Fix date/time columns

surveyData = mutate(surveyData,
    DATE = as.character(DATE),
    AGENT = ifelse(AGENT == 0, NA, AGENT))

personData = mutate(personData,
    M_GROUP = ifelse(M_GROUP == 0, NA, M_GROUP))

logEventData = mutate(logEventData,
    PERSON = ifelse(PERSON == 0, NA, PERSON),
    EXTRA_STRING = ifelse(EXTRA_STRING == '', NA, as.character(EXTRA_STRING)),
    TIME = hms(as.character(TIME)))

surveyData = mutate(surveyData,
    DATE = ymd(DATE),
    TIME = hms(TIME),
    DATETIME = ymd_hms(paste(DATE, TIME)))

## Generate more useful data by using the log events

state = newState()
for (i in 1:nrow(logEventData)) {
    event = logEventRawData[i, ]
#   if (isSitDownEvent(event)) {
#       writeNewData
#   }
    state = updateState(state, event)
}
