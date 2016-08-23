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

# Remove duplicate INITIALIZATION_END events

getInitEndEvents = function(logEventData) {
    filter(logEventData, EVENT_TYPE == 'INITIALIZATION_END')
}

removeDuplicateInitEndEvents = function(logEventData, initEndEvents) {
    initEndEvents = getInitEndEvents(logEventData)

    lastEvents = initEndEvents %>%
        group_by(SURVEY) %>%
        summarize(ID = max(ID)) %>%
        select(ID)

    duplicateInitEndEvents = subset(initEndEvents, !(initEndEvents$ID %in% lastEvents$ID))
    logEventData %>% anti_join(duplicateInitEndEvents, by = 'ID')
}

logEventData = removeDuplicateInitEndEvents(logEventData)

# Add TIME column to surveyData

logEventData$TIME = as.character(logEventData$TIME) # from factor

initEndEvents = getInitEndEvents(logEventData)
surveyData = surveyData %>% left_join(initEndEvents, by = c('ID' = 'SURVEY'))


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


# Fix order of log events (it might have changed during joins or other dplyr functions)

# IDs must be (weak) monotonic increasing within a survey
logEventData = arrange(logEventData, ID, TIME)

## Generate more useful data by using the log events

seatingData = data.frame()

collectSeatingData = function(stateBefore, state, event) {
}

for (i in 1:nrow(surveyData)) {
    logEvents = logEventData[logEventData$SURVEY == i, ]
    state = newState()
    for (j in 1:nrow(logEvents)) {
        event = logEventRawData[j, ]
        stateBefore = state
        state = updateState(state, event)
        collectSeatingData(stateBefore, state, event)
        # printState(state)
    }
}
