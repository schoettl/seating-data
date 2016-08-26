# This file must be sourced with chdir = TRUE!

library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(xtable)
library(knitr)
library(stringr)

source('seat-info.R')
source('state.R')
source('collector.R')

# save working directory for later
# (needed when used with knitr)
workingDirectory = getwd()

getCsvFileName = function(baseName) {
    paste0(workingDirectory, '/../data/', baseName, '.csv')
}

readCsvFile = function(baseName) {
    read.csv(getCsvFileName(baseName))
}

makeDataColumnDescriptionTable = function(dataframe, columnDescriptions, tableNameForCaption, tableLabel) {

    sanitizeTextFunction = function(s) {
        # field name is UPPER_CASE and has to be \verb
        if (all(s == toupper(s))) {
            # scheiß R, durch bug kann man keinen backslash vor _ einsetzen
            #s = gsub('_', '\\_', s)
            #s = gsub('_', '\\\\_', s)
            s = paste0('\\verb|', s, '|')
        }
        return(s)
    }

    tableCaption = paste('Columns of the', tableNameForCaption, 'table.')

    makeColumnDescriptionTable(dataframe, columnDescriptions, tableCaption, tableLabel, sanitizeTextFunction)
}

makeSeatingDataColumnDescriptionTable = function(dataframe, columnDescriptions, tableCaption, tableLabel) {
    sanitizeTextFunction = function(s) {
        # Only the field name has 1 single word
        if (hasNoSpaces(s)) {
            s = paste0('\\verb|', s, '|')
        }
        s
    }
    makeColumnDescriptionTable(dataframe, columnDescriptions, tableCaption, tableLabel, sanitizeTextFunction)
}

makeColumnDescriptionTable = function(dataframe, columnDescriptions, tableCaption, tableLabel, sanitizeTextFunction) {
    columnDescriptions = ldply(columnDescriptions)
    colnames(columnDescriptions) = c('field', 'description')
    # To suppress conversion warning later:
    columnDescriptions = mutate(columnDescriptions, field = factor(field))

    columnTable = data.frame(field = colnames(dataframe))
    columnTable = left_join(columnTable, columnDescriptions, by = 'field')
    colnames(columnTable) = c('Field name', 'Description')

    xtab = xtable(columnTable, align = c('r', 'p{3cm}', 'p{10cm}'),
        caption = tableCaption, label = tableLabel)

    print(xtab, type = 'latex',
          sanitize.text.function = sanitizeTextFunction,
          table.placement = '!h')
}

hasNoSpaces = function(s) {
    # countWords overstrains R, it is not possible with base functions
    length(str_split(s, ' ')[[1]]) == 1
}

getInitEndEvents = function(logEventData) {
    filter(logEventData, EVENT_TYPE == 'INITIALIZATION_END')
}

removeDuplicateInitEndEvents = function(logEventData, initEndEvents) {
    initEndEvents = getInitEndEvents(logEventData)

    lastEvents = initEndEvents %>%
        group_by(SURVEY) %>%
        summarize(ID = max(ID)) %>%
        select(ID)

    duplicateInitEndEvents = filter(initEndEvents, !(ID %in% lastEvents$ID))
    logEventData %>% anti_join(duplicateInitEndEvents, by = 'ID')
}

addTimeColumn = function(surveyData, logEventData) {
    initEndEvents = getInitEndEvents(logEventData)
    left_join(surveyData, initEndEvents, by = c('ID' = 'SURVEY'))

}

# logEventData: ordered log events
generateMoreData = function(surveyData, logEventData) {
    data = data.frame()

    for (i in 1:nrow(surveyData)) {
        logEvents = filter(logEventData, SURVEY == surveyData$ID[i])

        # create collector function and pass data value for its "static" variable
        collectData = createCollectDataFunction(logEvents)

        state = newState()
        for (j in 1:nrow(logEvents)) {
            event = logEvents[j, ]
            stateBefore = state
            state = updateState(state, event)
            data = collectData(data, stateBefore, state, event)
            # printState(state)
        }
    }

    data
}
