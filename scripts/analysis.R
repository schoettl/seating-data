## ---- init ----
library(ggplot2)
library(dplyr)
library(xtable)
library(knitr)

dataPath = '../../../seating-data/data'

getCsvFileName = function(baseName) {
    return(paste0(dataPath, '/', baseName, '.csv'))
}

readCsvFile = function(baseName) {
    return(read.csv(getCsvFileName(baseName)))
}

surveyData   = readCsvFile('SURVEY')
personData   = readCsvFile('PERSON')
logEventData = readCsvFile('LOG_EVENT')

# with(personData, ...) has no effect to the outside
personData$M_GROUP[personData$M_GROUP == 0] = NA
logEventData$EXTRA_STRING[logEventData$EXTRA_STRING == ''] = NA




makeTableWithColumns = function(dataframe) {
    columnTable = data.frame(colnames(dataframe))
    colnames(columnTable) = c('field name')
    xtable(columnTable)
}

## ---- survey-table ----
makeTableWithColumns(surveyData)

## ---- person-table ----
makeTableWithColumns(personData)

## ---- logevent-table ----
makeTableWithColumns(logEventData)
