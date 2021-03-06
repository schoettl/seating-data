# This file must be sourced with chdir = TRUE!

library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(xtable)
library(knitr)
library(stringr)
library(testthat)

source('seat-info.R')
source('state.R')
source('collector.R')
source('validity.R')

# save working directory for later
# (needed when used with knitr)
workingDirectory = getwd()

getDirectionString = function(forward) ifelse(forward, 'FORWARD', 'BACKWARD')

getSideString = function(window) ifelse(window, 'WINDOW', 'AISLE')

getCsvFileName = function(baseName) {
    paste0(workingDirectory, '/../data/', baseName, '.csv')
}

getCsvFileNameOfSimulatedData = function(baseName) {
    paste0(workingDirectory, '/../data/simulated/', baseName, '.csv')
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

    makeColumnDescriptionTable(dataframe, columnDescriptions, tableCaption,
                               tableLabel, sanitizeTextFunction, 3)
}

makeSeatingDataColumnDescriptionTable = function(dataframe, columnDescriptions, tableCaption, tableLabel) {
    sanitizeTextFunction = function(s) {
        # Only the field name has 1 single word
        if (hasNoSpaces(s)) {
            s = paste0('\\verb|', s, '|')
        }
        s
    }
    makeColumnDescriptionTable(dataframe, columnDescriptions, tableCaption,
                               tableLabel, sanitizeTextFunction, 4)
}

makeColumnDescriptionTable = function(dataframe, columnDescriptions, tableCaption, tableLabel, sanitizeTextFunction, fieldColumnWidthCm) {
    columnDescriptions = ldply(columnDescriptions)
    colnames(columnDescriptions) = c('field', 'description')
    # To suppress conversion warning later:
    columnDescriptions = mutate(columnDescriptions, field = factor(field))

    columnTable = data.frame(field = colnames(dataframe))
    columnTable = left_join(columnTable, columnDescriptions, by = 'field')
    colnames(columnTable) = c('Field name', 'Description')

    xtab = xtable(columnTable, align = getXtableAlignment(fieldColumnWidthCm),
        caption = tableCaption, label = tableLabel)

    print(xtab, type = 'latex',
          sanitize.text.function = sanitizeTextFunction,
          table.placement = '!h')
}

getXtableAlignment = function(col2WidthCm) {
    col3WidthCm = 13 - col2WidthCm
    col2Align = paste0('p{', col2WidthCm, 'cm}')
    col3Align = paste0('p{', col3WidthCm, 'cm}')
    c('r', col2Align, col3Align)
}

hasNoSpaces = function(s) {
    # countWords overstrains R, I couldn't find a solution on the internet
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
generateMoreData = function(surveyData, personData, logEventData) {
    data = data.frame()

    for (i in 1:nrow(surveyData)) {
        logEvents = filter(logEventData, SURVEY == surveyData$ID[i])

        # create collector function and pass data value for its "static" variable
        collectData = createCollectDataFunction(logEvents, personData)

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

# To be used only for filtered events where a person sits down to a seat group
# at which only one other person is sitting.
getPositionRelative = function(data) {
    ifelse(!is.na(data$personNext),     'NEXT',
    ifelse(!is.na(data$personAcross),   'ACROSS',
    ifelse(!is.na(data$personDiagonal), 'DIAGONAL', NA)))
}

# To be used only for filtered events where a person sits down to a seat group
# at which only one other person is sitting.
getTheOtherPerson = function(data) {
    persons = data.matrix(data[c('personNext', 'personAcross', 'personDiagonal')])
    apply(persons, 1, function(x) {
        getFirstValueOrNA(x)
    })
}

getTheOtherPersonSide = function(x) {
    if (x$seatSide == 'WINDOW')
        window = !is.na(x$personAcross)
    else
        window = is.na(x$personAcross)
    getSideString(window)
}

getTheOtherPersonDirection = function(x) {
    # getSeatInformation(x$theOtherPerson, x$direction)$direction)
    if (x$seatDirection == 'FORWARD')
        direction = !is.na(x$personNext)
    else
        direction = is.na(x$personNext)
    getDirectionString(direction)
}

mapAndAddNewColumn = function(dataframe, func) {
    adply(dataframe, 1, func)
}

nameLastColumnAndConvertToFactor = function(data, newColumnName) {
    colnames(data)[ncol(data)] = newColumnName
    data[[newColumnName]] = factor(data[[newColumnName]])
    data
}

filterDataNoGroupAndNPersonsSeatGroup = function(seatingData, nOtherPersons) {
    filter(seatingData, nPersonsSeatGroup == nOtherPersons & is.na(group))
}

# Simple exact binomial test function.
# NA values are automatically removed from the data column.
simpleBinomTest = function(dataColumn, successValue) {
    dataColumn = subset(dataColumn, !is.na(dataColumn))
    numberOfSuccess = count(dataColumn == successValue)
    totalNumber = length(dataColumn)
    binom.test(numberOfSuccess, totalNumber, alternative = 'two.sided')
}

simpleBinomTestResults = function(dataColumn, successValue) {
    result = simpleBinomTest(dataColumn, successValue)
    pValue = formatPValue(result$p.value)
    paste0("exact binomial test, $p$-value $",
           pValue, "$, $n = ", length(dataColumn), "$")
}

binomTest = simpleBinomTestResults # simpler alias for use in thesis

formatPValue = function(pValue) {
    ifelse(pValue < 0.001, "< 0.001", paste0("=", round(pValue, 3)))
}

makeBarsWithRelativeFrequency = function(ggp, xAxisTitle) {
    ggp +
        geom_bar(aes(y = ..count.. / sum(..count..)), width = 0.1) +
        scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
        ylab('relative frequency') +
        xlab(xAxisTitle) +
        geom_text(stat = 'count', aes(label = ..count.., y = ..count.. / sum(..count..)), vjust = -0.5, size = 8)
}

count = function(boolVector) length(which(boolVector))
