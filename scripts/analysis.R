library(ggplot2)
library(plyr)
library(dplyr)
library(xtable)
library(knitr)

getCsvFileName = function(baseName) {
    return(paste0('../data/', baseName, '.csv'))
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




makeTableWithColumns = function(dataframe, columnDescriptions) {
    columnDescriptions = ldply(columnDescriptions)
    colnames(columnDescriptions) = c('field')
    # Suppress conversion warning:
    columnDescriptions = transform(columnDescriptions, field = factor(field))

    columnTable = data.frame(field = colnames(dataframe))
    columnTable = left_join(columnTable, columnDescriptions, by = 'field')
    colnames(columnTable) = c('Field name', 'Description')

    sanitizeText = function(s) {
        # field name is UPPER_CASE and has to be \verb
        if (all(s == toupper(s))) {
            # scheiß R, durch bug kann man keinen backslash vor _ einsetzen
            #s = gsub('_', '\\_', s)
            #s = gsub('_', '\\\\_', s)
            s = paste0('\\verb|', s, '|')
        }
        return(s)
    }
    xtab = xtable(columnTable, align = c('r', 'p{3cm}', 'p{10cm}'))
    print(xtab, type = 'latex', sanitize.text.function = sanitizeText)
}

