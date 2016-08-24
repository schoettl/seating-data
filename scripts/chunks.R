source('analysis.R')

makeTableWithColumns = function(dataframe, columnDescriptions, tableNameForCaption) {
    columnDescriptions = ldply(columnDescriptions)
    colnames(columnDescriptions) = c('field')
    # Suppress conversion warning:
    columnDescriptions = mutate(columnDescriptions, field = factor(field))

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

    xtab = xtable(columnTable, align = c('r', 'p{3cm}', 'p{10cm}'),
        caption = paste0('Columns of the ', tableNameForCaption, ' table.'))

    print(xtab, type = 'latex',
          sanitize.text.function = sanitizeText,
          table.placement = '!h')
}

## ---- seating-survey-table ----
tableColumns = c('DATE', 'TIME', 'STARTING_AT', 'LINE', 'DESTINATION')
xtab = xtable(surveyData[, tableColumns],
    # align = c('r', 'p{3cm}', 'p{10cm}'),
    label = 'tab:surveys',
    caption = 'List of conducted surveys.')

print(xtab, type = 'latex',
    table.placement = 'ht')
