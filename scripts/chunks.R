source('analysis.R', chdir = TRUE)

## ---- seating-load-data ----

surveyData   = readCsvFile('SURVEY')
personData   = readCsvFile('PERSON')
logEventData = readCsvFile('LOG_EVENT')

## ---- seating-keep-raw-data ----

surveyRawData   = surveyData
personRawData   = personData
logEventRawData = logEventData

## ---- seating-preprocessing ----

# Remove duplicate INITIALIZATION_END events
logEventData = removeDuplicateInitEndEvents(logEventData)

# Fix column types and NA values

# - Change missing foreign key values to NA
# - Change missing text to NA
# - Fix date/time columns

surveyData = mutate(surveyData,
    DATE = as.character(DATE), # from factor
    AGENT = ifelse(AGENT == 0, NA, AGENT))

personData = mutate(personData,
    M_GROUP = ifelse(M_GROUP == 0, NA, M_GROUP))

logEventData = mutate(logEventData,
    PERSON = ifelse(PERSON == 0, NA, PERSON),
    EXTRA_STRING = ifelse(EXTRA_STRING == '', NA, as.character(EXTRA_STRING)),
    TIME = as.character(TIME), # from factor
    LTIME = hms(TIME))

# Add TIME column to surveyData
surveyData = addTimeColumn(surveyData, logEventData)

surveyData = mutate(surveyData,
    LDATE = ymd(DATE),
    LTIME = hms(TIME),
    LDATETIME = ymd_hms(paste(DATE, TIME)))

# Fix order of log events
# (Above operations cannot guarantee to keep the order)
logEventData = arrange(logEventData, ID, TIME)

## ---- seating-generate-seating-data ----

seatingData = generateMoreData(surveyData, logEventData)


## ---- seating-survey-table ----

tableColumns = c('DATE', 'TIME', 'STARTING_AT', 'LINE', 'DESTINATION')
xtab = xtable(surveyData[, tableColumns],
    # align = c('r', 'p{3cm}', 'p{10cm}'),
    label = 'tab:surveys',
    caption = 'List of conducted surveys.')

print(xtab, type = 'latex',
    table.placement = 'ht')

## ---- seating-group-related-data ----

groupRelatedSeatingData = seatingData %>%
    inner_join(personData, by = c('person' = 'ID')) %>%
    filter(!is.na(M_GROUP))
