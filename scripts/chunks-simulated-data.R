source('analysis.R', chdir = TRUE)

## ---- chunk-simulated-data-processing ----

# load simulated LOG_EVENTS.csv
loadSimulatedLogEventData = function() {
    read.csv(getCsvFileNameOfSimulatedData('LOG_EVENT'), sep = ' ')
}
logEventDataSimulated = loadSimulatedLogEventData()

# create artificial surveyData and personData

createArtificialSurveyData = function(logEventData) {
    logEventData %>%
        select(SURVEY) %>%
        rename(ID = SURVEY) %>%
        distinct(ID) %>%
        mutate(
            AGENT = NA,
            DATE = Sys.Date())
}
surveyDataSimulated = createArtificialSurveyData(logEventDataSimulated)

createArtificialPersonData = function(logEventData) {
    logEventData %>%
        select(PERSON) %>%
        rename(ID = PERSON) %>%
        distinct(ID) %>%
        filter(!is.na(ID)) %>%
        mutate(
            AGE_GROUP = NA,
            GENDER = NA,
            M_GROUP = NA)
}
personDataSimulated = createArtificialPersonData(logEventDataSimulated)

test_that('data is valid', {
    expect_that(surveyDataSimulated, has_no_duplicate_ids())
    expect_that(personDataSimulated, has_no_duplicate_ids())
    expect_that(logEventDataSimulated, has_monotonic_increasing_column('ID'))
    # expect_that(logEventDataSimulated, has_monotonic_increasing_column_per_survey('TIME'))
})

# do some preprocessing

logEventDataSimulated = mutate(logEventDataSimulated,
    TIME = as.character(TIME))

seatingDataSimulated = generateMoreData(surveyDataSimulated, personDataSimulated, logEventDataSimulated)

