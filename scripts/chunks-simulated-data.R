source('analysis.R', chdir = TRUE)

## ---- chunk-simulated-data-processing ----

# load simulated LOG_EVENTS.csv
logEventData = read.csv(getCsvFileNameOfSimulatedData('LOG_EVENT'), sep = ' ')

# create artificial surveyData and personData
surveyData = logEventData %>%
    select(SURVEY) %>%
    rename(ID = SURVEY) %>%
    distinct(ID) %>%
    mutate(
        AGENT = NA,
        DATE = Sys.Date())

personData = logEventData %>%
    select(PERSON) %>%
    rename(ID = PERSON) %>%
    distinct(ID) %>%
    mutate(
        AGE_GROUP = NA,
        GENDER = NA,
        M_GROUP = NA)

test_that('data is valid', {
    expect_that(surveyData, has_no_duplicate_ids())
    expect_that(personData, has_no_duplicate_ids())
    expect_that(logEventData, has_monotonic_increasing_column('ID'))
    # expect_that(logEventData, has_monotonic_increasing_column_per_survey('TIME'))
})

# do some preprocessing

logEventData = mutate(logEventData,
    TIME = as.character(TIME))

seatingData = generateMoreData(surveyData, logEventData)

