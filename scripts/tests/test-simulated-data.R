
test_that('creating artificial survey data works', {
    events = readRDS('logEventDataTest1.rds')
    surveys = createArtificialSurveyData(events)

    distinctSurveys = levels(factor(events$SURVEY))
    expect_that(nrow(surveys), equals(length(distinctSurveys)))
})

test_that('creating artificial person data works', {
    events = readRDS('logEventDataTest1.rds')
    persons = createArtificialPersonData(events)

    distinctPersons = levels(factor(events$PERSON))
    expect_that(nrow(persons), equals(length(distinctPersons)))
})
