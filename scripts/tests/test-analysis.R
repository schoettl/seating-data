getNumberOfSitDownEventsAfterInitEnd = function(events) {
    initEndEvents = filter(events, EVENT_TYPE == 'INITIALIZATION_END')
    result = 0
    for (i in 1:nrow(initEndEvents)) {
        e = initEndEvents[i, ]
        n = events %>%
            filter(SURVEY == e$SURVEY &
                   ID > e$ID &
                   EVENT_TYPE == 'SIT_DOWN') %>%
            nrow
        result = result + n
    }
    result
}

makeTestData = function(seatSide, seatDirection, relativePosition) {
    pNext   = NA
    pAcross = NA
    pDiag   = NA
    if      (relativePosition == 'next')
        pNext = 1
    else if (relativePosition == 'across')
        pAcross = 1
    else if (relativePosition == 'diagonal')
        pDiag = 1

    data.frame(seatSide = seatSide,
               seatDirection = seatDirection,
               personNext = pNext,
               personAcross = pAcross,
               personDiagonal = pDiag)
}

test_that('removing duplicate init events works', {
    initEndEvType = 'INITIALIZATION_END'
    data = matrix(
        c( 1, 1, initEndEvType, # dupl
           2, 1, 'other',
           3, 1, initEndEvType, # dupl
           4, 1, initEndEvType,
           5, 1, 'other',
           6, 2, initEndEvType, # dupl
           7, 2, initEndEvType,
           8, 3, 'other',
           9, 3, initEndEvType,
          10, 3, 'other',
          11, 4, initEndEvType, # dupl
          12, 4, 'other',
          13, 4, initEndEvType),
        ncol = 3, byrow = TRUE)
    events = data.frame(data, stringsAsFactors = FALSE)
    colnames(events) = c('ID', 'SURVEY', 'EVENT_TYPE')
    events = mutate(events, ID = as.integer(ID), SURVEY = as.integer(SURVEY))

    filtered = filter(events, EVENT_TYPE == initEndEvType)

    result = removeDuplicateInitEndEvents(events, filtered)
    expect_that(sort(result$ID), equals(c(2,4,5,7,8,9,10,12,13)))
})

test_that('processing works', {
    # saveRDS(filter(logEventData, SURVEY %in% c(2, 4)), file='tests/logEventDataTest1.rds')
    events = readRDS('logEventDataTest1.rds')
    surveys = createArtificialSurveyData(events)
    persons = createArtificialPersonData(events)

    seatingData = generateMoreData(surveys, persons, events)

    expect_that(nrow(seatingData) < nrow(filter(events, EVENT_TYPE == 'SIT_DOWN')),
        is_true())
    expect_that(nrow(seatingData),
        equals(getNumberOfSitDownEventsAfterInitEnd(events)))

    numberPersonsSittingThere = forEachRowAsVec(seatingData,
        c('personNext','personAcross','personDiagonal'), countAvailableValues)
    names(numberPersonsSittingThere) = NULL
    expect_that(seatingData$nPersonsSeatGroup,
        equals(numberPersonsSittingThere))
})

test_that('hasNoSpaces works', {
    expect_that(hasNoSpaces(''), is_true())
    expect_that(hasNoSpaces('uiae'), is_true())
    expect_that(hasNoSpaces(' uiae'), is_false())
    expect_that(hasNoSpaces('uiae '), is_false())
    expect_that(hasNoSpaces(' '), is_false())
    expect_that(hasNoSpaces('  '), is_false())
})

test_that('hasNoSpaces works', {
    df = data.frame(personNext     = c(1, NA, NA, NA),
                    personAcross   = c(NA, 1, NA, NA),
                    personDiagonal = c(NA, NA, 1, NA))
    expect_that(getPositionRelative(df),
            equals(c('NEXT', 'ACROSS', 'DIAGONAL', NA)))
})

test_that('name and convert col to factor work', {
    strings = paste0('str', 1:5)
    df = data.frame(col1 = 1:5, col2 = strings)
    df = nameLastColumnAndConvertToFactor(df, 'test')
    expect_that(ncol(df), equals(2))
    expect_that(colnames(df)[2], equals('test'))
    expect_that(df[[ncol(df)]], is_identical_to(factor(strings)))
})

test_that('get the other person works', {
    data = data.frame(personNext     = c(5,6,NA,NA),
                      personAcross   = c(NA,NA,7,NA),
                      personDiagonal = c(NA,NA,NA,8))
    expect_that(getTheOtherPerson(data), equals(5:8))
})

test_that('get the other person side works', {
    x = makeTestData('WINDOW', NA, 'next')
    expect_that(getTheOtherPersonSide(x), equals('AISLE'))
    x = makeTestData('WINDOW', NA, 'diagonal')
    expect_that(getTheOtherPersonSide(x), equals('AISLE'))
    x = makeTestData('WINDOW', NA, 'across')
    expect_that(getTheOtherPersonSide(x), equals('WINDOW'))

    x = makeTestData('AISLE', NA, 'next')
    expect_that(getTheOtherPersonSide(x), equals('WINDOW'))
    x = makeTestData('AISLE', NA, 'diagonal')
    expect_that(getTheOtherPersonSide(x), equals('WINDOW'))
    x = makeTestData('AISLE', NA, 'across')
    expect_that(getTheOtherPersonSide(x), equals('AISLE'))
})

test_that('get the other person direction works', {
    x = makeTestData(NA, 'FORWARD', 'next')
    expect_that(getTheOtherPersonDirection(x), equals('FORWARD'))
    x = makeTestData(NA, 'FORWARD', 'diagonal')
    expect_that(getTheOtherPersonDirection(x), equals('BACKWARD'))
    x = makeTestData(NA, 'FORWARD', 'across')
    expect_that(getTheOtherPersonDirection(x), equals('BACKWARD'))

    x = makeTestData(NA, 'BACKWARD', 'next')
    expect_that(getTheOtherPersonDirection(x), equals('BACKWARD'))
    x = makeTestData(NA, 'BACKWARD', 'diagonal')
    expect_that(getTheOtherPersonDirection(x), equals('FORWARD'))
    x = makeTestData(NA, 'BACKWARD', 'across')
    expect_that(getTheOtherPersonDirection(x), equals('FORWARD'))
})

test_that('simple binomial significance test works', {

    vec = rep('foo', 100)
    expect_that(simpleBinomTest(vec, 'foo')$p.value, equals(0))

    vec = c(rep('foo', 100), rep('bar', 100))
    expect_that(simpleBinomTest(vec, 'foo')$p.value, equals(1))
    expect_that(simpleBinomTest(vec, 'bar')$p.value, equals(1))

    vec = c(rep('foo', 100), 'bar', rep(NA, 100))
    expect_true(simpleBinomTest(vec, 'foo')$p.value < 0.01)
    expect_true(simpleBinomTest(vec, 'bar')$p.value < 0.01)

    expect_that(simpleBinomTest(vec, NA)$p.value, equals(0))
})
