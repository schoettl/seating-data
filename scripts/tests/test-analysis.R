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
    surveys = data.frame(ID = c(2, 4))

    seatingData = generateMoreData(surveys, events)

    expect_that(nrow(seatingData) < nrow(filter(events, EVENT_TYPE == 'SIT_DOWN')),
        is_true())
    expect_that(nrow(seatingData),
        equals(getNumberOfSitDownEventsAfterInitEnd(events)))

    numberPersonsSittingThere = forEachRowAsVec(seatingData,
        c('personNext','personVisAVis','personDiagonal'), countAvailableValues)
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
