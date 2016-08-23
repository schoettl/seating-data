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
    events = transform(events, ID = as.integer(ID), SURVEY = as.integer(SURVEY))

    filtered = subset(events, events$EVENT_TYPE == initEndEvType)

    result = removeDuplicateInitEndEvents(events, filtered)
    expect_that(sort(result$ID), equals(c(2,4,5,7,8,9,10,12,13)))
})
