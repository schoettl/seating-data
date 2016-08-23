test_that('newState produces the desired list', {
    state = newState()
    expect_that(state, is_a('list'))
    expect_that(length(state), equals(6))

    expect_that(state$seats, is_a('list'))
    expect_that(length(state$seats), equals(3))
    expect_that(length(state$seats$persons), equals(SEAT_COUNT))
    expect_that(length(state$seats$baggage), equals(SEAT_COUNT))
    expect_that(length(state$seats$disturbing), equals(SEAT_COUNT))

    expect_that(state$stopping, equals(FALSE))
    expect_that(state$direction, is_a('factor'))
    expect_that(as.character(state$direction), equals('FORWARD'))
    expect_that(state$standingPersons, equals(NA))
    expect_that(state$lastUpdate, equals(NULL))
    expect_that(state$initialized, equals(FALSE))
})

test_that('setting class for events works', {
    eventType = 'INITIALIZATION_END'
    event = data.frame(EVENT_TYPE = factor(eventType))
    event = setClassForEvent(event)
    expect_that(event, is_a(eventType))
})

test_that('selecting the right method works', {
    eventType = 'INITIALIZATION_END'
    event = data.frame(EVENT_TYPE = factor(eventType), TIME = c('13:13:13'))
    event = setClassForEvent(event)

    state = newState()
    expect_that(state$initialized, is_false())
    state = handleEvent(event, state)
    expect_that(state$initialized, is_true())
})

test_that('the default method gives a warning', {
    event = data.frame(EVENT_TYPE = c('INVALID'))
    event = setClassForEvent(event)

    state = newState()
    expect_that(handleEvent(event, state), gives_warning('not supported'))
})
