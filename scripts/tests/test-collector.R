
test_that('number of persons is counted correctly', {
    state = newState()
    event = list(SEAT = 1)
    expect_that(getNumberOfPersonsInCompartment(state), equals(0))
    expect_that(getNumberOfPersonsInSeatGroup(state, event), equals(0))

    state$seats$persons[2:5] = 1
    expect_that(getNumberOfPersonsInCompartment(state), equals(4))
    expect_that(getNumberOfPersonsInSeatGroup(state, event), equals(2))
})

test_that('persons on neigbor seats are detected', {
    state = newState()
    state$seats$persons = 1:SEAT_COUNT
    event = list(SEAT = 1)
    expect_that(getPersonOnNextSeat(state, event), equals(2))
    expect_that(getPersonOnVisAVisSeat(state, event), equals(5))
    expect_that(getPersonOnDiagonalVisAVisSeat(state, event), equals(6))

    event = list(SEAT = 16)
    expect_that(getPersonOnNextSeat(state, event), equals(15))
    expect_that(getPersonOnVisAVisSeat(state, event), equals(12))
    expect_that(getPersonOnDiagonalVisAVisSeat(state, event), equals(11))

    state$seats$persons = rep(NA, SEAT_COUNT)
    expect_that(getPersonOnNextSeat(state, event), equals(NA))
    expect_that(getPersonOnVisAVisSeat(state, event), equals(NA))
    expect_that(getPersonOnDiagonalVisAVisSeat(state, event), equals(NA))
})

test_that('isEventOfType works', {
    evType = 'SIT_DOWN'
    event = data.frame(EVENT_TYPE = c(evType))

    expect_that(isEventOfType(event, evType), is_true())

    otherEvType = paste0(' ', evType)
    expect_that(isEventOfType(event, otherEvType), is_false())
})
