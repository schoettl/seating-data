
test_that('number of persons is counted correctly', {
    state = newState()
    event = list(SEAT = 1)
    expect_that(getNumberOfPersonsInCompartment(state), equals(0))
    expect_that(getNumberOfPersonsInSeatGroup(state, event), equals(0))

    state$seats$persons[2:5] = 1
    expect_that(getNumberOfPersonsInCompartment(state), equals(4))
    expect_that(getNumberOfPersonsInSeatGroup(state, event), equals(2))
})

test_that('helper function getFirstValueOrNA works', {
    expect_that(getFirstValueOrNA(c(5)), equals(5))
    expect_that(getFirstValueOrNA(c(NA)), equals(NA))
    expect_that(getFirstValueOrNA(c(NA, NA, 5, NA)), equals(5))
    expect_that(getFirstValueOrNA(c(NA, 3,  5, NA)), equals(3))
    expect_that(getFirstValueOrNA(c(NA, NA, NA)), equals(NA))
})

test_that('persons on neigbor seats are detected', {
    state = newState()
    state$seats$persons = 1:SEAT_COUNT
    event = list(SEAT = 1)
    expect_that(getPersonOnNextSeat(state, event), equals(2))
    expect_that(getPersonOnVisAVisSeat(state, event), equals(5))
    expect_that(getPersonOnDiagonalVisAVisSeat(state, event), equals(6))

    event = list(SEAT = 7)
    expect_that(getPersonOnNextSeat(state, event), equals(8))
    expect_that(getPersonOnVisAVisSeat(state, event), equals(3))
    expect_that(getPersonOnDiagonalVisAVisSeat(state, event), equals(4))

    event = list(SEAT = 16)
    expect_that(getPersonOnNextSeat(state, event), equals(15))
    expect_that(getPersonOnVisAVisSeat(state, event), equals(12))
    expect_that(getPersonOnDiagonalVisAVisSeat(state, event), equals(11))

    state$seats$persons = rep(NA, SEAT_COUNT)
    expect_that(getPersonOnNextSeat(state, event), equals(NA))
    expect_that(getPersonOnVisAVisSeat(state, event), equals(NA))
    expect_that(getPersonOnDiagonalVisAVisSeat(state, event), equals(NA))

    state$seats$persons = rep(NA, SEAT_COUNT)
    state$seats$persons[4] = 1
    event = list(SEAT = 7)
    expect_that(getPersonOnNextSeat(state, event), equals(NA))
    expect_that(getPersonOnVisAVisSeat(state, event), equals(NA))
    expect_that(getPersonOnDiagonalVisAVisSeat(state, event), equals(1))

})

test_that('isEventOfType works', {
    evType = 'SIT_DOWN'
    event = data.frame(EVENT_TYPE = c(evType))

    expect_that(isEventOfType(event, evType), is_true())

    otherEvType = paste0(' ', evType)
    expect_that(isEventOfType(event, otherEvType), is_false())
})

test_that('seat information window/aisle works', {
    event = list(SEAT = NA)

    for (i in c(1,4,5,8,9,12,13,16)) {
        event$SEAT = i
        expect_that(getSeatSide(event), equals('WINDOW'))
    }

    for (i in c(2,3,6,7,10,11,14,15)) {
        event$SEAT = i
        expect_that(getSeatSide(event), equals('AISLE'))
    }
})

test_that('seat information forward/backward works', {
    event = list(SEAT = NA)

    oddRowsSeats  = c(1:4,  9:12)
    evenRowsSeats = c(5:8, 13:16)

    state = list(direction = 'FORWARD')
    for (i in oddRowsSeats) {
        event$SEAT = i
        expect_that(getSeatFacingDirection(state, event), equals('BACKWARD'))
    }
    for (i in evenRowsSeats) {
        event$SEAT = i
        expect_that(getSeatFacingDirection(state, event), equals('FORWARD'))
    }

    state = list(direction = 'BACKWARD')
    for (i in oddRowsSeats) {
        event$SEAT = i
        expect_that(getSeatFacingDirection(state, event), equals('FORWARD'))
    }
    for (i in evenRowsSeats) {
        event$SEAT = i
        expect_that(getSeatFacingDirection(state, event), equals('BACKWARD'))
    }

})

test_that('current driving direction forward/backward works', {
    state = list(direction = 'FORWARD')
    expect_that(getCurrentDrivingDirection(state), equals('FORWARD'))
    state = list(direction = 'BACKWARD')
    expect_that(getCurrentDrivingDirection(state), equals('BACKWARD'))
})
