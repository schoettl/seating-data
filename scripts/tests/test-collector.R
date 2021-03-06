checkSeatGroup1To4 = function(state, expectedCounts) {
    for (i in 1:4)
        expect_that(getNumberOfPersonsInSeatGroupX(state, i), equals(expectedCounts[i]))
}

test_that('number of persons is counted correctly', {
    state = newState()
    event = list(SEAT = 1)
    expect_that(getNumberOfPersonsInCompartment(state), equals(0))
    expect_that(getNumberOfPersonsInSeatGroup(state, event), equals(0))

    state$seats$persons[2:5] = 1
    expect_that(getNumberOfPersonsInCompartment(state), equals(4))
    expect_that(getNumberOfPersonsInSeatGroup(state, event), equals(2))
})

test_that('getting number of persons in seat group works', {
    state = newState()

    expectedCounts = rep(0, 4)
    checkSeatGroup1To4(state, expectedCounts)

    state$seats$persons[1:2] = 1:2
    expectedCounts = c(2,0,0,0)
    checkSeatGroup1To4(state, expectedCounts)

    state$seats$persons[7] = 7
    expectedCounts = c(2,1,0,0)
    checkSeatGroup1To4(state, expectedCounts)

    state$seats$persons[9:10] = 9:10
    state$seats$persons[13] = 13
    expectedCounts = c(2,1,3,0)
    checkSeatGroup1To4(state, expectedCounts)

    state$seats$persons = 1:16
    expectedCounts = rep(4, 4)
    checkSeatGroup1To4(state, expectedCounts)

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
    expect_that(getPersonOnSeatAcross(state, event), equals(5))
    expect_that(getPersonOnSeatDiagonallyAcross(state, event), equals(6))

    event = list(SEAT = 7)
    expect_that(getPersonOnNextSeat(state, event), equals(8))
    expect_that(getPersonOnSeatAcross(state, event), equals(3))
    expect_that(getPersonOnSeatDiagonallyAcross(state, event), equals(4))

    event = list(SEAT = 16)
    expect_that(getPersonOnNextSeat(state, event), equals(15))
    expect_that(getPersonOnSeatAcross(state, event), equals(12))
    expect_that(getPersonOnSeatDiagonallyAcross(state, event), equals(11))

    state$seats$persons = NA
    expect_that(getPersonOnNextSeat(state, event), equals(NA))
    expect_that(getPersonOnSeatAcross(state, event), equals(NA))
    expect_that(getPersonOnSeatDiagonallyAcross(state, event), equals(NA))

    state$seats$persons = NA
    state$seats$persons[4] = 1
    state$seats$persons[15] = 4
    event = list(SEAT = 7)
    expect_that(getPersonOnNextSeat(state, event), equals(NA))
    expect_that(getPersonOnSeatAcross(state, event), equals(NA))
    expect_that(getPersonOnSeatDiagonallyAcross(state, event), equals(1))

    state$seats$persons = NA
    state$seats$persons[4] = 1
    state$seats$persons[15] = 4
    event = list(SEAT = 8)
    expect_that(getPersonOnNextSeat(state, event), equals(NA))
    expect_that(getPersonOnSeatAcross(state, event), equals(1))
    expect_that(getPersonOnSeatDiagonallyAcross(state, event), equals(NA))

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

test_that('getting group from person works', {
    event = data.frame(PERSON = 11)

    personData = data.frame(ID = 11:15, M_GROUP = 1:5)
    expect_that(getGroupOfPerson(event, personData), equals(1))

    personData = data.frame(ID = 11:15, M_GROUP = c(NA, 2:5))
    expect_that(is.na(getGroupOfPerson(event, personData)), is_true())
})
