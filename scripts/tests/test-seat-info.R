
test.getSeatRow = function() {
    checkEquals(1, getSeatRow(1))
    checkEquals(1, getSeatRow(4))

    checkEquals(2, getSeatRow(5))
    checkEquals(2, getSeatRow(8))

    checkEquals(3, getSeatRow(9))
    checkEquals(3, getSeatRow(12))

    checkEquals(4, getSeatRow(13))
    checkEquals(4, getSeatRow(16))

    checkEquals(1, getSeatRow(2))
    checkEquals(4, getSeatRow(15))
}

test.getSeatColumn = function() {
    checkEquals(1, getSeatColumn(1))
    checkEquals(1, getSeatColumn(5))
    checkEquals(1, getSeatColumn(9))
    checkEquals(1, getSeatColumn(13))

    checkEquals(2, getSeatColumn(2))
    checkEquals(2, getSeatColumn(14))

    checkEquals(3, getSeatColumn(3))
    checkEquals(3, getSeatColumn(7))
    checkEquals(3, getSeatColumn(11))
    checkEquals(3, getSeatColumn(15))

    checkEquals(4, getSeatColumn(4))
    checkEquals(4, getSeatColumn(16))
}

test.getSeatInformation.BackwardForward = function() {
    forwardFacingSeats  = c(5:8, 13:16)
    backwardFacingSeats = c(1:4, 9:12)

    for (i in backwardFacingSeats)
        checkForwardFacing(FALSE, i, TRUE)

    for (i in forwardFacingSeats)
        checkForwardFacing(TRUE, i, TRUE)

    for (i in backwardFacingSeats)
        checkForwardFacing(TRUE, i, FALSE)

    for (i in forwardFacingSeats)
        checkForwardFacing(FALSE, i, FALSE)
}

checkForwardFacing = function(expected, seatNumber, forward) {
    direction = getDirectionString(forward)
    result = getSeatInformation(seatNumber, direction)
    checkEquals(expected,  result$forwardFacing)
    checkEquals(expected, !result$backwardFacing)

    seatDirection = getDirectionString(result$forwardFacing)
    checkEquals(seatDirection, result$direction)
}

test.getSeatInformation.WindowAisle = function() {
    windowSeats = c(1,5,9,13,4,8,12,16)
    aisleSeats = setdiff(1:16, windowSeats)

    for (i in windowSeats)
        checkWindow(TRUE, i)

    for (i in aisleSeats)
        checkWindow(FALSE, i)
}

checkWindow = function(expected, seatNumber) {
    result = getSeatInformation(seatNumber, 'FORWARD')
    checkEquals(expected,  result$windowSeat)
    checkEquals(expected, !result$aisleSeat)
    side = getSideString(result$windowSeat)
    checkEquals(side, result$side)

    result = getSeatInformation(seatNumber, 'BACKWARD')
    checkEquals(expected,  result$windowSeat)
    checkEquals(expected, !result$aisleSeat)
    side = getSideString(result$windowSeat)
    checkEquals(side, result$side)
}

test_that('getting the seat group number works', {
    expect_that(getSeatGroup(0), throws_error())

    expect_that(getSeatGroup(1), equals(1))
    expect_that(getSeatGroup(2), equals(1))
    expect_that(getSeatGroup(6), equals(1))

    expect_that(getSeatGroup(3), equals(2))

    expect_that(getSeatGroup(9), equals(3))

    expect_that(getSeatGroup(16), equals(4))
})
