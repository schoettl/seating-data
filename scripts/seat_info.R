
checkSeatNumberValid = function(seatNumber) {
    if (any(seatNumber < 1 || seatNumber > SEAT_COUNT))
        error('invalid seat number')
}

getSeatRow = function(seatNumber) {
    checkSeatNumberValid(seatNumber)
    ifelse(seatNumber <=  4, 1,
    ifelse(seatNumber <=  8, 2,
    ifelse(seatNumber <= 12, 3,
    ifelse(seatNumber <= 16, 4))))
}

getSeatColumn = function(seatNumber) {
    checkSeatNumberValid(seatNumber)
    ifelse(((seatNumber %% 4) == 0), 4,
    ifelse(((seatNumber %% 2) == 0), 2,
    ifelse((((seatNumber - 1) %% 4) == 0), 1,
    ifelse((((seatNumber - 1) %% 2) == 0), 3))))
}

# Return a list with keys: windowSeat, aisleSeat,
# forwardFacing, backwardFacing, row, column
getSeatInformation = function(seatNumber, drivingDirection) {
    result = list()

    seatColumn = getSeatColumn(seatNumber)
    seatRow    = getSeatRow(seatNumber)

    result$windowSeat = seatColumn %in% c(1, 4)
    result$aisleSeat  = !result$windowSeat

    backward = (drivingDirection == 'BACKWARD')
    result$forwardFacing = xor(backward,
        seatRow %in% c(2, 4))
    result$backwardFacing = !result$forwardFacing

    result$row    = seatRow
    result$column = seatColumn

    result
}

getSeatGroup = function(seatNumber) {
    checkSeatNumberValid(seatNumber)
    ifelse(seatNumber %in% c(1,2,5,6), 1,
    ifelse(seatNumber %in% c(3,4,7,8), 2,
    ifelse(seatNumber %in% c(9,10,13,14), 3,
    ifelse(seatNumber %in% c(11,12,15,16), 4))))
}
