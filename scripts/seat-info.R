
checkSeatNumberValid = function(seatNumber) {
    if (any(seatNumber < 1 | seatNumber > SEAT_COUNT))
        stop('invalid seat number')
}

getSeatRow = function(seatNumber) {
    checkSeatNumberValid(seatNumber)
    sapply(seatNumber, function(seatNumber) {
        if      (seatNumber <= 4)
            return(1)
        else if (seatNumber <= 8)
            return(2)
        else if (seatNumber <= 12)
            return(3)
        else if (seatNumber <= 16)
            return(4)
    })
}

getSeatColumn = function(seatNumber) {
    checkSeatNumberValid(seatNumber)
    sapply(seatNumber, function(seatNumber) {
        if      ((seatNumber %% 4) == 0)
            return(4)
        else if ((seatNumber %% 2) == 0)
            return(2)
        else if (((seatNumber - 1) %% 4) == 0)
            return(1)
        else if (((seatNumber - 1) %% 2) == 0)
            return(3)
    })
}

# Return a list with keys: windowSeat, aisleSeat,
# forwardFacing, backwardFacing, row, column
getSeatInformation = function(seatNumber, drivingDirection) {
    result = list()

    seatColumn = getSeatColumn(seatNumber)
    seatRow    = getSeatRow(seatNumber)

    result$windowSeat = seatColumn %in% c(1, 4)
    result$aisleSeat  = !result$windowSeat
    result$side = ifelse(result$windowSeat, 'WINDOW', 'AISLE')

    backward = (drivingDirection == 'BACKWARD')
    result$forwardFacing = xor(backward,
        seatRow %in% c(2, 4))
    result$backwardFacing = !result$forwardFacing
    result$direction = ifelse(result$forwardFacing, 'FORWARD', 'BACKWARD')

    result$row    = seatRow
    result$column = seatColumn

    result
}

getSeatGroup = function(seatNumber) {
    checkSeatNumberValid(seatNumber)
    sapply(seatNumber, function(seatNumber) {
        if      (seatNumber %in% c(1,2,5,6))
            return(1)
        else if (seatNumber %in% c(3,4,7,8))
            return(2)
        else if (seatNumber %in% c(9,10,13,14))
            return(3)
        else if (seatNumber %in% c(11,12,15,16))
            return(4)
    })
}
