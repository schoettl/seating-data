getSeatRow = function(seatNumber) {
    if (seatNumber <= 0)
        return(0)
    else if (seatNumber <= 4)
        return(1)
    else if (seatNumber <= 8)
        return(2)
    else if (seatNumber <= 12)
        return(3)
    else if (seatNumber <= 16)
        return(4)
    else
        return(0)
}

getSeatColumn = function(seatNumber) {
    if (seatNumber <= 0 || seatNumber > 16)
        return(0)
    else if ((seatNumber %% 4) == 0)
        return(4)
    else if ((seatNumber %% 2) == 0)
        return(2)
    else if (((seatNumber - 1) %% 4) == 0)
        return(1)
    else if (((seatNumber - 1) %% 2) == 0)
        return(3)
    else
        return(0)
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

