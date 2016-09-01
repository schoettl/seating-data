SEAT_NUMBERS = 1:16

# Create a function with closure. The returned function
# collects data for further analysis.
#
# The variable newRow must be single-row data frame, not a list!
# Otherwise, rbind makes problems with character-factor conversion:
# The first string is converted to 1-level factor,
# the next different string has a "invalid factor level".
createCollectDataFunction = function(logEventData) {
    # closure variables:
    allEvents = logEventData
    collectionStarted = FALSE

    # collect data function:
    function(data, stateBefore, state, event) {
        isEventType = function(x) isEventOfType(event, x)

        if (collectionStarted && isEventType('SIT_DOWN')) {
            newRow = data.frame( # single-row data frame
                survey              = event$SURVEY,
                person              = event$PERSON,
                seat                = event$SEAT,
                nPersonsCompartment = getNumberOfPersonsInCompartment(stateBefore),
                nPersonsSeatGroup   = getNumberOfPersonsInSeatGroup(stateBefore, event),
                seatSide            = getSeatSide(event),
                seatDirection       = getSeatFacingDirection(stateBefore, event),
                direction           = getCurrentDrivingDirection(stateBefore),
                # wasBaggageSeat      = NULL,
                personNext          = getPersonOnNextSeat(state, event),
                personAcross        = getPersonOnSeatAcross(state, event),
                personDiagonal      = getPersonOnSeatDiagonallyAcross(state, event)
            )

            data = rbind(data, newRow)

        } else if (isEventType('INITIALIZATION_END')) {
            collectionStarted <<- TRUE
        }

        data
    }
}

isEventOfType = function(event, type) {
    all(event$EVENT_TYPE == type)
}

# Return first non-NA value or NA if there is no such value.
getFirstValueOrNA = function(vec) {
    # cannot return NULL: replacement has length zero
    indexes = which(!is.na(vec))
    ifelse(length(indexes) != 0, vec[indexes[1]], NA)
}

getNumberOfPersonsInCompartment = function(stateBefore) {
    length(which(!is.na(stateBefore$seats$persons)))
}

getNumberOfPersonsInSeatGroup = function(stateBefore, event) {
    seatGroup = getSeatGroup(event$SEAT)
    otherPersonsSeats = which(!is.na(stateBefore$seats$persons))
    seatGroups = sapply(otherPersonsSeats, getSeatGroup)
    length(which(seatGroups == seatGroup))
}

getPersonOnNextSeat = function(state, event) {
    s = event$SEAT
    persons = state$seats$persons
    result = persons[getSeatGroup(SEAT_NUMBERS) == getSeatGroup(s) &
                     getSeatRow(SEAT_NUMBERS)   == getSeatRow(s)   &
                     SEAT_NUMBERS != s]
    getFirstValueOrNA(result)
}

getPersonOnSeatAcross = function(state, event) {
    s = event$SEAT
    persons = state$seats$persons
    result = persons[getSeatGroup(SEAT_NUMBERS)  == getSeatGroup(s)  &
                     getSeatColumn(SEAT_NUMBERS) == getSeatColumn(s) &
                     SEAT_NUMBERS != s]
    getFirstValueOrNA(result)
}

getPersonOnSeatDiagonallyAcross = function(state, event) {
    s = event$SEAT
    persons = state$seats$persons
    result = persons[getSeatGroup(SEAT_NUMBERS)  == getSeatGroup(s) &
                     getSeatRow(SEAT_NUMBERS)    != getSeatRow(s)   &
                     getSeatColumn(SEAT_NUMBERS) != getSeatColumn(s)]
    getFirstValueOrNA(result)
}

getSeatSide = function(event) {
    seatInfo = getSeatInformation(event$SEAT, NA)
    ifelse(seatInfo$windowSeat, 'WINDOW', 'AISLE')
}

getSeatFacingDirection = function(state, event) {
    seatInfo = getSeatInformation(event$SEAT, state$direction)
    ifelse(seatInfo$forwardFacing, 'FORWARD', 'BACKWARD')
}

getCurrentDrivingDirection = function(state) {
    state$direction
}
