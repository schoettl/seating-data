
CollectSeatingData = function(logEventData) {
    # closure variables:
    eventData = logEventData
    collectionStarted = FALSE

    # collect data function:
    function(seatingData, stateBefore, state, event) {
        if (collectionStarted && isEventOfType(event, 'SIT_DOWN')) {
            newRow = list(
                person                   = event$PERSON,
                seat                     = event$SEAT,
                numberPersonsCompartment = getNumberOfPersonsInCompartment(stateBefore),
                numberPersonsSeatGroup   = getNumberOfPersonsInSeatGroup(stateBefore, event),
                # wasBaggageSeat           = NULL,
                personNext               = getPersonOnNextSeat(state, event),
                personVisAVis            = getPersonOnVisAVisSeat(state, event),
                personDiagonal           = getPersonOnDiagonalVisAVisSeat(state, event))
            seatingData = rbind(seatingData, newRow)
        } else if (isEventOfType(event, 'INITIALIZATION_END')) {
            collectionStarted <<- TRUE
        }

        seatingData
    }
}

isEventOfType = function(event, type) {
    all(event$EVENT_TYPE == type)
}

returnNAForZeroLength = function(vec) {
    # cannot return NULL: replacement has length zero
    ifelse(length(vec) != 0, vec, NA)
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
    seats = which(!is.na(persons))
    result = persons[getSeatGroup(seats) == getSeatGroup(s) &
            getSeatRow(seats)   == getSeatRow(s) &
            seats != s]
    returnNAForZeroLength(result)
}

getPersonOnVisAVisSeat = function(state, event) {
    s = event$SEAT
    persons = state$seats$persons
    seats = which(!is.na(persons))
    result = persons[getSeatGroup(seats)  == getSeatGroup(s) &
            getSeatColumn(seats) == getSeatColumn(s) &
            seats != s]
    returnNAForZeroLength(result)
}

getPersonOnDiagonalVisAVisSeat = function(state, event) {
    s = event$SEAT
    persons = state$seats$persons
    seats = which(!is.na(persons))
    result = persons[getSeatGroup(seats)  == getSeatGroup(s) &
            getSeatRow(seats)    != getSeatRow(s) &
            getSeatColumn(seats) != getSeatColumn(s)]
    returnNAForZeroLength(result)
}