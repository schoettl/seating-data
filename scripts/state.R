SEAT_COUNT = 16

newState = function() {
    list(
        seats = list(
                     # for the vectors here, I have to use NA instead of NULL
                     # (although it stands for not defined, not for N/A)
                     # because rep(NULL, 5) == NULL
                     persons = rep(NA, SEAT_COUNT),
                     baggage = rep(NA, SEAT_COUNT),
                     disturbing = rep(FALSE, SEAT_COUNT)),
        stopping = FALSE, # TRUE between DOOR_RELEASE and TRAIN_STARTS
        direction = factor('FORWARD', levels = c('FORWARD', 'BACKWARD')),
        standingPersons = NA,
        initialized = FALSE,
        lastUpdate = NULL)
}


handleEvent = function(event, state) {
    UseMethod('handleEvent', event)
}

handleEvent.default = function(event, state) {
    warning(paste0('event not supported: ', event$ID, ', ', event$EVENT_TYPE))
    state
}

commonUpdateState = function(event, state) {
    state$lastUpdate = event$TIME
    state
}

handleEvent.INITIALIZATION_END = function(event, state) {
    state$initialized = TRUE
    commonUpdateState(event, state)
}

handleEvent.SIT_DOWN = function(event, state) {
    # actually, events should (and will) have removed the baggage before
    state$seats$baggage[event$SEAT] = NA
    state$seats$persons[event$SEAT] = event$PERSON
    commonUpdateState(event, state)
}

handleEvent.LEAVE = function(event, state) {
    state$seats$persons[event$SEAT] = NA
    commonUpdateState(event, state)
}

handleEvent.CHANGE_SEAT = function(event, state) {
    oldSeat = which(state$seats$persons == event$PERSON)
    state$seats$persons[oldSeat] = NA
    state$seats$persons[event$SEAT] = event$PERSON
    commonUpdateState(event, state)
}

handleEvent.PLACE_BAGGAGE = function(event, state) {
    state$seats$baggage[event$SEAT] = event$PERSON
    commonUpdateState(event, state)
}

handleEvent.REMOVE_BAGGAGE = function(event, state) {
    state$seats$baggage[event$SEAT] = NA
    commonUpdateState(event, state)
}

handleEvent.DISTURBING = function(event, state) {
    state$seats$disturbing[event$SEAT] = TRUE
    commonUpdateState(event, state)
}

handleEvent.STOPS_DISTURBING = function(event, state) {
    state$seats$disturbing[event$SEAT] = FALSE
    commonUpdateState(event, state)
}

handleEvent.DOOR_RELEASE = function(event, state) {
    state$stopping = TRUE
    commonUpdateState(event, state)
}

handleEvent.TRAIN_STARTS = function(event, state) {
    state$stopping = FALSE
    commonUpdateState(event, state)
}

handleEvent.DIRECTION_CHANGE = function(event, state) {
    state$direction = event$EXTRA_STRING
    commonUpdateState(event, state)
}

handleEvent.COUNT_STANDING_PERSONS = function(event, state) {
    state$standingPersons = event$EXTRA_INT
    commonUpdateState(event, state)
}

setClassForEvent = function(event) {
    class(event) = append(class(event), as.character(event$EVENT_TYPE))
    event
}

# event: one single row from the logEventData data frame, e.g. df[1, ]
updateState = function(state, event) {
    event = setClassForEvent(event)
    state = handleEvent(event, state)

    state
}

printState = function(state) {
    seatsToPrint = state$seats$persons
    print(matrix(seatsToPrint, ncol = 4))
}
