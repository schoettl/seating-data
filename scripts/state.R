SEAT_COUNT = 16

newState = function() {
    list(
        seats = list(
                     persons = rep(NULL, SEAT_COUNT),
                     baggage = rep(NULL, SEAT_COUNT),
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
    warning(paste0('event not supported: ', event$EVENT_TYPE))
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
    commonUpdateState(event, state)
}

handleEvent.LEAVE = function(event, state) {
    commonUpdateState(event, state)
}

handleEvent.CHANGE_SEAT = function(event, state) {
    commonUpdateState(event, state)
}

handleEvent.PLACE_BAGGAGE = function(event, state) {
    commonUpdateState(event, state)
}

handleEvent.REMOVE_BAGGAGE = function(event, state) {
    commonUpdateState(event, state)
}

handleEvent.DISTURBING = function(event, state) {
    commonUpdateState(event, state)
}

handleEvent.STOPS_DISTURBING = function(event, state) {
    commonUpdateState(event, state)
}

handleEvent.DOOR_RELEASE = function(event, state) {
    commonUpdateState(event, state)
}

handleEvent.TRAIN_STARTS = function(event, state) {
    commonUpdateState(event, state)
}

handleEvent.DIRECTION_CHANGE = function(event, state) {
    commonUpdateState(event, state)
}

handleEvent.COUNT_STANDING_PERSONS = function(event, state) {
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
