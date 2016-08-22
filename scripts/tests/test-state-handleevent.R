# tests if a list is invariant except given fields
is_invariant_except = function(originalList, notInvariantFields) {
    function(x) {
        for (field in notInvariantFields) {
            x[field] = originalList[field]
        }
        expect_equal(x, originalList)
    }
}


TEST_SEAT = 5
TEST_PERSON = 42
TEST_DIRECTION = 'BACKWARD'
TEST_EXTRA_INT = 7

createTestEvent = function(eventType) {
    event = data.frame(EVENT_TYPE = eventType,
                       TIME = '15:15:15',
                       SEAT = TEST_SEAT,
                       PERSON = TEST_PERSON,
                       EXTRA_STRING = TEST_DIRECTION,
                       EXTRA_INT = TEST_EXTRA_INT)
    event = setClassForEvent(event)
    event
}

test_that('INITIALIZATION_END state update works', {
    state = newState()
    event = createTestEvent('INITIALIZATION_END')
    updatedState = handleEvent(event, state)
    expect_that(updatedState, is_invariant_except(state,
        c('lastUpdate', 'initialized')))
    expect_that(updatedState$initialized, is_true())
})

test_that('SIT_DOWN state update works', {
    state = newState()
    state$seats$baggage[TEST_SEAT] = 100

    event = createTestEvent('SIT_DOWN')
    updatedState = handleEvent(event, state)
    expect_that(updatedState, is_invariant_except(state,
        c('lastUpdate', 'seats')))
    expect_that(updatedState$seats, is_invariant_except(state$seats,
        c('persons', 'baggage')))
    expect_that(updatedState$seats$persons[TEST_SEAT], equals(TEST_PERSON))
    expect_that(is.na(updatedState$seats$baggage[TEST_SEAT]), is_true())
})

test_that('LEAVE state update works', {
    state = newState()
    state$seats$persons[TEST_SEAT] = TEST_PERSON

    event = createTestEvent('LEAVE')
    updatedState = handleEvent(event, state)
    expect_that(updatedState, is_invariant_except(state,
        c('lastUpdate', 'seats')))
    expect_that(updatedState$seats, is_invariant_except(state$seats,
        c('persons')))
    expect_that(is.na(updatedState$seats$persons[TEST_SEAT]), is_true())
})

test_that('CHANGE_SEAT state update works', {
    state = newState()
    state$seats$persons[1] = TEST_PERSON

    event = createTestEvent('CHANGE_SEAT')
    updatedState = handleEvent(event, state)
    expect_that(updatedState, is_invariant_except(state,
        c('lastUpdate', 'seats')))
    expect_that(updatedState$seats, is_invariant_except(state$seats,
        c('persons')))
    expect_that(is.na(updatedState$seats$persons[1]), is_true())
    expect_that(updatedState$seats$persons[TEST_SEAT], equals(TEST_PERSON))
})

test_that('PLACE_BAGGAGE state update works', {
    state = newState()
    event = createTestEvent('PLACE_BAGGAGE')
    updatedState = handleEvent(event, state)
    expect_that(updatedState, is_invariant_except(state,
        c('lastUpdate', 'seats')))
    expect_that(updatedState$seats, is_invariant_except(state$seats,
        c('baggage')))
    expect_that(updatedState$seats$baggage[TEST_SEAT], equals(TEST_PERSON))
})

test_that('REMOVE_BAGGAGE state update works', {
    state = newState()
    event = createTestEvent('REMOVE_BAGGAGE')
    updatedState = handleEvent(event, state)
    expect_that(updatedState, is_invariant_except(state,
        c('lastUpdate', 'seats')))
    expect_that(updatedState$seats, is_invariant_except(state$seats,
        c('baggage')))
    expect_that(is.na(updatedState$seats$baggage[TEST_SEAT]), is_true())
})

test_that('DISTURBING state update works', {
    state = newState()
    event = createTestEvent('DISTURBING')
    updatedState = handleEvent(event, state)
    # TODO implement
})

test_that('STOPS_DISTURBING state update works', {
    state = newState()
    event = createTestEvent('STOPS_DISTURBING')
    updatedState = handleEvent(event, state)
    # TODO implement
})

test_that('DOOR_RELEASE state update works', {
    state = newState()
    event = createTestEvent('DOOR_RELEASE')
    updatedState = handleEvent(event, state)
    expect_that(updatedState, is_invariant_except(state,
        c('lastUpdate', 'stopping')))
    expect_that(updatedState$stopping, is_true())
})

test_that('TRAIN_STARTS state update works', {
    state = newState()
    event = createTestEvent('TRAIN_STARTS')
    updatedState = handleEvent(event, state)
    expect_that(updatedState, is_invariant_except(state,
        c('lastUpdate', 'stopping')))
    expect_that(updatedState$stopping, is_false())
})

test_that('DIRECTION_CHANGE state update works', {
    state = newState()
    event = createTestEvent('DIRECTION_CHANGE')
    updatedState = handleEvent(event, state)
    expect_that(updatedState, is_invariant_except(state,
        c('lastUpdate', 'direction')))
    expect_that(updatedState$direction, equals('BACKWARD'))
})

test_that('COUNT_STANDING_PERSONS state update works', {
    state = newState()
    event = createTestEvent('COUNT_STANDING_PERSONS')
    updatedState = handleEvent(event, state)
    expect_that(updatedState, is_invariant_except(state,
        c('lastUpdate', 'standingPersons')))
    expect_that(updatedState$standingPersons, equals(TEST_EXTRA_INT))
})
