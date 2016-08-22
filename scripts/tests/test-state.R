# tests if a list is invariant except given fields
is_invariant_except = function(originalList, notInvariantFields) {
    function(x) {
        for (field in notInvariantFields) {
            x[field] = originalList[field]
        }
        expect_equal(x, originalList)
    }
}

test_that('newState produces the desired list', {
    state = newState()
    expect_that(state, is_a('list'))
    expect_that(length(state), equals(6))
    expect_that(state$seats, is_a('list'))
    expect_that(length(state$seats), equals(3))
    expect_that(state$stopping, equals(FALSE))
    expect_that(state$direction, is_a('factor'))
    expect_that(as.character(state$direction), equals('FORWARD'))
    expect_that(state$standingPersons, equals(NA))
    expect_that(state$lastUpdate, equals(NULL))
    expect_that(state$initialized, equals(FALSE))
})

test_that('setting class for events works', {
    eventType = 'INITIALIZATION_END'
    event = data.frame(EVENT_TYPE = factor(eventType))
    event = setClassForEvent(event)
    expect_that(event, is_a(eventType))
})

test_that('selecting the right method works', {
    eventType = 'INITIALIZATION_END'
    event = data.frame(EVENT_TYPE = factor(eventType), TIME = c('13:13:13'))
    event = setClassForEvent(event)

    state = newState()
    expect_that(state$initialized, is_false())
    state = handleEvent(event, state)
    expect_that(state$initialized, is_true())
})

test_that('the default method gives a warning', {
    event = data.frame(EVENT_TYPE = c('INVALID'))
    event = setClassForEvent(event)

    state = newState()
    expect_that(handleEvent(event, state), gives_warning('not supported'))
    returnedState = suppressWarnings(handleEvent(event, state))
    expect_that(returnedState, equals(state))
})

test_that('is_invariant_except works', {
    original = list(a = 1, b = 'foo', c = 'bar')

    x = original
    expect_that(x, is_invariant_except(original, c('a')))

    x$a = 2
    expect_that(x, is_invariant_except(original, c('a')))
    
    x$b = 'baz'
    # fail expected:
    # expect_that(x, is_invariant_except(original, c('a')))

    x = original
    x$c = 'baz'
    # fail expected:
    # expect_that(x, is_invariant_except(original, c('a')))
})

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
    expect_that(updatedState$seats$baggage[TEST_SEAT], equals(NULL))
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
    expect_that(updatedState$seats$persons[TEST_SEAT], equals(NULL))
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
    expect_that(updatedState$seats$persons[1], equals(NULL))
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
    expect_that(updatedState$seats$baggage[TEST_SEAT], equals(NULL))
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
