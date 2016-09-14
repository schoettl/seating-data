source('analysis.R', chdir = TRUE)

## ---- seating-load-data ----

surveyData   = readCsvFile('SURVEY')
personData   = readCsvFile('PERSON')
logEventData = readCsvFile('LOG_EVENT')

## ---- seating-check-data ----

# Convert TIME from factor to string to enable check for monotonicity
logEventData = mutate(logEventData, TIME = as.character(TIME))

test_that('data is valid', {
    expect_that(surveyData, has_no_duplicate_ids())
    expect_that(personData, has_no_duplicate_ids())
    expect_that(logEventData, has_monotonic_increasing_column('ID'))
    expect_that(logEventData,
                has_monotonic_increasing_column_per_survey('TIME'))
})

## ---- seating-keep-raw-data ----

surveyRawData   = surveyData
personRawData   = personData
logEventRawData = logEventData

## ---- seating-preprocessing ----

# Remove duplicate INITIALIZATION_END events
logEventData = removeDuplicateInitEndEvents(logEventData)

# Fix column types and NA values

# - Change missing foreign key values to NA
# - Change missing text to NA
# - Fix date/time columns

surveyData = mutate(surveyData,
    DATE = as.character(DATE), # from factor
    AGENT = ifelse(AGENT == 0, NA, AGENT))

personData = mutate(personData,
    M_GROUP = ifelse(M_GROUP == 0, NA, M_GROUP))

logEventData = mutate(logEventData,
    PERSON = ifelse(PERSON == 0, NA, PERSON),
    EXTRA_STRING = ifelse(EXTRA_STRING == '', NA, as.character(EXTRA_STRING)),
    TIME = as.character(TIME), # from factor
    LTIME = hms(TIME))

# Add TIME column to surveyData
surveyData = addTimeColumn(surveyData, logEventData)

surveyData = mutate(surveyData,
    LDATE = ymd(DATE),
    LTIME = hms(TIME),
    LDATETIME = ymd_hms(paste(DATE, TIME)))

# Fix order of log events
# (Above operations cannot guarantee to keep the order)
logEventData = arrange(logEventData, ID, TIME)

## ---- seating-generate-seating-data ----

seatingData = generateMoreData(surveyData, logEventData)

## ---- seating-survey-table ----

tableColumns = c('DATE', 'TIME', 'STARTING_AT', 'LINE', 'DESTINATION')
xtab = xtable(surveyData[, tableColumns],
    # align = c('r', 'p{3cm}', 'p{10cm}'),
    label = 'tab:surveys',
    caption = 'List of conducted surveys.')

print(xtab, type = 'latex',
    table.placement = 'ht')

## ---- seating-group-related-data ----

groupRelatedSeatingData = seatingData %>%
    inner_join(personData, by = c('person' = 'ID')) %>%
    filter(!is.na(M_GROUP))

## ---- seating-data-plot-groups ----

seatingData$inGroup = !is.na(seatingData$group)
ggplot(seatingData, aes(inGroup)) +
    geom_bar(width = 0.1) +
    ggtitle('Number of passengers traveling alone or in groups')

## ---- seating-data-plot-empty-side ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 0)
ggplot(filteredData, aes(seatSide)) +
    geom_bar(width = 0.1) +
    ggtitle('Preference for window/aisle seats in empty seat group')

## ---- seating-data-plot-empty-direction ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 0)
ggplot(filteredData, aes(seatDirection)) +
    geom_bar(width = 0.1) +
    ggtitle('Preference for facing direction in empty seat group')

## ---- seating-data-plot-empty-side-direction ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 0)
filteredData = mutate(filteredData, seatSideDirection = interaction(seatSide, seatDirection, sep = '_'))
ggplot(filteredData, aes(seatSideDirection)) +
    geom_bar(width = 0.1) +
    ggtitle('Seat preference in empty seat group')

## ---- seating-data-plot-position-relative ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 1)
filteredData$positionRelative = getPositionRelative(filteredData)
ggplot(filteredData, aes(positionRelative)) +
    geom_bar(width = 0.1) +
    ggtitle('Preference for position relative to one other person')

## ---- seating-data-plot-position-relative-window ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 1)
filteredData$positionRelative = getPositionRelative(filteredData)
ggplot(filteredData, aes(positionRelative)) +
    geom_bar(width = 0.1) +
    facet_wrap(~ seatSide) +
    ggtitle('Preference for position relative to one other person splitted by chosen seat')

## ---- seating-data-plot-position-relative-forward ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 1)
filteredData$positionRelative = getPositionRelative(filteredData)
ggplot(filteredData, aes(positionRelative)) +
    geom_bar(width = 0.1) +
    facet_wrap(~ seatDirection) +
    ggtitle('Preference for position relative to one other person splitted by chosen seat')

## ---- seating-data-plot-position-relative-4 ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 1)
filteredData$positionRelative = getPositionRelative(filteredData)

filteredData = adply(filteredData, 1, getTheOtherPersonSide)
filteredData = nameLastColumnAndConvertToFactor(filteredData, 'theOtherPersonSide')
filteredData = adply(filteredData, 1, getTheOtherPersonDirection)
filteredData = nameLastColumnAndConvertToFactor(filteredData, 'theOtherPersonDirection')

ggplot(filteredData, aes(positionRelative)) +
    geom_bar(width = 0.1) +
    facet_grid(theOtherPersonDirection ~ theOtherPersonSide) +
    ggtitle('Preference for position relative to one other person depending on their position')

## ---- seating-data-plot-chosen-seat-group-min ----

# How often do people choose the most sparse seat group?

getChosenSeatGroup = function(x) {
    chosenSeatGroup = x$seatGroup
    columns = paste0('nPersonsSeatGroup', 1:4)
    counts = as.numeric(x[columns])
    if (length(unique(counts)) == 1) # same counts in all seat groups
        return(NA)
    ranks = rank(counts, ties.method = 'min')
    # Ranking of chosen seat group is minimal?
    if (ranks[chosenSeatGroup] == min(ranks)) {
        return('SMALLEST_NUMBER') # seat group with lowest number of other persons
    } else {
        return('OTHER_NUMBER')
    }
}

seatingData = adply(seatingData, 1, getChosenSeatGroup)
seatingData = nameLastColumnAndConvertToFactor(seatingData, 'seatGroupOccupancy')
filteredData = filter(seatingData, !is.na(seatGroupOccupancy) & is.na(group))

ggplot(filteredData, aes(seatGroupOccupancy)) +
    geom_bar(width = 0.1) +
    ggtitle('Preference for seat groups within a compartment')

## ---- seating-data-plot-chosen-seat-group-01vs23 ----

# How often do people choose a seat group with 0 or 1 others
# as opposed to a seat group with 2 or 3 others?

getChosenSeatGroup01vs23 = function(x) {
    chosenSeatGroup = x$seatGroup
    columns = paste0('nPersonsSeatGroup', 1:4)
    counts = as.numeric(x[columns])
    if (!any(0:1 %in% counts) || !any(2:3 %in% counts)) # NA if there are no seat groups to compare with
        return(NA)
    if (counts[chosenSeatGroup] %in% 0:1) {
        return('0_OR_1')
    } else {
        return('2_OR_3')
    }
}

seatingData = adply(seatingData, 1, getChosenSeatGroup01vs23)
seatingData = nameLastColumnAndConvertToFactor(seatingData, 'seatGroup01vs23')
filteredData = filter(seatingData, !is.na(seatGroup01vs23) & is.na(group))

ggplot(filteredData, aes(seatGroup01vs23)) +
    geom_bar(width = 0.1) +
    ggtitle('Preference for seat groups within a compartment')

## ---- seating-data-plot-chosen-seat-group-empty ----

# How often do people prefer an empty seat group to other seat groups?

getChosenSeatGroupEmpty = function(x) {
    chosenSeatGroup = x$seatGroup
    columns = paste0('nPersonsSeatGroup', 1:4)
    counts = as.numeric(x[columns])
    if (length(unique(counts)) == 1) # same counts in all seat groups
        return(NA)
    if (min(counts) != 0) # there is no empty seat group
        return(NA)
    if (counts[chosenSeatGroup] == 0) {
        return('EMPTY')
    } else {
        return('OTHER')
    }
}

seatingData = adply(seatingData, 1, getChosenSeatGroupEmpty)
seatingData = nameLastColumnAndConvertToFactor(seatingData, 'seatGroupEmptyVsOther')
filteredData = filter(seatingData, !is.na(seatGroupEmptyVsOther) & is.na(group))

ggplot(filteredData, aes(seatGroupEmptyVsOther)) +
    geom_bar(width = 0.1) +
    ggtitle('Preference for seat groups within a compartment')

## ---- seating-data-plot-2other-side ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 2)
# only consider decisions where there was no choice between the facing direction
filteredData = filter(filteredData, is.na(personNext))
ggplot(filteredData, aes(seatSide)) +
    geom_bar(width = 0.1) +
    ggtitle('Preference for window/aisle seats in seat group with two others')

## ---- seating-data-plot-2other-direction ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 2)
# only consider decisions where there was no choice between the side
filteredData = filter(filteredData, is.na(personAcross))
ggplot(filteredData, aes(seatDirection)) +
    geom_bar(width = 0.1) +
    ggtitle('Preference for facing direction in seat group with two others')

## ---- seating-data-plot-2other-side-direction ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 2)
filteredData = filter(filteredData, is.na(personDiagonal))
filteredData = mutate(filteredData,
        seatSideDirection = interaction(seatSide, seatDirection, sep = '_'))
ggplot(filteredData, aes(seatSideDirection)) +
    geom_bar(width = 0.1) +
    scale_x_discrete(drop = FALSE) +
    ggtitle('Preference for seats in seat group with two others')

## ---- seating-data-plot- ----
