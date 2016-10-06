source('analysis.R', chdir = TRUE)

# Precondition: The following global data frame variables must exist:
# surveyData
# personData
# logEventData



## ---- seating-survey-table ----

tableColumns = c('DATE', 'TIME', 'STARTING_AT', 'LINE', 'DESTINATION')
xtab = xtable(surveyData[, tableColumns],
    # align = c('r', 'p{3cm}', 'p{10cm}'),
    label = 'tab:surveys',
    caption = 'List of conducted surveys.')

print(xtab, type = 'latex',
    table.placement = 'ht')

## ---- seating-data-plot-age-groups ----

personData$ageGroupOrdered = factor(personData$AGE_GROUP,
        levels = c('YOUNG_CHILD', 'SCHOOLCHILD', 'YOUTHFUL',
                   'YOUNG_ADULT', 'ADULT', 'AGED', NA))
ggplot(personData, aes(ageGroupOrdered)) +
    geom_bar(width = 0.1) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
    ggtitle('Passengers by age groups') +
    xlab('age group')

## ---- seating-data-plot-gender ----

ggplot(personData, aes(GENDER)) +
    geom_bar(width = 0.1) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
    ggtitle('Passengers by gender') +
    xlab('gender')

## ---- seating-group-related-data ----

groupRelatedSeatingData = seatingData %>%
    inner_join(personData, by = c('person' = 'ID')) %>%
    filter(!is.na(M_GROUP))

## ---- seating-data-plot-groups ----

personData$inGroup = ifelse(is.na(personData$M_GROUP), 'ALONE', 'GROUP')
ggplot(personData, aes(inGroup)) +
    geom_bar(width = 0.1) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
    ggtitle('Number of passengers traveling alone or in groups')

## ---- seating-data-plot-empty-side ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 0)
ggplot(filteredData, aes(seatSide)) +
    geom_bar(width = 0.1) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
    ggtitle('Preference for window/aisle seats in empty seat group')

## ---- seating-data-plot-empty-direction ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 0)
ggplot(filteredData, aes(seatDirection)) +
    geom_bar(width = 0.1) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
    ggtitle('Preference for facing direction in empty seat group')

## ---- seating-data-plot-empty-side-direction ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 0)
filteredData = mutate(filteredData,
        seatPosition = interaction(seatSide, seatDirection, sep = '_'))
ggplot(filteredData, aes(seatPosition)) +
    geom_bar(width = 0.1) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
    ggtitle('Seat preference in empty seat group')

## ---- seating-data-plot-position-relative ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 1)
filteredData$positionRelative = getPositionRelative(filteredData)
ggplot(filteredData, aes(positionRelative)) +
    geom_bar(width = 0.1) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
    ggtitle('Preference for position relative to one other person')

## ---- seating-data-plot-position-relative-window ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 1)
filteredData$positionRelative = getPositionRelative(filteredData)
ggplot(filteredData, aes(positionRelative)) +
    geom_bar(width = 0.1) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
    facet_wrap(~ seatSide) +
    ggtitle('Preference for position relative to one other person splitted by chosen seat')

## ---- seating-data-plot-position-relative-forward ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 1)
filteredData$positionRelative = getPositionRelative(filteredData)
ggplot(filteredData, aes(positionRelative)) +
    geom_bar(width = 0.1) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
    facet_wrap(~ seatDirection) +
    ggtitle('Preference for position relative to one other person splitted by chosen seat')

## ---- seating-data-plot-position-relative-4 ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 1)
filteredData$positionRelative = getPositionRelative(filteredData)

filteredData = mapAndAddNewColumn(filteredData, getTheOtherPersonSide)
filteredData = nameLastColumnAndConvertToFactor(filteredData, 'theOtherPersonSide')
filteredData = mapAndAddNewColumn(filteredData, getTheOtherPersonDirection)
filteredData = nameLastColumnAndConvertToFactor(filteredData, 'theOtherPersonDirection')

ggplot(filteredData, aes(positionRelative)) +
    geom_bar(width = 0.1) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
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

seatingData = mapAndAddNewColumn(seatingData, getChosenSeatGroup)
seatingData = nameLastColumnAndConvertToFactor(seatingData, 'seatGroupOccupancy')
filteredData = filter(seatingData, !is.na(seatGroupOccupancy) & is.na(group))

ggplot(filteredData, aes(seatGroupOccupancy)) +
    geom_bar(width = 0.1) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
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

seatingData = mapAndAddNewColumn(seatingData, getChosenSeatGroup01vs23)
seatingData = nameLastColumnAndConvertToFactor(seatingData, 'seatGroup01vs23')
filteredData = filter(seatingData, !is.na(seatGroup01vs23) & is.na(group))

ggplot(filteredData, aes(seatGroup01vs23)) +
    geom_bar(width = 0.1) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
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

seatingData = mapAndAddNewColumn(seatingData, getChosenSeatGroupEmpty)
seatingData = nameLastColumnAndConvertToFactor(seatingData, 'seatGroupEmptyVsOther')
filteredData = filter(seatingData, !is.na(seatGroupEmptyVsOther) & is.na(group))

ggplot(filteredData, aes(seatGroupEmptyVsOther)) +
    geom_bar(width = 0.1) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
    ggtitle('Preference for seat groups within a compartment')

## ---- seating-data-plot-2other-side ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 2)
# only consider decisions where there was no choice between the facing direction
filteredData = filter(filteredData, is.na(personNext))
ggplot(filteredData, aes(seatSide)) +
    geom_bar(width = 0.1) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
    ggtitle('Preference for window/aisle seats in seat group with two others')

## ---- seating-data-plot-2other-direction ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 2)
# only consider decisions where there was no choice between the side
filteredData = filter(filteredData, is.na(personAcross))
ggplot(filteredData, aes(seatDirection)) +
    geom_bar(width = 0.1) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
    ggtitle('Preference for facing direction in seat group with two others')

## ---- seating-data-plot-2other-side-direction ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 2)
# only consider decision where there was only choice between two diagonally opposite seats
# flaw: cannot be splitted with facets
filteredData = filter(filteredData, is.na(personDiagonal))
filteredData = mutate(filteredData,
        seatPosition = interaction(seatSide, seatDirection, sep = '_'))
filteredData$diagonalPair = paste0('diagonal pair ',
        ifelse(filteredData$seatPosition %in% c('AISLE_BACKWARD', 'WINDOW_FORWARD'), 1, 2))
ggplot(filteredData, aes(seatPosition)) +
    geom_bar(width = 0.1) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
    facet_wrap(~ diagonalPair) +
    scale_x_discrete(drop = FALSE) +
    ggtitle('Preference for seats in seat group with two others')

## ---- seating-data-plot- ----
