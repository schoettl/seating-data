source('analysis.R', chdir = TRUE)

# Precondition: The following global data frame variables must exist:
# surveyData
# personData
# logEventData
# For tests, data is loaded this way:
source('chunks-collected-data.R')



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
ggp = ggplot(personData, aes(ageGroupOrdered))
makeBarsWithRelativeFrequency(ggp) +
    #ggtitle('Passengers by age groups') +
    xlab('age group')

## ---- seating-data-plot-gender ----

ggp = ggplot(personData, aes(GENDER))
makeBarsWithRelativeFrequency(ggp) +
    #ggtitle('Passengers by gender') +
    xlab('gender')

## ---- seating-group-related-data ----

groupRelatedSeatingData = seatingData %>%
    inner_join(personData, by = c('person' = 'ID')) %>%
    filter(!is.na(M_GROUP))

## ---- seating-data-plot-groups ----

personData$inGroup = ifelse(is.na(personData$M_GROUP), 'ALONE', 'GROUP')
ggp = ggplot(personData, aes(inGroup))
makeBarsWithRelativeFrequency(ggp)
    #ggtitle('Number of passengers traveling alone or in groups')

## ---- seating-data-plot-empty-side ----

plotSeatGroup0Side = function(seatingData) {
    filteredData <<- filterDataNoGroupAndNPersonsSeatGroup(seatingData, 0)
    filteredData <<- mutate(filteredData, seatSide = factor(seatSide,
                                levels = c('AISLE', 'WINDOW')))
    ggp = ggplot(filteredData, aes(seatSide))
    makeBarsWithRelativeFrequency(ggp)
        #ggtitle('Preference for window/aisle seats in empty seat group')
}

plotSeatGroup0Side(seatingData)

## ---- seating-data-plot-empty-direction ----

plotSeatGroup0Direction = function(seatingData) {
    filteredData <<- filterDataNoGroupAndNPersonsSeatGroup(seatingData, 0)
    filteredData <<- mutate(filteredData, factor(seatDirection,
                            levels = c('BACKWARD', 'FORWARD')))
    ggp = ggplot(filteredData, aes(seatDirection))
    makeBarsWithRelativeFrequency(ggp)
        #ggtitle('Preference for facing direction in empty seat group')
}

plotSeatGroup0Direction(seatingData)

## ---- seating-data-plot-empty-side-direction ----

plotSeatGroup0SideDirection = function(seatingData) {
    filteredData <<- filterDataNoGroupAndNPersonsSeatGroup(seatingData, 0)
    filteredData <<- mutate(filteredData,
            seatPosition = interaction(seatSide, seatDirection, sep = '_'),
            seatPosition = factor(seatPosition,
                    levels = c('AISLE_BACKWARD',
                               'WINDOW_BACKWARD',
                               'AISLE_FORWARD',
                               'WINDOW_FORWARD')))
    ggp = ggplot(filteredData, aes(seatPosition))
    makeBarsWithRelativeFrequency(ggp) +
        #ggtitle('Seat preference in empty seat group') +
        scale_x_discrete(drop = FALSE)
}

plotSeatGroup0SideDirection(seatingData)

## ---- seating-data-plot-position-relative ----

plotSeatGroup1 = function(seatingData) {
    filteredData <<- filterDataNoGroupAndNPersonsSeatGroup(seatingData, 1)
    filteredData <<- mutate(filteredData,
                            positionRelative = getPositionRelative(filteredData),
                            positionRelative = factor(positionRelative,
                                levels = c('NEXT', 'ACROSS', 'DIAGONAL')))
    ggp = ggplot(filteredData, aes(positionRelative))
    makeBarsWithRelativeFrequency(ggp)
        #ggtitle('Preference for position relative to one other person')
}

plotSeatGroup1(seatingData)

## ---- seating-data-plot-position-relative-window ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 1)
filteredData$positionRelative = getPositionRelative(filteredData)
ggp = ggplot(filteredData, aes(positionRelative))
makeBarsWithRelativeFrequency(ggp) +
    #ggtitle('Preference for position relative to one other person splitted by chosen seat') +
    facet_wrap(~ seatSide)

## ---- seating-data-plot-position-relative-forward ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 1)
filteredData$positionRelative = getPositionRelative(filteredData)
ggp = ggplot(filteredData, aes(positionRelative))
makeBarsWithRelativeFrequency(ggp) +
    #ggtitle('Preference for position relative to one other person splitted by chosen seat') +
    facet_wrap(~ seatDirection)

## ---- seating-data-plot-position-relative-4 ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 1)
filteredData$positionRelative = getPositionRelative(filteredData)

filteredData = mapAndAddNewColumn(filteredData, getTheOtherPersonSide)
filteredData = nameLastColumnAndConvertToFactor(filteredData, 'theOtherPersonSide')
filteredData = mapAndAddNewColumn(filteredData, getTheOtherPersonDirection)
filteredData = nameLastColumnAndConvertToFactor(filteredData, 'theOtherPersonDirection')

ggp = ggplot(filteredData, aes(positionRelative))
makeBarsWithRelativeFrequency(ggp) +
    #ggtitle('Preference for position relative to one other person depending on their position') +
    facet_grid(theOtherPersonDirection ~ theOtherPersonSide)

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

plotChosenSeatGroupMin = function(seatingData) {
    seatingData = mapAndAddNewColumn(seatingData, getChosenSeatGroup)
    seatingData = nameLastColumnAndConvertToFactor(seatingData, 'seatGroupOccupancy')
    filteredData <<- filter(seatingData, !is.na(seatGroupOccupancy) & is.na(group))
    ggp = ggplot(filteredData, aes(seatGroupOccupancy))
    makeBarsWithRelativeFrequency(ggp)
        #ggtitle('Preference for seat groups within a compartment')
}

plotChosenSeatGroupMin(seatingData)

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

ggp = ggplot(filteredData, aes(seatGroup01vs23))
makeBarsWithRelativeFrequency(ggp)
    #ggtitle('Preference for seat groups within a compartment')

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

ggp = ggplot(filteredData, aes(seatGroupEmptyVsOther))
makeBarsWithRelativeFrequency(ggp)
    #ggtitle('Preference for seat groups within a compartment')

## ---- seating-data-plot-2other-side ----

plotSeatGroup2Side = function(seatingData) {
    filteredData <<- filterDataNoGroupAndNPersonsSeatGroup(seatingData, 2)
    # only consider decisions where there was no choice between the facing direction
    filteredData <<- filter(filteredData, is.na(personNext))
    filteredData <<- mutate(filteredData, seatSide = factor(seatSide,
                                levels = c('AISLE', 'WINDOW')))
    ggp = ggplot(filteredData, aes(seatSide))
    makeBarsWithRelativeFrequency(ggp)
        #ggtitle('Preference for window/aisle seats in seat group with two others')
}

plotSeatGroup2Side(seatingData)

## ---- seating-data-plot-2other-direction ----

plotSeatGroup2Direction = function(seatingData) {
    filteredData <<- filterDataNoGroupAndNPersonsSeatGroup(seatingData, 2)
    # only consider decisions where there was no choice between the side
    filteredData <<- filter(filteredData, is.na(personAcross))
    filteredData <<- mutate(filteredData, seatDirection = factor(seatDirection,
                                levels = c( 'BACKWARD', 'FORWARD')))
    ggp = ggplot(filteredData, aes(seatDirection))
    makeBarsWithRelativeFrequency(ggp)
        #ggtitle('Preference for facing direction in seat group with two others')
}

plotSeatGroup2Direction(seatingData)

## ---- seating-data-plot-2other-side-direction ----

filteredData = filterDataNoGroupAndNPersonsSeatGroup(seatingData, 2)
# only consider decision where there was only choice between two diagonally opposite seats
# flaw: cannot be splitted with facets
filteredData = filter(filteredData, is.na(personDiagonal))
filteredData = mutate(filteredData,
        seatPosition = interaction(seatSide, seatDirection, sep = '_'))
filteredData$diagonalPair = paste0('diagonal pair ',
        ifelse(filteredData$seatPosition %in% c('AISLE_BACKWARD', 'WINDOW_FORWARD'), 1, 2))
ggp = ggplot(filteredData, aes(seatPosition))
makeBarsWithRelativeFrequency(ggp) +
    facet_wrap(~ diagonalPair) +
    #ggtitle('Preference for seats in seat group with two others') +
    scale_x_discrete(drop = FALSE) +

## ---- seating-data-plot- ----
