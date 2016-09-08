
newSeatGroupChoiceTestRow = function(seatGroup, nPersonsSeatGroup) {
    nPersColumnNames = paste0('nPersonsSeatGroup', 1:4)
    columnList = list()
    for (i in 1:4) {
        columnList[nPersColumnNames[i]] = nPersonsSeatGroup[i]
    }
    result = data.frame(seatGroup = seatGroup)
    result = cbind(result, columnList)
    result
}

test_that('minimum method to analyze seat group choice works', {

    for (i in 0:4) {
        row = newSeatGroupChoiceTestRow(1, rep(i, 4))
        expect_that(is.na(getChosenSeatGroup(row)), is_true())
    }

    row = newSeatGroupChoiceTestRow(2, c(3,1,2,3))
    expect_that(getChosenSeatGroup(row), equals('SMALLEST_NUMBER'))

    for (i in 1:2) {

        row = newSeatGroupChoiceTestRow(i, c(1,1,2,3))
        expect_that(getChosenSeatGroup(row), equals('SMALLEST_NUMBER'))

        row = newSeatGroupChoiceTestRow(i, c(2,2,2,3))
        expect_that(getChosenSeatGroup(row), equals('SMALLEST_NUMBER'))

    }

    for (i in 3:4) {

        row = newSeatGroupChoiceTestRow(i, c(1,1,2,3))
        expect_that(getChosenSeatGroup(row), equals('OTHER_NUMBER'))

        row = newSeatGroupChoiceTestRow(i, c(1,2,2,3))
        expect_that(getChosenSeatGroup(row), equals('OTHER_NUMBER'))

    }

})

test_that('0:1 or 2:3 method to analyze seat group choice works', {

    row = newSeatGroupChoiceTestRow(1, c(1,0,1,0))
    expect_that(is.na(getChosenSeatGroup01vs23(row)), is_true())

    row = newSeatGroupChoiceTestRow(1, c(2,3,2,3))
    expect_that(is.na(getChosenSeatGroup01vs23(row)), is_true())

    for (i in 1:2) {
        row = newSeatGroupChoiceTestRow(i, 0:3)
        expect_that(getChosenSeatGroup01vs23(row), equals('0_OR_1'))
    }

    for (i in 3:4) {
        row = newSeatGroupChoiceTestRow(i, 0:3)
        expect_that(getChosenSeatGroup01vs23(row), equals('2_OR_3'))
    }

})
