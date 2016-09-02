
test_that('has no duplicate ids expectation works', {
    df = data.frame(ID = 1:100)
    expect_that(df, has_no_duplicate_ids())

    # supposed to fail:
    # df = data.frame(ID = c(1:100, 20:30, 90:110))
    # expect_that(df, has_no_duplicate_ids())
})

test_that('has monotonic increasing col expectation works', {
    df = data.frame(ID = 1:100, COL = 1:100)
    expect_that(df, has_monotonic_increasing_column('COL'))

    df = data.frame(ID = 1:202, COL = c(1:100, 100, 100:200))
    expect_that(df, has_monotonic_increasing_column('COL'))

    df = data.frame(ID = 1:6, COL = c(1,2,5,7,8,10))
    expect_that(df, has_monotonic_increasing_column('COL'))

    # supposed to fail:
    # df = data.frame(ID = 1:202, COL = c(1:100, 99, 100:200))
    # expect_that(df, has_monotonic_increasing_column('COL'))

    # supposed to fail:
    # df = data.frame(ID = 1:101, COL = c(1:50, 49, 51:100))
    # expect_that(df, has_monotonic_increasing_column('COL'))

})

test_that('has monotonic increasing col within survey expectation works', {
    surveys = c(rep(1, 50), rep(2, 50))

    df = data.frame(ID = 1:100, COL = 1:100, SURVEY = surveys)
    expect_that(df, has_monotonic_increasing_column_per_survey('COL'))

    df = data.frame(ID = 1:100, COL = c(1:50, 1:50), SURVEY = surveys)
    expect_that(df, has_monotonic_increasing_column_per_survey('COL'))

    # supposed to fail:
    # df = data.frame(ID = 1:100, COL = c(1:50, 50:1), SURVEY = surveys)
    # expect_that(df, has_monotonic_increasing_column_per_survey('COL'))

    # supposed to fail:
    # df = data.frame(ID = 1:100, COL = c(1:49, 1:51), SURVEY = surveys)
    # expect_that(df, has_monotonic_increasing_column_per_survey('COL'))

    # supposed to fail:
    # df = data.frame(ID = 1:100, COL = c(1:70, 1:30), SURVEY = surveys)
    # expect_that(df, has_monotonic_increasing_column_per_survey('COL'))

})
