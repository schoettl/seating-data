
test_that('paste brings together date and time', {
    expect_that(paste(c('2016-08-22', '2016-08-22'), c('13:13:13', '13:13:13')),
                equals(c('2016-08-22 13:13:13', '2016-08-22 13:13:13')))
})

test_that('lubridate works as I expect it', {
    expect_that(ymd_hms('2016-08-22 13:13:13'), is_a('POSIXct'))

    expect_that(hms('13:13:13'), is_a('Period'))
    expect_that(ymd('2016-08-22') + hms('13:13:13'), equals(ymd_hms('2016-08-22 13:13:13')))
})
