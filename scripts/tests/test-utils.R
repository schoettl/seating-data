
test_that('count available values works', {
    expect_that(countAvailableValues(c(NA,1,NA,2,NA,NA,3,NA)), equals(3))
})

test_that('forEach... map function works', {
    df = data.frame(a = c(1:3,    NA),
                    b = c(1:2, NA,NA),
                    c = c(1,NA,NA,NA),
                    d = c(1:4))
    expect_that(forEachRowAsVec(df, c('a', 'b', 'c'), countAvailableValues), equals(3:0))
})
