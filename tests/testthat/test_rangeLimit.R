context("Range limit")
library(STManaged)

set.seed(0.0)
initLand <- create_virtual_landscape(cellSize = 4)
rg <- range_limit(initLand[['land']], nRow = initLand[['nRow']], nCol = initLand[['nCol']], occup = 0.7)


test_that("range_limit returns a vector of 2 elements", {
    expect_equal(length(rg), 2)
})

test_that("range_limit returns a vector with correct names", {
    expect_equal(names(rg), c('limitB', 'limitT'))
})

test_that("range_limit returns correct value of range limit", {
    expect_identical(rg, c(limitB = 54, limitT = 102))
})