context("Create landscape")
library(STManaged)

test_that("create_landscape returns a list of 6 elements", {

  initLand <- create_landscape(cellSize = 4)

  expect_equal(names(initLand), c("land", "env1", "nCol", "nRow", "position", "neighbor"))
})

test_that("create_landscape returns an integer vector with four forest states", {

  initLand <- create_landscape(cellSize = 4)
  land <- initLand[['land']]

  expect_equal(sort(unique(land)), c(1, 2, 3, 4))
  expect_match(class(land), "integer")
})

test_that("create_landscape returns a proper env1 vector", {

  cr <- c(-3, 0.5)
  initLand <- create_landscape(climRange = cr, cellSize = 4)
  env1 <- initLand[['env1']]

  expect_equal(length(env1), 800/4)
  expect_equal(c(env1[1], env1[length(env1)]), cr)
})

test_that("create_landscape returns good dimensions", {

  initLand <- create_landscape(cellSize = 4)
  nCol <- initLand[['nCol']]
  nRow <- initLand[['nRow']]

  expect_true(nCol > nRow)
})

test_that("create_landscape position and neighbor are correct", {

  initLand <- create_landscape(cellSize = 4)
  position <- initLand[['position']]
  neighbor <- initLand[['neighbor']]

  expect_identical(length(position), length(neighbor))
  expect_equal(position[1], initLand[['nCol']] + 2)
  expect_equal(position[1], neighbor[[1]][5])
})

test_that("create_landscape position and neighbor are correct", {

  initLand <- create_landscape(cellSize = 4)
  position <- initLand[['position']]
  neighbor <- initLand[['neighbor']]

  expect_identical(length(position), length(neighbor))
  expect_equal(position[1], initLand[['nCol']] + 2)
  expect_equal(position[1], neighbor[[1]][5])
})
