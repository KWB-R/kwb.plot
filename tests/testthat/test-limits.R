test_that("getCurrentLimits() works", {

  kwb.plot:::getCurrentLimits()
})

test_that("inLimits() works", {

  expect_error(kwb.plot:::inLimits())
})

test_that("appropriateLimits() works", {

  expect_error(kwb.plot:::appropriateLimits())
})

test_that("userCoordinatesToLimits() works", {

  expect_error(kwb.plot:::userCoordinatesToLimits())
})

