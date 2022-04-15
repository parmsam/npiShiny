library(testthat)
test_that("stdz_zips works to format 9 digit numbers", {
  expected <- stdz_zips(902101010)
  expect_type(expected, "character")
  expect_equal(expected, "90210-1010")
})

test_that("stdz_zips works to format 5 digit numbers", {
  expected2 <- stdz_zips(90210)
  expect_type(expected2, "character")
  expect_equal(expected2, "90210")
})
