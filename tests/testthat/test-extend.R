test_that("the tail is filled with cumulative growth", {
  level  <- c(100, 105, 110, NA, NA)
  growth <- c(NA, 0.05, 0.048, 0.05, 0.04)
  time   <- 2020:2024

  expect_equal(extend_with_change(level, growth, time),
               c(100, 105, 110, 110 * 1.05, 110 * 1.05 * 1.04))
})

test_that("observed values are never overwritten", {
  level  <- c(100, 105, 110, NA)
  growth <- c(0.9, 0.9, 0.9, 0.02)
  time   <- 2020:2023

  out <- extend_with_change(level, growth, time)
  expect_equal(out[1:3], level[1:3])
  expect_equal(out[4], 110 * 1.02)
})

test_that("a gap inside the series is left alone", {
  # only the tail is filled, the hole in the middle stays a hole
  level  <- c(100, NA, 110, NA)
  growth <- c(NA, 0.05, 0.05, 0.03)
  time   <- 2020:2023

  out <- extend_with_change(level, growth, time)
  expect_true(is.na(out[2]))
  expect_equal(out[4], 110 * 1.03)
})

test_that("nothing to do when the series is complete", {
  level <- c(100, 105, 110)
  expect_equal(extend_with_change(level, c(NA, 0.05, 0.05), 2020:2022), level)
})

test_that("a missing growth rate stops the chain", {
  level  <- c(100, NA, NA, NA)
  growth <- c(NA, 0.05, NA, 0.04)
  time   <- 2020:2023

  out <- extend_with_change(level, growth, time)
  expect_equal(out[2], 105)
  expect_true(all(is.na(out[3:4])))
})

test_that("an all missing series comes back untouched", {
  expect_true(all(is.na(
    extend_with_change(rep(NA_real_, 3), c(0.1, 0.1, 0.1), 2020:2022))))
})

test_that("the order of the rows does not matter", {
  level  <- c(100, 105, 110, NA, NA)
  growth <- c(NA, 0.05, 0.048, 0.05, 0.04)
  time   <- 2020:2024
  ord    <- c(3, 5, 1, 4, 2)

  expect_equal(extend_with_change(level[ord], growth[ord], time[ord]),
               extend_with_change(level, growth, time)[ord])
})

test_that("dates work as well as years", {
  level  <- c(100, 105, NA)
  growth <- c(NA, 0.05, 0.02)
  expect_equal(
    extend_with_change(level, growth, as.Date(c("2020-01-01", "2021-01-01", "2022-01-01"))),
    extend_with_change(level, growth, 2020:2022))
})

test_that("bad input is rejected", {
  expect_error(extend_with_change(1:3, 1:2, 2020:2022), "same length")
  expect_error(extend_with_change(1:3, 1:3, 2020:2021), "same length")
})
