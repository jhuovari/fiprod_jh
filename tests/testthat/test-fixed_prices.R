test_that("prev_year_prices matches its definition", {
  expect_equal(prev_year_prices(c(100, 120), c(100, 110), time = c(1, 2)),
               c(NA, 110))

  cp <- c(100, 120, 150)
  fp <- c(90, 110, 150)
  expect_equal(prev_year_prices(cp, fp, 2018:2020),
               c(NA, 100 * 110 / 90, 120 * 150 / 110))
})

test_that("prev_year_prices is NA when the previous year is missing", {
  # 2019 is not in the data, so 2020 has no previous year to be valued at
  expect_equal(prev_year_prices(c(1, 2, 3), c(1, 2, 3), c(2017, 2018, 2020)),
               c(NA, 1 * 2 / 1, NA))
})

test_that("fixed_prices reverses prev_year_prices", {
  set.seed(1)
  time <- 1995:2024
  cp <- cumprod(c(100, runif(29, 1.00, 1.08)))
  fp <- cumprod(c(80, runif(29, 0.97, 1.05)))

  pyp <- prev_year_prices(cp, fp, time)
  out <- fixed_prices(cp, pyp, time, ref_year = 2020)

  # the same volume series, rescaled to 2020 prices
  expect_equal(out, fp * cp[time == 2020] / fp[time == 2020])
  expect_equal(out[time == 2020], cp[time == 2020])
})

test_that("fixed_prices does not depend on the order of the rows", {
  time <- 2010:2020
  cp <- seq(100, 200, length.out = 11)
  pyp <- prev_year_prices(cp, seq(90, 210, length.out = 11), time)
  ord <- c(5, 1, 11, 2:4, 6:10)

  expect_equal(fixed_prices(cp[ord], pyp[ord], time[ord], 2015),
               fixed_prices(cp, pyp, time, 2015)[ord])
})

test_that("fixed_prices breaks the chain at gaps and missing values", {
  # 2019 missing: 2015-2018 cannot be linked to 2020-2022
  time <- c(2015:2018, 2020:2022)
  out <- fixed_prices(cp = 1:7, pyp = c(NA, 2, 3, 4, 5, 6, 7), time, ref_year = 2021)
  expect_equal(out, c(NA, NA, NA, NA, 5, 6, 7))

  # a missing previous year price breaks it in the same way
  out2 <- fixed_prices(cp = c(1, 2, 3), pyp = c(NA, NA, 4), 2014:2016, ref_year = 2015)
  expect_equal(out2, c(NA, 2, 4))
})

test_that("fixed_prices returns NA when the reference year value is missing", {
  expect_true(all(is.na(fixed_prices(c(NA, NA, NA), c(NA, 2, 4), 2014:2016, 2015))))
})

test_that("time may be dates as well as years", {
  time <- as.Date(c("2018-01-01", "2019-01-01", "2020-01-01"))
  cp <- c(100, 120, 150)
  fp <- c(90, 110, 150)

  expect_equal(prev_year_prices(cp, fp, time), prev_year_prices(cp, fp, 2018:2020))
  expect_equal(fixed_prices(cp, prev_year_prices(cp, fp, time), time, 2020),
               fixed_prices(cp, prev_year_prices(cp, fp, 2018:2020), 2018:2020, 2020))
})

test_that("bad input is rejected", {
  expect_error(prev_year_prices(1:3, 1:2, 2018:2020), "same length")
  expect_error(prev_year_prices(1:3, 1:3, c(2018, 2018, 2019)), "duplicated")
  expect_error(fixed_prices(1:3, 1:3, 2018:2020, ref_year = 2030), "not present")
  expect_error(fixed_prices(1:3, 1:3, 2018:2020, ref_year = c(2018, 2019)), "single year")
})
