test_that("ind_ulc is cost over output, indexed", {
  cost   <- c(100, 104, 110)
  output <- c(200, 205, 208)
  time   <- 2018:2020

  raw <- cost / output
  expect_equal(ind_ulc(cost, output, time = time, baseyear = 2020),
               100 * raw / raw[3])
  expect_equal(ind_ulc(cost, output, time = time, baseyear = 2020)[3], 100)
})

test_that("the labour inputs enter as per head measures", {
  cost   <- c(100, 104, 110)
  output <- c(200, 205, 208)
  emp    <- c(60, 60, 61)
  sal    <- c(50, 50, 51)
  time   <- 2018:2020

  raw <- (cost / sal) / (output / emp)
  expect_equal(ind_ulc(cost, output, sal, emp, time, 2020), 100 * raw / raw[3])

  # equal inputs cancel out
  expect_equal(ind_ulc(cost, output, emp, emp, time, 2020),
               ind_ulc(cost, output, time = time, baseyear = 2020))
})

test_that("unit labour cost decomposes into compensation over productivity", {
  set.seed(3)
  time   <- 2000:2020
  output <- cumprod(c(1000, runif(20, 1.00, 1.04)))
  emp    <- cumprod(c(500, runif(20, 0.99, 1.01)))
  sal    <- 0.87 * emp
  cost   <- cumprod(c(470, runif(20, 1.00, 1.05)))

  ulc <- ind_ulc(cost, output, sal, emp, time, 2010)
  compensation <- rebase_index(cost / sal, time, 2010)
  productivity <- rebase_index(output / emp, time, 2010)

  # this identity is what the board's decomposition figure rests on
  expect_equal(ulc, 100 * compensation / productivity)
})

test_that("gdp_trading_gain adds the purchasing power of exports", {
  # import prices unchanged: the adjusted volume equals GDP plus the gain
  expect_equal(
    gdp_trading_gain(gdp = 100, exports = 40, exports_cp = 44,
                     imports = 30, imports_cp = 30),
    100 - 40 + 44
  )

  # exports and imports priced alike and volumes equal to values: no adjustment
  expect_equal(
    gdp_trading_gain(gdp = 100, exports = 40, exports_cp = 40,
                     imports = 30, imports_cp = 30),
    100
  )
})

test_that("a better terms of trade raises the adjusted volume", {
  base <- gdp_trading_gain(100, exports = 40, exports_cp = 40,
                           imports = 30, imports_cp = 30)
  better <- gdp_trading_gain(100, exports = 40, exports_cp = 44,
                             imports = 30, imports_cp = 30)
  worse <- gdp_trading_gain(100, exports = 40, exports_cp = 36,
                            imports = 30, imports_cp = 30)

  expect_gt(better, base)
  expect_lt(worse, base)
})

test_that("the adjustment responds to the import deflator", {
  # import prices up 10 %: the same nominal exports buy less
  expect_equal(
    gdp_trading_gain(100, exports = 40, exports_cp = 44,
                     imports = 30, imports_cp = 33),
    100 - 40 + 44 / 1.1
  )
})

test_that("terms of trade adjusted unit labour costs use the adjusted output", {
  time <- 2018:2020
  gdp  <- c(100, 102, 104)
  adj  <- gdp_trading_gain(gdp, exports = c(40, 41, 42),
                           exports_cp = c(40, 43, 46),
                           imports = c(30, 31, 32),
                           imports_cp = c(30, 31, 32))
  cost <- c(47, 49, 51)

  # exports got dearer, so the adjusted output is larger and the cost per unit
  # of it smaller than the unadjusted one in the later years
  expect_true(all(adj[-1] > gdp[-1]))
  expect_lt(ind_ulc(cost, adj, time = time, baseyear = 2018)[3],
            ind_ulc(cost, gdp, time = time, baseyear = 2018)[3])
})
