make_ind <- function(seed, activity, geo = "FI", n = 20) {
  set.seed(seed)
  time <- seq_len(n) + 2004
  cp <- cumprod(c(100, runif(n - 1, 1.00, 1.08)))
  fp <- cumprod(c(70, runif(n - 1, 0.97, 1.06)))
  dplyr::bind_rows(
    tibble::tibble(time, geo, activity, measure = "GVA", price_base = "V", values = cp),
    tibble::tibble(time, geo, activity, measure = "GVA", price_base = "LR", values = fp),
    tibble::tibble(time, geo, activity, measure = "EMP", price_base = "_Z",
                   values = seq(50, 60, length.out = n))
  )
}

dat_ind <- dplyr::bind_rows(
  make_ind(1, "BTE"), make_ind(2, "F"), make_ind(3, "GTNXL"),
  make_ind(4, "BTE", "US"), make_ind(5, "F", "US"), make_ind(6, "GTNXL", "US")
)

key <- list(BTNXL = c("BTE", "F", "GTNXL"))

test_that("current price and additive series are summed", {
  out <- aggregate_activities(dat_ind, key, ref_year = 2020, append = FALSE)

  expected <- dat_ind |>
    dplyr::filter(price_base %in% c("V", "_Z")) |>
    dplyr::summarise(values = sum(values), .by = c(geo, measure, price_base, time))

  expect_equal(
    dplyr::arrange(dplyr::filter(out, price_base %in% c("V", "_Z")),
                   geo, measure, price_base, time)$values,
    dplyr::arrange(expected, geo, measure, price_base, time)$values
  )
})

test_that("volumes are chained through previous year's prices", {
  out <- aggregate_activities(dat_ind, key, ref_year = 2020, append = FALSE)

  # the same calculation written out by hand
  reference <- dat_ind |>
    dplyr::filter(measure == "GVA") |>
    tidyr::pivot_wider(names_from = price_base, values_from = values) |>
    dplyr::arrange(time) |>
    dplyr::mutate(Y = prev_year_prices(V, LR, time), .by = c(geo, activity)) |>
    dplyr::summarise(V = sum(V), Y = sum(Y), .by = c(geo, time)) |>
    dplyr::mutate(LR = fixed_prices(V, Y, time, 2020), .by = geo)

  got <- out |>
    dplyr::filter(measure == "GVA", price_base == "LR") |>
    dplyr::arrange(geo, time)

  expect_equal(got$values, dplyr::arrange(reference, geo, time)$LR)
})

test_that("the aggregate equals the current price sum in the reference year", {
  out <- aggregate_activities(dat_ind, key, ref_year = 2020, append = FALSE)
  ref <- dplyr::filter(out, measure == "GVA", time == 2020)
  expect_equal(ref$values[ref$price_base == "LR"], ref$values[ref$price_base == "V"])
})

test_that("published previous year's prices are used when available", {
  pyp <- dat_ind |>
    dplyr::filter(measure == "GVA") |>
    tidyr::pivot_wider(names_from = price_base, values_from = values) |>
    dplyr::arrange(time) |>
    dplyr::mutate(values = prev_year_prices(V, LR, time), .by = c(geo, activity)) |>
    dplyr::select(time, geo, activity, measure, values) |>
    dplyr::mutate(price_base = "Y")

  with_pyp <- aggregate_activities(dplyr::bind_rows(dat_ind, pyp), key,
                                   ref_year = 2020, append = FALSE)
  without <- aggregate_activities(dat_ind, key, ref_year = 2020, append = FALSE)

  expect_equal(
    dplyr::filter(with_pyp, price_base == "LR")$values,
    dplyr::filter(without, price_base == "LR")$values
  )
  # the supplied previous year's prices are aggregated and returned as well
  expect_true("Y" %in% with_pyp$price_base)
  expect_false("Y" %in% without$price_base)
})

test_that("an aggregate is NA whenever a component is missing", {
  hole <- dplyr::filter(dat_ind, !(activity == "F" & time == 2015))
  out <- aggregate_activities(hole, key, ref_year = 2020, append = FALSE)
  out <- dplyr::filter(out, geo == "FI", measure == "GVA")

  expect_true(is.na(out$values[out$price_base == "V" & out$time == 2015]))
  # the chain is broken, so everything before 2015 is NA too
  expect_true(all(is.na(out$values[out$price_base == "LR" & out$time <= 2015])))
  expect_false(anyNA(out$values[out$price_base == "LR" & out$time > 2015]))
})

test_that("an NA in a component propagates to the aggregate", {
  nav <- dplyr::mutate(dat_ind, values = ifelse(
    activity == "F" & time == 2015 & price_base == "V", NA_real_, values))
  out <- aggregate_activities(nav, key, ref_year = 2020, append = FALSE)
  expect_true(is.na(dplyr::filter(out, geo == "FI", price_base == "V", time == 2015)$values))
})

test_that("append replaces existing rows and is idempotent", {
  once <- aggregate_activities(dat_ind, key, ref_year = 2020)
  twice <- aggregate_activities(once, key, ref_year = 2020)

  expect_equal(names(once), names(dat_ind))
  expect_equal(nrow(once), nrow(dat_ind) + nrow(aggregate_activities(dat_ind, key, append = FALSE)))
  expect_equal(dplyr::arrange(twice, geo, activity, measure, price_base, time),
               dplyr::arrange(once, geo, activity, measure, price_base, time))
})

test_that("factor columns keep their levels and gain the new activities", {
  fac <- dplyr::mutate(dat_ind, dplyr::across(c(geo, activity, measure, price_base),
                                              as.factor))
  out <- aggregate_activities(fac, key, ref_year = 2020)

  expect_s3_class(out$activity, "factor")
  expect_equal(levels(out$activity), c("BTE", "F", "GTNXL", "BTNXL"))
  expect_equal(levels(out$price_base), levels(fac$price_base))
})

test_that("series that cannot be added are dropped with a message", {
  with_index <- dplyr::bind_rows(
    dat_ind, dplyr::mutate(dplyr::filter(dat_ind, price_base == "V"), price_base = "L"))

  expect_message(
    out <- aggregate_activities(with_index, key, ref_year = 2020, append = FALSE),
    "Not aggregated")
  expect_false("L" %in% out$price_base)
})

test_that("columns that encode the price base are rejected", {
  vid <- dplyr::mutate(dat_ind, var_id = paste(measure, price_base, sep = "-"))
  expect_error(aggregate_activities(vid, key), "identify the price base")
})

test_that("bad input is rejected", {
  expect_error(aggregate_activities(dat_ind, list(X = c("BTE", "NOPE"))), "not found")
  expect_error(aggregate_activities(dat_ind, list(X = c("BTE", "BTE"))), "Duplicated components")
  expect_error(aggregate_activities(dat_ind, "BTE"), "named list")
  expect_error(aggregate_activities(dplyr::select(dat_ind, -price_base), key),
               "Missing column")
})
