eurostat <- tidyr::expand_grid(geo = c("FI", "SE", "DE", "NO"), time = 2018:2020) |>
  dplyr::mutate(values = 1)
oecd <- tidyr::expand_grid(geo = c("FI", "SE", "US", "JP"), time = 2018:2020) |>
  dplyr::mutate(values = 2)

test_that("each country comes from the first source that has it", {
  out <- combine_geo_sources(eurostat = eurostat, oecd = oecd)

  expect_equal(sort(unique(out$geo)), c("DE", "FI", "JP", "NO", "SE", "US"))
  expect_equal(sort(unique(out$geo[out$source == "eurostat"])),
               c("DE", "FI", "NO", "SE"))
  expect_equal(sort(unique(out$geo[out$source == "oecd"])), c("JP", "US"))
  # every country appears once only
  expect_equal(nrow(dplyr::distinct(out, geo, time)), nrow(out))
})

test_that("geos restricts what a source contributes", {
  out <- combine_geo_sources(eurostat = eurostat, oecd = oecd,
                             geos = list(eurostat = c("FI", "SE")))

  expect_equal(sort(unique(out$geo[out$source == "eurostat"])), c("FI", "SE"))
  expect_equal(sort(unique(out$geo[out$source == "oecd"])), c("JP", "US"))
  # DE and NO are in neither, because Eurostat was restricted and OECD lacks them
  expect_false(any(c("DE", "NO") %in% out$geo))
})

test_that("source_col can be left out", {
  out <- combine_geo_sources(eurostat = eurostat, oecd = oecd, source_col = NULL)
  expect_equal(names(out), names(eurostat))
})

test_that("only shared columns are kept", {
  expect_message(
    out <- combine_geo_sources(eurostat = dplyr::mutate(eurostat, extra = "x"),
                               oecd = oecd),
    "Dropping column")
  expect_false("extra" %in% names(out))
})

test_that("bad input is rejected", {
  expect_error(combine_geo_sources(eurostat, oecd), "must be named")
  expect_error(combine_geo_sources(a = eurostat, b = oecd, geos = list(zzz = "FI")),
               "unknown")
  expect_error(combine_geo_sources(a = eurostat, b = dplyr::mutate(oecd, source = "x")),
               "already a column")
  expect_error(combine_geo_sources(a = dplyr::select(eurostat, -geo), b = oecd),
               "No `geo` column")
})

test_that("compare_sources finds a difference in scale", {
  x <- tibble::tibble(geo = "FI", time = 2010:2020,
                      values = seq(100, 200, length.out = 11))
  y <- dplyr::mutate(x, values = values / 1000)

  out <- compare_sources(x, y, by = "geo")
  expect_equal(out$n, 11L)
  expect_equal(out$ratio_median, 1000)
  expect_equal(out$ratio_min, 1000)
  expect_equal(out$growth_cor, 1)
})

test_that("compare_sources finds series that move differently", {
  set.seed(7)
  x <- tibble::tibble(geo = "FI", time = 2010:2020,
                      values = cumprod(c(100, runif(10, 1, 1.05))))
  y <- dplyr::mutate(x, values = values * runif(11, 0.8, 1.2))

  expect_lt(compare_sources(x, y, by = "geo")$growth_cor, 0.9)
})
