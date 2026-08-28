with_clean_defaults <- function(code) {
  old <- getOption("fiprod.fig")
  on.exit(options(fiprod.fig = old))
  options(fiprod.fig = NULL)
  force(code)
}

a_plot <- function() {
  ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
}

test_that("the report's own defaults are what comes out of the box", {
  with_clean_defaults({
    d <- fig_defaults()
    expect_equal(d$width, 13.5)
    expect_equal(d$height, 8.5)
    expect_equal(d$units, "cm")
    expect_equal(d$device, "png")
    expect_null(d$year)
  })
})

test_that("a default can be changed and put back", {
  with_clean_defaults({
    old <- set_fig_defaults(year = 2026, width = 20)
    expect_equal(fig_defaults()$year, 2026)
    expect_equal(fig_defaults()$width, 20)
    # untouched settings keep their value
    expect_equal(fig_defaults()$units, "cm")

    set_fig_defaults(!!!old)
    expect_null(fig_defaults()$year)
    expect_equal(fig_defaults()$width, 13.5)
  })
})

test_that("a year can be set back to the current one", {
  with_clean_defaults({
    set_fig_defaults(year = 2026)
    # NULL is a value here, not a request to leave the setting alone
    set_fig_defaults(year = NULL)
    expect_null(fig_defaults()$year)
  })
})

test_that("a misspelled setting is refused", {
  with_clean_defaults({
    expect_error(set_fig_defaults(with = 20), "Unknown setting")
    expect_error(set_fig_defaults(2026), "must be named")
  })
})

test_that("the file lands in a folder named by the year", {
  skip_if_not_installed("ggplot2")
  with_clean_defaults({
    root <- tempfile("figs")
    save_fig(a_plot(), "kuvio", dir = root, year = 2026)
    expect_true(file.exists(file.path(root, "2026", "kuvio.png")))
  })
})

test_that("the current year is used when none is given", {
  skip_if_not_installed("ggplot2")
  with_clean_defaults({
    root <- tempfile("figs")
    save_fig(a_plot(), "kuvio", dir = root)
    expect_true(file.exists(file.path(root, format(Sys.Date(), "%Y"), "kuvio.png")))
  })
})

test_that("the defaults are used and can be overridden for one figure", {
  skip_if_not_installed("ggplot2")
  with_clean_defaults({
    root <- tempfile("figs")
    set_fig_defaults(dir = root, year = 2026)

    save_fig(a_plot(), "oletus")
    save_fig(a_plot(), "poikkeus", year = 2025)

    expect_true(file.exists(file.path(root, "2026", "oletus.png")))
    expect_true(file.exists(file.path(root, "2025", "poikkeus.png")))
  })
})

test_that("the size asked for is the size written", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("png")
  with_clean_defaults({
    root <- tempfile("figs")
    save_fig(a_plot(), "iso", dir = root, year = 2026, dpi = 100)

    # 13.5 cm at 100 dpi is 13.5 / 2.54 * 100 pixels each way
    px <- round(13.5 / 2.54 * 100)
    dim <- dim(png::readPNG(file.path(root, "2026", "iso.png")))
    expect_equal(dim[1], px, tolerance = 2)
    expect_equal(dim[2], px, tolerance = 2)
  })
})

test_that("the plot comes back so the chunk still draws it", {
  skip_if_not_installed("ggplot2")
  with_clean_defaults({
    p <- a_plot()
    expect_identical(save_fig(p, "kuvio", dir = tempfile("figs")), p)
  })
})

test_that("a name that is a path is refused", {
  with_clean_defaults({
    expect_error(save_fig(a_plot(), "2026/kuvio"), "not a path")
    expect_error(save_fig(a_plot(), ""), "non empty")
    expect_error(save_fig(a_plot(), "kuvio", widht = 10), "Unknown setting")
  })
})
