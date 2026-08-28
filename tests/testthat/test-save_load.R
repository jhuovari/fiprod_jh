## A throwaway package tree: <root>/DESCRIPTION and <root>/inst/extdata
fake_pkg <- function(name = "fakepkg") {
  root <- file.path(tempfile("pkg"), name)
  dir.create(file.path(root, "inst", "extdata"), recursive = TRUE)
  dir.create(file.path(root, "vignettes"))
  writeLines(paste0("Package: ", name), file.path(root, "DESCRIPTION"))
  normalizePath(root)
}

in_dir <- function(dir, code) {
  old <- setwd(dir)
  on.exit(setwd(old))
  force(code)
}

test_that("the package root is found from a subdirectory", {
  root <- fake_pkg()
  in_dir(file.path(root, "vignettes"), {
    expect_equal(normalizePath(.pkg_root()), root)
  })
})

test_that("data is written to the package root, not the working directory", {
  root <- fake_pkg()

  # this is the bug: rendering a vignette runs in vignettes/, and the old
  # relative default created vignettes/inst/extdata
  in_dir(file.path(root, "vignettes"), {
    expect_equal(normalizePath(.pkg_extdata_dir()),
                 normalizePath(file.path(root, "inst", "extdata")))
  })

  # and the same answer from the root itself
  in_dir(root, {
    expect_equal(normalizePath(.pkg_extdata_dir()),
                 normalizePath(file.path(root, "inst", "extdata")))
  })
})

test_that("without a DESCRIPTION the old relative default is kept", {
  plain <- tempfile("plain")
  dir.create(plain)
  in_dir(plain, {
    expect_true(is.na(.pkg_root()))
    expect_equal(.pkg_extdata_dir(), "inst/extdata")
  })
})

test_that("a vintage gets its own file name", {
  expect_equal(.vintage_filename("dat_gdp_main", 2026), "v2026_dat_gdp_main")
  expect_equal(.vintage_filename("dat_gdp_main.parquet", "2026q1"),
               "v2026q1_dat_gdp_main.parquet")
})

test_that("a data file is found from a subdirectory", {
  root <- fake_pkg()
  file.create(file.path(root, "inst", "extdata", "dat_x.parquet"))

  in_dir(file.path(root, "vignettes"), {
    expect_equal(normalizePath(.find_dat_path("dat_x.parquet", "")),
                 normalizePath(file.path(root, "inst", "extdata", "dat_x.parquet")))
    # a file that is not there gives "", not an error
    expect_equal(.find_dat_path("dat_missing.parquet", ""), "")
  })
})

test_that("an existing vintage is found and not written again", {
  root <- fake_pkg()
  # a vintage that has already been taken
  file.create(file.path(root, "inst", "extdata", "v2026_dat_x.parquet"))

  in_dir(file.path(root, "vignettes"), {
    # load_dat() builds the vintage name first and appends the extension after;
    # finding the file is what stops it from overwriting the frozen copy
    looked_for <- paste0(.vintage_filename("dat_x", 2026), ".parquet")
    expect_equal(looked_for, "v2026_dat_x.parquet")
    expect_equal(
      normalizePath(.find_dat_path(looked_for, "")),
      normalizePath(file.path(root, "inst", "extdata", "v2026_dat_x.parquet")))
  })
})

test_that("save_dat refuses to overwrite unless told to", {
  root <- fake_pkg()
  target <- file.path(root, "inst", "extdata", "dat_x.parquet")
  file.create(target)

  # the guard that would have fired on every vintage read before the fix
  expect_error(
    save_dat(data.frame(a = 1), "dat_x.parquet",
             dir = file.path(root, "inst", "extdata")),
    "already exists")
})
