#' Save a dataset to inst/extdata as Parquet (for package build time)
#'
#' Writes a data frame to the package's \code{inst/extdata/} directory so that,
#' after installation, the file will be available under \code{system.file("extdata", ...)}.
#' Intended to be used during development (not at runtime after installation).
#'
#' @param x A \code{data.frame} (or tibble) to be saved.
#' @param filename File name to write. Defaults to the name of \code{x} with a
#'   \code{.parquet} extension.
#' @param dir Directory where to write during development. Defaults to
#'   \code{inst/extdata} under the package root, found by walking up from the
#'   working directory to \code{DESCRIPTION}, so that writing from a
#'   subdirectory such as \code{vignettes/} still lands in the right place. The
#'   directory is created if it does not exist.
#' @param overwrite Logical, overwrite an existing file (default \code{FALSE}).
#'
#' @return (Invisibly) the path to the written file.
#' @examples
#' \dontrun{
#' # During development of the package:
#' save_dat(mtcars) # writes inst/extdata/mtcars.parquet
#' }
#' @export
save_dat <- function(x, filename = deparse(substitute(x)),
                     dir = .pkg_extdata_dir(), overwrite = FALSE) {
  # Validate inputs
  if (!is.data.frame(x)) {
    stop("'x' must be a data.frame (or tibble).")
  }

  # Ensure .parquet extension
  if (!grepl("\\.parquet$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".parquet")
  }

  # Ensure target directory exists
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Full path
  path <- file.path(dir, filename)

  # Overwrite check
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop("Target file already exists: ", path,
         " (set 'overwrite = TRUE' to replace it).")
  }

  # Write Parquet using nanoparquet
  nanoparquet::write_parquet(x, path)

  invisible(path)
}

# ---- helpers (internal) -----------------------------------------------------

#' @keywords internal
.infer_pkg_name <- function() {
  # Try the current namespace (works when installed or load_all'ed)
  nm <- tryCatch(utils::packageName(), error = function(e) NULL)
  if (!is.null(nm) && nzchar(nm)) return(nm)

  # Fallback: search upwards for DESCRIPTION and read Package: field
  desc_path <- .find_description()
  if (!is.na(desc_path)) {
    dcf <- tryCatch(utils::read.dcf(desc_path, fields = "Package"),
                    error = function(e) NULL)
    if (!is.null(dcf) && length(dcf) > 0) {
      return(as.character(dcf[1]))
    }
  }
  ""
}

#' Package root, found by walking up to DESCRIPTION
#'
#' @param start Directory to start from.
#' @return The package root, or `NA_character_` if no DESCRIPTION is found.
#' @keywords internal
.pkg_root <- function(start = getwd()) {
  desc <- .find_description(start)
  if (is.na(desc)) NA_character_ else dirname(desc)
}

#' Where package data is written during development
#'
#' Resolved from the package root rather than from the working directory, so
#' that a call from `vignettes/` does not create `vignettes/inst/extdata`.
#'
#' @return A path to `inst/extdata`.
#' @keywords internal
.pkg_extdata_dir <- function() {
  root <- .pkg_root()
  if (is.na(root)) "inst/extdata" else file.path(root, "inst", "extdata")
}

#' File name of a vintage copy
#'
#' @param filename Base file name.
#' @param vintage Vintage label, e.g. a year.
#' @keywords internal
.vintage_filename <- function(filename, vintage) {
  paste0("v", as.character(vintage), "_", filename)
}

#' Locate a data file, installed or in the source tree
#'
#' @param filename File name with extension.
#' @param package Package name, possibly `""`.
#' @return The path, or `""` when the file is not there.
#' @keywords internal
.find_dat_path <- function(filename, package) {
  if (nzchar(package)) {
    path <- system.file("extdata", filename, package = package, mustWork = FALSE)
    if (nzchar(path) && file.exists(path)) return(path)
  }
  root <- .pkg_root()
  if (!is.na(root)) {
    dev_path <- file.path(root, "inst", "extdata", filename)
    if (file.exists(dev_path)) return(dev_path)
  }
  ""
}

#' @keywords internal
.find_description <- function(start = getwd()) {
  cur  <- normalizePath(start, winslash = "/", mustWork = FALSE)
  last <- ""
  while (!identical(cur, last)) {
    f <- file.path(cur, "DESCRIPTION")
    if (file.exists(f)) return(f)
    last <- cur
    cur  <- dirname(cur)
  }
  NA_character_
}

#' Load a dataset from extdata (installed package) as Parquet
#'
#' Reads a Parquet file stored under the package's \code{inst/extdata/} at build time.
#' At runtime, the function first tries \code{system.file("extdata", ...)} (works for installed
#' packages and with \code{devtools::load_all()}), and if not found, falls back to the
#' development path \code{inst/extdata/} under the package root (found via \code{DESCRIPTION}).
#'
#' @param filename File name to read (with or without \code{.parquet}).
#' @param package Package name where the file resides. Defaults to \code{NULL},
#'   in which case the function tries to infer the package name; if that fails,
#'   it will still try the development path under \code{inst/extdata/}.
#' @param must_work If \code{TRUE} (default), error if the file cannot be located.
#' @param vintage If non \code{NULL}, read a frozen copy of the data instead of
#'   the live file. The copy is written the first time a vintage is asked for,
#'   as \code{v<vintage>_<filename>.parquet} next to the other data, and read
#'   back unchanged after that, so a report keeps the numbers it was written
#'   with even when the underlying data is updated.
#'
#' @return A \code{data.frame} loaded from the Parquet file.
#' @examples
#' \dontrun{
#' # After installation or devtools::load_all():
#' df <- load_dat("mtcars.parquet")
#'
#' # During development when not using load_all(), the function can still
#' # locate inst/extdata/ by walking up to DESCRIPTION:
#' df <- load_dat("mtcars.parquet")
#' }
#' @export
load_dat <- function(filename, package = NULL, must_work = TRUE, vintage = NULL) {
  if (missing(filename) || !nzchar(filename)) {
    stop("'filename' must be provided (e.g., 'mydata.parquet').")
  }

  filename_org <- filename
  if (!is.null(vintage)) {
    filename <- .vintage_filename(filename_org, vintage)
  }

  # Ensure .parquet extension (users may omit it)
  if (!grepl("\\.parquet$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".parquet")
  }

  # Try to infer package name if not provided
  if (is.null(package) || !nzchar(package)) {
    package <- .infer_pkg_name()
  }

  path <- .find_dat_path(filename, package)

  # A vintage is written once and then read back on every later call, which is
  # the point: it is a copy that does not follow the live data.
  if (!nzchar(path) && !is.null(vintage)) {
    message("Writing vintage ", vintage, " of ", filename_org, ".")
    vdat <- load_dat(filename_org, package = package, must_work = TRUE)
    path <- save_dat(vdat, filename)
  }

  if (!nzchar(path)) {
    if (isTRUE(must_work)) {
      stop("File not found: ", filename,
           ". Tried system.file('extdata', ...) for package '", package,
           "' and development path under inst/extdata.")
    }
    return(invisible(NULL))
  }

  nanoparquet::read_parquet(path)
}
