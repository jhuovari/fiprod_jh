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
#'   \code{"inst/extdata"}. The directory is created if it does not exist.
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
                     dir = "inst/extdata", overwrite = FALSE) {
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
#' @param vintage If non \code{NULL} loads from vintage data. If data not
#'   present saves new vintage.
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

  if (!is.null(vintage)){
    filename_org <- filename
    filename <- paste0("v", as.character(vintage), "_", filename_org)
  }

  # Ensure .parquet extension (users may omit it)
  if (!grepl("\\.parquet$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".parquet")
  }

  # Try to infer package name if not provided
  if (is.null(package) || !nzchar(package)) {
    package <- .infer_pkg_name()
  }

  # 1) Look in installed/loaded package via system.file
  path <- ""
  if (nzchar(package)) {
    path <- system.file("extdata", filename, package = package, mustWork = FALSE)
  }

  # 2) Fallback: look in development path under inst/extdata
  if (!nzchar(path) || !file.exists(path)) {
    desc_path <- .find_description()
    if (!is.na(desc_path)) {
      pkg_root <- dirname(desc_path)
      dev_path <- file.path(pkg_root, "inst", "extdata", filename)
      if (file.exists(dev_path)) {
        path <- dev_path
      }
    }
  }

  if (!nzchar(path) || !file.exists(path) || !is.null(vintage)) {
    vdat <- load_dat(filename_org, package = package, must_work = TRUE)
    path <- save_dat(vdat, filename)
  } else if (!nzchar(path) || !file.exists(path)) {
    if (isTRUE(must_work)) {
      stop(
        "File not found: ",
        filename,
        ". Tried system.file('extdata', ...) for package '",
        package,
        "' and development path under inst/extdata."
      )
    } else {
      return(invisible(NULL))
    }
  }

  nanoparquet::read_parquet(path)
}
