## Saving report figures ------------------------------------------------------
##
## The productivity board's report is written elsewhere and takes the figures as
## files, one per year of publication. The defaults below are what the report
## wants; `set_fig_defaults()` is there so that a change of size, format or
## folder is a one line change in the vignette rather than an edit to every
## figure.

.fig_defaults <- list(
  dir    = "figures",
  year   = NULL,       # NULL: the current year
  width  = 13.5,
  height = 13.5,
  units  = "cm",
  dpi    = 300,
  device = "png"
)

#' Defaults for saving report figures
#'
#' The settings [save_fig()] uses when it is not told otherwise: the folder, the
#' year that names the subfolder, and the size, resolution and format of the
#' file.
#'
#' @param ... Named settings to change: `dir`, `year`, `width`, `height`,
#'   `units`, `dpi` or `device`. `year = NULL` means the current year.
#'
#' @return `set_fig_defaults()` returns the previous settings invisibly, so they
#'   can be restored. `fig_defaults()` returns the settings in force.
#'
#' @seealso [save_fig()]
#'
#' @examples
#' fig_defaults()
#'
#' old <- set_fig_defaults(dir = "kuviot", year = 2026)
#' fig_defaults()$dir
#'
#' set_fig_defaults(!!!old)
#'
#' @export
set_fig_defaults <- function(...) {
  new <- rlang::list2(...)
  if (length(new) && (is.null(names(new)) || any(!nzchar(names(new))))) {
    stop("All settings must be named, e.g. `set_fig_defaults(year = 2026)`.")
  }
  unknown <- setdiff(names(new), names(.fig_defaults))
  if (length(unknown)) {
    stop("Unknown setting(s): ", paste(unknown, collapse = ", "),
         ". Known: ", paste(names(.fig_defaults), collapse = ", "), ".")
  }

  old <- fig_defaults()
  # a NULL is a value here (year = NULL means "this year"), so modifyList,
  # which would drop it, is not what we want
  set <- getOption("fiprod.fig", list())
  for (nm in names(new)) set[nm] <- list(new[[nm]])
  options(fiprod.fig = set)

  invisible(old)
}

#' @rdname set_fig_defaults
#' @export
fig_defaults <- function() {
  set <- getOption("fiprod.fig", list())
  out <- .fig_defaults
  for (nm in intersect(names(set), names(out))) out[nm] <- list(set[[nm]])
  out
}

#' Save a report figure
#'
#' Writes a figure to `<dir>/<year>/<name>.<device>`, creating the folder if it
#' is not there. The year is the year of publication, so that the figures of
#' successive reports stay side by side instead of overwriting each other.
#'
#' The plot is returned, so a chunk that ends in `save_fig()` both writes the
#' file and shows the figure:
#'
#' ```
#' dat |>
#'   ggplot2::ggplot(...) |>
#'   save_fig("bkt-per-capita")
#' ```
#'
#' @param plot A plot object, normally a `ggplot`.
#' @param name File name without the extension.
#' @param ... Settings for this call only, overriding [fig_defaults()]: `dir`,
#'   `year`, `width`, `height`, `units`, `dpi`, `device`.
#'
#' @return `plot`, so that the figure is still drawn.
#'
#' @seealso [set_fig_defaults()] to change the defaults for every figure at
#'   once.
#'
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
#' save_fig(p, "wt-mpg", dir = tempdir())
#' }
#'
#' @export
save_fig <- function(plot, name, ...) {
  if (!rlang::is_string(name) || !nzchar(name)) {
    stop("`name` must be a single non empty string.")
  }
  if (basename(name) != name) {
    stop("`name` is a file name, not a path: ", name,
         ". Use the `dir` setting for the folder.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Saving a figure needs the ggplot2 package.")
  }

  opts <- fig_defaults()
  new <- rlang::list2(...)
  unknown <- setdiff(names(new), names(opts))
  if (length(unknown)) {
    stop("Unknown setting(s): ", paste(unknown, collapse = ", "), ".")
  }
  for (nm in names(new)) opts[nm] <- list(new[[nm]])

  year <- opts$year %||% format(Sys.Date(), "%Y")
  dir <- file.path(opts$dir, as.character(year))
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  path <- file.path(dir, paste0(name, ".", opts$device))
  ggplot2::ggsave(path, plot = plot,
                  width = opts$width, height = opts$height, units = opts$units,
                  dpi = opts$dpi, device = opts$device)

  plot
}
