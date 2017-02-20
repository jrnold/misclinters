#' Lint a directory
#'
#' @param path The path to the directory to lint.
#' @param recursive Recursively search directories
#' @param relative_path	If \code{TRUE}, file paths are printed using
#'   their path relative to the package base directory.
#'   If \code{FALSE}, use the full absolute path.
#' @param ... Arguments passed to \code{link[lintr]{lint}}
#' @importFrom rex rex re_substitutes
#' @export
lint_dir <- function(path = ".", recursive = FALSE, relative_path = TRUE,
                     ...) {
  one_of <- NULL
  # Avoid global variable
  pattern <- "(\\.([Rr](md|tex|nw|html|rst))$|^\\.Rprofile$))"
  read_settings(path)
  on.exit(clear_settings, add = TRUE)
  names(settings$exclusions) <-
    normalizePath(file.path(path, names(settings$exclusions)))
  exclusions = force(settings$exclusions)
  files <- dir(path, pattern = pattern,
               recursive = TRUE,
               full.names = TRUE)
  files <- normalizePath(files)
  lints <- flatten_lints(lapply(files, function(file) {
    if (interactive()) {
      message(".", appendLF = FALSE)
    }
    lint(file, ..., parse_settings = FALSE, exclusions = exclusions)
  }))
  if (interactive()) {
    message()
  }
  lints <- reorder_lints(lints)
  if (relative_path == TRUE) {
    lints[] <- lapply(lints, function(x) {
      x$filename <- re_substitutes(x$filename, rex(normalizePath(path),
                                                   one_of("/", "\\")), "")
      x
    })
    attr(lints, "path") <- path
  }
  class(lints) <- "lints"
  lints
}

#' Lint R projects or packages
#'
#' Lint R projects or packages
#'
#' @param criterion A criterion, will be coerced using \code{\link[rprojroot]{as.root_criterion}()}.
#' @param path Directory to start searching for the root directory of the
#'   project or
#' @param ... Arguments passed to \code{\link{lint_dir}}
#' @export
lint_root_dir <- function(criterion, ..., path = ".") {
  pkg_path <- rprojroot::find_root(criterion, path = path)
  lint_dir(pkg_path, ...)
}

#' @rdname lint_root_dir
#' @export
lint_rstudio_project <- function(criterion, ..., path = ".") {
  lint_root_dir(rprojroot::is_rstudio_project, ..., path = path)
}

#' @rdname lint_root_dir
#' @export
lint_r_package <- function(criterion, ..., path = ".") {
  lint_root_dir(rprojroot::is_r_package, ..., path = path)
}
