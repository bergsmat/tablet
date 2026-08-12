#' CDISC Listing Generator
#'
#' Launch \code{listado}, a companion Shiny application to \code{\link{mesa}}
#' for building CDISC-style long listings. Point the app at a data file
#' (\code{*.xpt}, \code{*.sas7bdat}, or \code{*.csv}), optionally choose up to
#' two facet columns to break the listing into headed sections, drag columns
#' into one or more "bins" to collapse them into single display columns,
#' adjust column widths, set fixed header/footer text, and generate a
#' \code{.qmd} file that renders the listing with \code{kableExtra::kbl()}.
#' By default the generated \code{.qmd} is also rendered to PDF using the
#' \code{quarto} command line tool (must be on \code{PATH}).
#'
#' Session configuration (facets, bins, widths, headers/footers, output
#' directory) can be saved to and loaded from a sibling \code{.conf} file,
#' matched to the data file by basename -- e.g. choosing \code{adae.xpt}
#' loads \code{adae.conf} if it exists, and creates a default one if it does
#' not.
#'
#' The \code{adae.xpt} dataset under 'examples/' is a subset of the CDISC
#' pilot ADAE dataset.
#'
#' @param launch.browser passed to \code{\link[shiny]{runApp}}
#' @param display.mode passed to \code{\link[shiny]{runApp}}
#' @param ... passed to \code{\link[shiny]{runApp}}
#' @export
#' @return used for side effects: launches shiny application

listado <- function(launch.browser = TRUE, display.mode = 'normal', ...) {
  dependencies <- c(
    'shiny',
    'shinyFiles',
    'fs',
    'haven',
    'tablet',
    'dplyr',
    'magrittr',
    'yamlet',
    'yaml',
    'sortable',
    'kableExtra',
    'tools',
    'csv',
    'foreign'
  )
  have <- sapply(dependencies, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  need <- dependencies[!have]
  if(length(need)){
    message('please install dependencies: ', paste(need, collapse = ', '))
    return(NULL)
  }
  if(Sys.which('quarto') == ''){
    message(
      'quarto was not found on PATH; listado will still run, ',
      'but "Render to PDF" will fail until quarto is installed and on PATH.'
    )
  }
  appDir <- system.file("shiny-examples", "listado", package = "tablet")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `tablet`.", call. = FALSE)
  }

  shiny::runApp(
    appDir,
    launch.browser = launch.browser,
    display.mode = display.mode,
    ...
  )
}
