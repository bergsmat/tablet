#' Drag-and-drop Descriptive Statistics
#'
#' Generate a table of descriptive statistics
#' by selecting columns from a file,
#' Currently supported formats include *.xpt, *.sas7bdat, and *.csv.
#' XPT formats and CSV formats are supported.
#' Launch the application using \code{mesa()} and use the interface
#' to select a data file. Then classify the
#' columns of interest to generate the corresponding
#' displays.
#'
#' Currently,
#'
#' * xpt files are read using the defaults for \code{\link[foreign]{read.xport}},
#'
#' * sas7bdat files are read using the defaults for \code{\link[haven]{read_sas}}, and
#'
#' * csv files are read using the defaults for \code{\link[csv]{as.csv}}.
#'
#' If a file in the same directory has a corresponding base name but a \code{.yaml}
#' extension, it is treated as metadata and an attempt is made to apply it
#' to the internal version of the data. This file will not be over-written,
#' but it WILL be constructed if missing.  You can hand-edit it to supply
#' metadata.  See \code{?yamlet}.
#'
#' The \code{\link[datasets]{mtcars}} datasets in the 'examples' volume is from \pkg{datasets}.
#'
#' @param launch.browser passed to \code{\link[shiny]{runApp}}
#' @param display.mode passed to \code{\link[shiny]{runApp}}
#' @param ... passed to \code{\link[shiny]{runApp}}
#' @export
#' @return used for side effects: launches shiny application

mesa <- function(launch.browser = TRUE, display.mode = 'normal', ...) {
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
    'knitr',
    'latexpdf',
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
  appDir <- system.file("shiny-examples", "mesa", package = "tablet")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `tablet`.", call. = FALSE)
  }

  shiny::runApp(
    appDir,
    launch.browser = launch.browser,
    display.mode = "normal",
    ...
  )
}
# https://deanattali.com/2015/04/21/r-package-shiny-app/
