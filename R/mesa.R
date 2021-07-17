#' Drag-and-drop Descriptive Statistics
#'
#' Generate a table of descriptive statistics
#' by selecting columns from a file.
#' Currently supported formats include *.xpt, *.sas7bdat, and *.csv.
#' Launch the application using \code{mesa()} and use the interface
#' to select a data file, such as 'mtcars.xpt' under 'examples/')
#' (or select configuration file 'mtcars.conf' under 'examples/').
#' Then classify the
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
#' metadata.  See \code{?yamlet} for format; see the Metadata tab for an easy interface.
#'
#' This is a metadata-driven application.  Columns in the data that are *not*
#' in the metadata will be ignored, and columns in the metadata that are *not*
#' in the data will be constructed (maybe *all* of them).
#'
#' The \code{\link[datasets]{mtcars}} datasets in the 'examples' volume is from \pkg{datasets}.
#'
#' @param launch.browser passed to \code{\link[shiny]{runApp}}
#' @param display.mode passed to \code{\link[shiny]{runApp}}
#' @param ... passed to \code{\link[shiny]{runApp}}
#' @importFrom spork as_html as_latex as_spork
#' @importFrom DT dataTableOutput
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
