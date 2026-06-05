globalVariables(c('c1','c2','label'))
  
#' Import and Export Tablet
#'
#' Imports or exports tablet as comma-separated variable.
#' Generic, with methods that extend \code{\link[yamlet]{io_csv}}.

#' @param x object
#' @param ... passed arguments
#' @export
#' @importFrom yamlet io_csv
#' @return See methods.
#' @family io
#' @examples
#' library(yamlet)
#' library(boot)
#' library(dplyr)
#' library(magrittr)
#' library(tablet)
#' library(kableExtra)
#' melanoma %<>% redecorate('
#' time: [ Time, day ]
#' status: [ Patient Status, [ Died^1: 1, Alive^2: 2, Unrelated: 3 ]]
#' sex: [ Sex^3, [ Male: 1, Female^4: 0 ]]
#' age: [ Age, year ]
#' year: [ Year of Operation ]
#' ulcer: [ Ulceration, [ Present: 1, Absent: 0 ]]
#' thickness: [ Tumor Thickness, mm ]
#' ')

#' tbl <- melanoma %>%
#'   select(-time, -year) %>%
#'   group_by(status, ulcer) %>%
#'   enscript %>%
#'   tablet(Missing~NULL)
#' path <- tbl %>% io_tablet(tempfile(fileext = '.csv'))
#' # alternatively:
#' path <- tbl %>% io_csv(tempfile(fileext = '.csv'))
#' tab <- path %>% io_tablet
#' 
#' tbl %>% 
#' as_kable(booktabs = TRUE) %>%
#' kable_styling(latex_options = 'scale_down')
#' 
#' tab %>% 
#' as_kable(booktabs = TRUE) %>%
#' kable_styling(latex_options = 'scale_down')


io_tablet <- function(x, ...)UseMethod('io_tablet')

#' Import Tablet
#'
#' Imports a tablet.
#' A wrapper for \code{\link[yamlet]{io_csv.character}} that also
#' reads associated yamlet metadata, if present, and applies it
#' as attributes.
#'
#' @param x character file path
#' @param na.strings passed to \code{\link[yamlet]{io_csv.character}}
#' @param strip.white passed to \code{\link[yamlet]{io_csv.character}}
#' @param ... passed to \code{\link[yamlet]{io_csv.character}}
#' @export
#' @keywords internal
#' @family io
#' @family interface
#' @return data.frame
#' @examples
#' example(io_tablet)
io_tablet.character <- function(
    x, 
    na.strings = 'NA', 
    strip.white = FALSE, 
    ...
  ){
    y <- as_tablet(
      io_csv(
        x, 
        na.strings = na.strings,  
        strip.white = strip.white,
        ...
      )
    )
    y <- modify(y, -c(c1, c2), label = gsub('\\\\n', '\n', label))
    y
  }

#' Export Dataframe as Tablet
#'
#' Exports a tablet as comma-separated variable,
#' as well as a yamlet version of its decorations.
#' A wrapper for \code{\link[yamlet]{io_csv.data.frame}}.
#'
#' @param x tablet
#' @param file passed to \code{\link[yamlet]{io_csv}}
#' @param ...  passed to \code{\link[yamlet]{io_csv}}
#' @export
#' @keywords internal
#' @family io
#' @family interface
#' @return invisible(file)
#' @examples
#' example(io_tablet)
io_tablet.data.frame <- function(x, file = '', ...){
  y <- modify(x, -c(c1, c2), label = gsub('\n', '\\\\n', label))
  # even though we likely got here from NextMethod(),
  # y still is class 'tablet' and will invoke io_csv.tablet
  # which contains a warning probably not needed here.
  # we strip.
  class(y) <- setdiff(class(y), 'tablet')
  io_csv(y, file = file, ...)
}

#' Export Tablet
#'
#' Exports a tablet as comma-separated variable,
#' as well as a yamlet version of its decorations.
#' Forwards to next method.
#'
#' @param x tablet
#' @param file passed to \code{\link[yamlet]{io_csv}}
#' @param ...  passed to \code{\link[yamlet]{io_csv}}
#' @export
#' @keywords internal
#' @family io
#' @family interface
#' @return invisible(file)
#' @examples
#' example(io_tablet)
io_tablet.tablet <- function(x, file = '', ...)NextMethod()


#' Export Tablet as CSV
#'
#' Exports a tablet as comma-separated variable,
#' by means of \code{io_csv}.  Reminds the user
#' to prefer \code{io_tablet} for lossless recovery.
#'
#' @param x tablet
#' @param file passed to \code{\link{io_tablet}}
#' @param ...  passed to \code{\link{io_tablet}}
#' @export
#' @importFrom yamlet io_csv modify
#' @keywords internal
#' @family io
#' @family interface
#' @return invisible(file)
#' @examples
#' example(io_tablet)
#' 
io_csv.tablet <- function(x, file = '', ...){
  if(file != '') message('import with io_tablet(.../',basename(file), ')')
  NextMethod()
}









