#' Convert to tablette
#' 
#' Converts to 'tablette'.  Generic,
#' with method \code{\link{tablette.tablet}}.
#' 
#' @export
#' @keywords internal
#' @family tablette
#' @param x object of dispatch
#' @param ... passed arguments

tablette <- function(x, ...)UseMethod('tablette')

#' Convert tablet to tablette
#' 
#' Converts 'tablet' to 'tablette'.
#' I.e., makes a pretty data.frame that emulates
#' the layout expected from \code{\link{as_kable}}.
#'
#' @export
#' @family tablette
#' @param x object of dispatch
#' @param ... passed arguments
#' @return tablette data.frame

#' @examples
#' library(boot)
#' library(dplyr)
#' library(magrittr)
#' rm(melanoma)
#' melanoma %<>% select(-time, -year) 
#' melanoma %<>% decorate('
#' time:      [ Survival Time Since Operation, day               ]
#' status:    [ End of Study Patient Status,
#'            [ Alive: 2, Melanoma Death: 1, Unrelated Death: 3 ]]
#' sex:       [ Sex, [ Male: 1, Female: 0                       ]]
#' age:       [ Age at Time of Operation, year                   ]
#' thickness: [ Tumor Thickness, mm                              ]
#' ulcer:     [ Ulceration, [ Absent: 0, Present: 1             ]]
#' ')
#' melanoma %<>% resolve
#' melanoma %<>% group_by(status, ulcer) 
#' melanoma %<>% tablet 
#' melanoma %>% as_kable
#' melanoma %>% tablette
#' melanoma %>% tablette %>% tablet
#'   

tablette.tablet <- function(x, ...){
  x
}


#' Convert tablette to tablet
#' 
#' Converts tablette to tablet.
#' Intends to be the approximate inverse of \code{\link{tablette.tablet}}. See also \code{\link{as_tablette.data.frame}}.
#' 
#' @export
#' @family tablette
#' @family tablet
#' @return tablet data.frame
#' @param x tablette
#' @param ... passed arguments
#' @examples
#' example(tablette.tablet)
tablet.tablette <- function(x, ...){
  x
}

#' Coerce to tablette
#' 
#' Coerces to tablette.
#' Generic, with method \code{\link{as_tablette.data.frame}}.
#' 
#' @export
#' @keywords internal
#' @family tablette
#' @return tablette
#' @param x object of dispatch
#' @param ... passed arguments
as_tablette <- function(x, ...)UseMethod('as_tablette')

#' Coerce data.frame to tablette
#' 
#' Coerces data.frame to tablette.  Checks format and assigns the class.  See Details.
#' 
#' A tablette is a special case of data.frame with grouped rows and columns.
#'   * There is always one level of row groups.
#'   * There can be any number of column groups, including zero.
#'   * All columns inherit factor or character.
#'   * The first column is anonymous, with empty elements that represent the last non-empty value. It can be class 'latex' or 'character'.
#'   * Leading blank elements of first column correspond to header rows.
#'   * The second column is anonymous and represents group-specific property names. It is populated always and only where column 1 is not.
#'   * All other columns represent group-specific property values; elements before the first non-empty group value represent nested headers.
#'   * Header values may be repeated.
#'   * Header values may be empty, representing the last non-empty value to the left. 
#' 
#' 
#' @export
#' @family tablette
#' @return tablette
#' @param x data.frame
#' @param ... passed arguments
as_tablette.data.frame <- function(x, ...){
  stopifnot(length(x) >= 2)
  stopifnot(names(x)[[1]] == '')
  stopifnot(names(x)[[2]] == '')
  class(x) <- union('tablette', class(x))
  x
}

