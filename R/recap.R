#' Recap Something
#' 
#' Recaps something.
#' Generic, with method \code{\link{recap.knitr_kable}}.
#' 
#' @export
#' @keywords internal
#' @family recap
#' @param x object of dispatch
#' @param ... passed arguments
recap <- function(x, ...)UseMethod('recap')

#' Recap knitr_kable.
#' 
#' Recaps knitr_kable.
#' Specifically, it replaces the first non-tabled caption
#' with multicolumn text.  The intent is to
#' prevent repeat bookmarks when generating pdf.
#' 
#' @export
#' @family recap
#' @param x object of dispatch
#' @param cols number of columns to span; guesses \code{ncol(x)} by default
#' @param pos position of text: 'l','c' (default), or 'r'
#' @param ... ignored


recap.knitr_kable <- function(x, cols = NULL, pos = 'c', ...){
  if(is.null(cols)) cols <- length(attributes(x)$kable_meta$align_vector)
  stopifnot(length(cols) == 1)
  if(cols == 0) cols <- 1
  stopifnot(length(pos) == 1)
  stopifnot(pos %in% c('l','c','r'))
  target <- '\\caption[]{'
  replacement <- paste0('\\multicolumn{',cols, '}{', pos, '}{')
  y <- sub(target, replacement, x, fixed = TRUE)
  y
}
