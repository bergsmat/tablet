#' Coerce to Xtable
#'
#' Coerces to xtable output class.  Generic,
#' with method \code{\link{as_xtable.tablet}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @return see methods
#' @keywords internal
#' @examples
#' example(classifiers)
#' as_xtable(tablet(groupwise(x)))
as_xtable <- function(x, ...)UseMethod('as_xtable')


#' Coerce Tablet to Xtable
#'
#' Renders a tablet as xtable.  Calls \code{\link[xtable]{xtable}} and implements
#' default aesthetics.
#'
#' @param x \code{\link{tablet}}
#' @param caption passed to \code{\link[xtable]{xtableList}}
#' @param label passed to \code{\link[xtable]{xtableList}}
#' @param align passed to \code{\link[xtable]{xtableList}}
#' @param digits passed to \code{\link[xtable]{xtableList}}
#' @param display passed to \code{\link[xtable]{xtableList}}
#' @param format_name function to format variable names (accepts at least x and dots)
#' @param format_stat function to format names of statistics and factor levels (accepts at least x and dots)
#' @param format_value function to format cell values (accepts at least x and dots)
#' @param ... passed to \code{\link[xtable]{xtableList}}
#' @importFrom dplyr rename
#' @export
#' @keywords internal
#' @return like \code{\link[xtable]{xtableList}}
#' @examples
#' library(boot)
#' library(dplyr)
#' library(magrittr)
#' library(xtable)
#' melanoma %>%
#'   select(-time, -year) %>%
#'   mutate(sex = factor(sex), ulcer = factor(ulcer)) %>%
#'   group_by(status) %>%
#'   tablet %>%
#'   as_xtable

as_xtable.tablet <- function(
  x,
  caption = NULL,
  label = NULL,
  align = NULL,
  digits = NULL,
  display = NULL,
  auto = FALSE,
  variable = ' ',
  format_name = function(x, ...)paste0('\\!\\!\\textbf{', x, '}'),
  format_stat = function(x, ...)x,
  format_value = function(x, ...)sub('0 (0.0%)','', x),
  ...
){
  if(!requireNamespace('xtable')){
    stop('Please install and load the xtable package')
  }
  x$`_tablet_sort` <- NULL
  # index <- index(x)
  x$`_tablet_name` <- match.fun(format_name)(x = x$`_tablet_name`, ...)
  split <- x$`_tablet_name`
  x$`_tablet_name` <- NULL
  x$`_tablet_stat` <- as.character(x$`_tablet_stat`)
  x$`_tablet_level` <- ifelse(
    x$`_tablet_level` == 'numeric',
    x$`_tablet_stat`,
    x$`_tablet_level`
  )
  x$`_tablet_stat` <- NULL
  x$`_tablet_level` <- match.fun(format_stat)(x = x$`_tablet_level`, ...)
  # names(x)[names(x) == 'level'] <- ''
  headerlist <- headerlist(x)
  if(length(headerlist))warning('grouped columns not currently supported')
  for(i in seq_len(ncol(x))){
    lab <- attr(x[[i]], 'label')
    if(length(lab)){
      names(x)[[i]] <- lab
    }
  }
  stopifnot(is.character(variable), length(variable) == 1)
  names(x)[names(x) == '_tablet_level'] <- variable
  for(nm in names(x)[names(x) != variable]){
    x[[nm]] <- match.fun(format_value)(x = x[[nm]], ...)
  }
  xlist <- split(x, split)
  attr(xlist, 'subheadings') <- names(xlist)
  y <- xtable::xtableList(
    xlist,
    caption = caption,
    label = label,
    align = align,
    digits = digits,
    display = display,
    auto = auto,
    ...
  )
  y
}


