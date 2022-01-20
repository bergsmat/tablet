globalVariables(c(
   '_tablet_level', 'n','N', '.data','_tablet_value',
   '_tablet_N','_tablet_n', '_tablet_x',
   '_tablet_devalued_N','_tablet_devalued_n',
   '_tablet_widget', '_tablet_name', '_tablet_groupless',
   '_tablet_stat','_tablet_groupfull','where',
   '_tablet_class'
))

#' Identify Categories
#'
#' Identifies categories.  Generic, with method \code{\link{categoricals.data.frame}}.
#' @param x object
#' @param ... passed
#' @export
#' @return see methods
#' @keywords internal
#' @examples
#' example(classifiers)
#' categoricals(x)
#'
categoricals <- function(x, ...)UseMethod('categoricals')

#' Identify Numerics
#'
#' Identifies numerics.  Generic, with method \code{\link{numerics.data.frame}}.
#' @param x object
#' @param ... passed
#' @export
#' @return see methods
#' @keywords internal
#' @examples
#' example(classifiers)
#' numerics(x)
numerics <- function(x, ...)UseMethod('numerics')

#' Identify Observations
#'
#' Identifies observations.  Generic, with method \code{\link{observations.data.frame}}.
#' @param x object
#' @param ... passed
#' @export
#' @return see methods
#' @keywords internal
#' @examples
#' example(classifiers)
#' observations(x)
observations <- function(x, ...)UseMethod('observations')

#' Identify Classifiers
#'
#' Identifies classifiers.  Generic, with method \code{\link{classifiers.data.frame}}.
#' @param x object
#' @param ... passed
#' @export
#' @return see methods
#' @keywords internal
#' @examples
#' library(dplyr)
#' x <- data.frame(grp = 1:2, a = factor(1:2), b = 1:2, c = factor(1:2, levels = 2:1))
#' x <- group_by(x, grp)
#' classifiers(x)
classifiers <- function(x, ...)UseMethod('classifiers')

#' Identify Classifiers for Data Frame
#'
#' Identifies classifiers: literally, groups. Supplies tablet_n and tablet_N: groupwise
#' and ungrouped counts.
#' @param x data.frame
#' @param ... ignored
#' @importFrom dplyr groups ungroup tally add_count select group_by
#' @export
#' @keywords internal
#' @return data.frame, grouped if x was (see \code{\link[dplyr]{group_by}})

classifiers.data.frame <- function(x, ...){
   for(i in c('_tablet_N','_tablet_n'))if(i %in% names(x))stop('x cannot include ', i)
   grp <- groups(x)
   x <- ungroup(x)
   x <- add_count(x, name = '_tablet_N') #, wt = n())
   x <- select(x, `_tablet_N`, everything())
   for(col in grp){
      #nm <- paste0('n_', col)
      x <- group_by(x, .data[[col]], .add = TRUE)
      #x <- add_tally(x, name = nm, wt = n())
   }
   x <- add_tally(x, name = '_tablet_n') # , wt = n())
   x
}

#' Identify Categoricals for Data Frame
#'
#' Identifies categorical columns: literally, factors. Stacks factor levels and supplies value = 1.
#' @param x data.frame; names must not include 'name' or 'level'
#' @param ... passed to \code{\link{classifiers}}
#' @param na.rm_fac whether to drop NA observations; passed to \code{\link[tidyr]{gather}} as na.rm
#' @param exclude_fac which factor levels to exclude; see \code{\link{factor}} (exclude)
#' @importFrom dplyr groups ungroup add_tally add_count select group_by mutate
#' @importFrom tidyr gather
#' @importFrom dplyr all_of across everything
#' @export
#' @keywords internal
#' @return same class as x
#' @examples
#' example(classifiers)
#' categoricals(x)
#' levels(categoricals(x)$level)
categoricals.data.frame <- function(x, ..., na.rm_fac = FALSE, exclude_fac = NULL){
   for(i in c('_tablet_name','_tablet_level','_tablet_value'))if(i %in% names(x))stop('names x cannot include ',i)
   if(any(duplicated(names(x))))stop('names cannot be duplicated')
   x <- select(x, !!!groups(x),where(is.factor))
   var <- setdiff(names(x), sapply(groups(x), rlang::as_string))
   for(i in var){
      # prepend each factor level with column name for uniqueness
      levels(x[[i]]) <- paste(sep = '_tablet_', i, levels(x[[i]]))
   }
   lev <- unlist(lapply(select(ungroup(x), all_of(var)), levels))
   # if('tablet_numeric' %in% lev)stop('levels of factors in x cannot include \'tablet_numeric\'')
   x <- classifiers(x,...)
   x <- mutate(x, across(where(is.factor), as.character))
   x <- suppressWarnings(
      # attributes not identical across groups, they will be dropped
      # this is where _tablet_name is created
      gather(
         x,
         factor_key = TRUE,
         key = '_tablet_name',
         value = '_tablet_level',
         var,
         na.rm = na.rm_fac
      )
   )
   # possibly no var
   if(length(var) == 0){
      x <- slice(x, 0)
      x <- mutate(
         x,
         # _tablet_name created here too
         `_tablet_name` = factor(), # was character() before 0.4.3
         `_tablet_level` = factor()
      )
   }
   x <- mutate(x, `_tablet_level` = factor(`_tablet_level`, levels = lev, exclude = exclude_fac))
   x <- mutate(x, `_tablet_value` = 1L) # bogus value
   # missing observation of some category is moot if na.rm = TRUE
   # if exclude is NULL, NA is a distinct category and value should be available for tally
   # but if na not removed and not categorized, it should be passed to summary functions as na
   # ???
   # if(!na.rm){
   #    if(!is.null(exclude)){
   #       x <- mutate(x, value = ifelse(is.na(level), NA_integer_, value))
   #    }
   # }
   x
}

#' Identify Numerics for Data Frame
#'
#' Identifies numeric columns. Stacks values and supplies factor level 'numeric'.
#' @param x data.frame; names must not include 'name' or 'level'
#' @param ... passed to \code{\link{classifiers}}
#' @param na.rm_num whether to drop NA observations; passed to \code{\link[tidyr]{gather}} as na.rm
#' @importFrom dplyr groups ungroup tally add_count select group_by mutate slice
#' @importFrom tidyr gather
#' @export
#' @keywords internal
#' @return same class as x
numerics.data.frame <- function(x, ..., na.rm_num = FALSE){
   for(i in c('_tablet_level', '_tablet_name','_tablet_value'))if(i %in% names(x))stop('names x cannot include ',i)
   x <- select(x, !!!groups(x),where(is.numeric))
   var <- setdiff(names(x), sapply(groups(x),rlang::as_string))
   x <- classifiers(x,...)
   x <- mutate(x, `_tablet_level` = factor('numeric')) # bogus level
   x <- suppressWarnings(
      # attributes not identical across groups, they will be dropped
      # this is also where _tablet_name is created
      gather(
         x,
         factor_key = TRUE,
         key = '_tablet_name',
         value = '_tablet_value',
         var,
         na.rm = na.rm_num
      )
   )
   if(!length(var)){
      x <- slice(x, 0)
   }
   x
}

#' Identify Observations for Data Frame
#'
#' Identifies observations. Stacks values by level, supplying defaults as necessary.
#' @param x data.frame
#' @param ... passed to categoricals(), numerics()
#' @param exclude_name whether to drop NA levels of name; passed to \code{\link[tidyr]{gather}}
#' @importFrom dplyr groups ungroup tally add_count select group_by mutate arrange bind_rows
#' @importFrom tidyr gather
#' @export
#' @keywords internal
#' @return class 'observations' arranged by groups (presented first):
#' \item{_tablet_N}{number of records}
#' \item{_tablet_n}{number of records in group}
#' \item{_tablet_name}{observation identifier}
#' \item{_tablet_level}{factor level (or special value 'numeric' for numerics)}
#' \item{_tablet_value}{numeric value (or special value 1 for factors)}

observations.data.frame <- function(x, ..., exclude_name = NULL){
   x <- select(x, !!!groups(x), everything()) # groups first for consistency
   nms <- names(x)
   y <- categoricals(x, ...)
   z <- numerics(x, ...)
   x <- bind_rows(y, z)
   # x <- mutate(x, name = factor(exclude = exclude_name, name, levels = intersect(nms, levels(name))))
   x <- mutate(
      x,
      `_tablet_name` = factor(
         exclude = exclude_name,
         `_tablet_name`,
         levels = intersect( # tries to make output order match input across fac, num
            nms,
            unique(as.character(`_tablet_name`))
         )
      )
   )
   x <- group_by(x, `_tablet_name`, .add = TRUE)
   x <- group_by(x, `_tablet_level`, .add = TRUE)
   x <- arrange(x, .by_group = TRUE)
   class(x) <- union('observations', class(x))
   x
}

#' Aggregate Values
#'
#' Aggregated values.  Generic, with method \code{\link{devalued.observations}}.
#' @param x object
#' @param ... passed
#' @export
#' @return see methods
#' @keywords internal
#' @examples
#' example(classifiers)
#' devalued(observations(x))
devalued <- function(x, ...)UseMethod('devalued')

#' Aggregate Values for Observations
#' Aggregates values for observations.
#' Accepts a list of formulas with result name on left and
#' aggregating expression on the right.
#' Behavior is undefined if any expression does not aggregate! (I.e., length != 1).
#' Expressions are evaluated in an environment where values are available as 'x',
#' grouped count is available as 'n' and ungouped count is available as 'N'.
#'
#'
#' @param x observations
#' @param ... passed formulas with matching LHS will replace corresponding element of fun.
#' @param fun default aggregate functions expressed as formulas
#' @param silent whether to suppress warnings from evaluations of 'fun'
#' @importFrom rlang f_lhs f_rhs eval_tidy :=
#' @importFrom dplyr mutate distinct rename
#' @export
#' @keywords internal
#' @return class 'devalued', presumably one record per group:
#' \item{_tablet_N}{number of records}
#' \item{_tablet_n}{number of records in group}
#' \item{_tablet_name}{observation identifier}
#' \item{_tablet_level}{factor level (or special value 'numeric' for numerics)}
#' \item{(other)}{additional column for each statistic in 'fun'}
devalued.observations <- function(
   x,
   ...,
   fun = list(
      sum ~ signif(digits = 3,    sum(x, na.rm = TRUE)),
      pct ~ signif(digits = 3,    sum / n * 100       ),
      ave ~ signif(digits = 3,   mean(x, na.rm = TRUE)),
      std ~ signif(digits = 3,     sd(x, na.rm = TRUE)),
      med ~ signif(digits = 3, median(x, na.rm = TRUE)),
      min ~ signif(digits = 3,    min(x, na.rm = TRUE)),
      max ~ signif(digits = 3,    max(x, na.rm = TRUE))
   ),
   silent = TRUE
){
  nms <- names(x)
  # ns <- nms[grepl('n_',nms)]
  # for(i in seq_along(ns)){ # alias
  #    this <- ns[[i]]
  #    that <- paste0('n_', i)
  #    x[[that]] <- x[[this]]
  # }
  extra <- list(...)
  form <- sapply(extra, function(f)inherits(f, 'formula'))
  if(length(form))extra <- extra[form]
  LHS <- sapply(extra, rlang::f_lhs)
  LHS <- sapply(LHS, rlang::as_string)
  # guard
  if('x' %in% names(x)){
     x <- rename(x, `_tablet_x` = x)
  }
  if('N' %in% names(x)){
     x <- rename(x, `_tablet_devalued_N` = N)
  }
  if('n' %in% names(x)){
     x <- rename(x, `_tablet_devalued_n` = n)
  }

  # supply
  x <- rename(x, x = `_tablet_value`)
  x <- rename(x, N = `_tablet_N`)
  x <- rename(x, n = `_tablet_n`)

  # evaluate
  for(i in seq_along(fun)){
     this <- fun[[i]]
     lhs <- rlang::as_string(rlang::f_lhs(this))
     if(lhs %in% LHS)this <- extra[[match(lhs, LHS)]]
     rhs <- rlang::f_rhs(this)
     if(silent){
        x <- suppressWarnings(mutate(x, !!lhs := rlang::eval_tidy(rhs)))
     } else {
        x <- mutate(x, !!lhs := rlang::eval_tidy(rhs))
     }
  }
  # for(i in seq_along(ns)){ # cleanup
  #    that <- paste0('n_', i)
  #    x[[that]] <- NULL
  # }
  x <- select(x, -x)

  # restore
  x <- rename(x, `_tablet_N` = N)
  x <- rename(x, `_tablet_n` = n)

  if('_tablet_x' %in% names(x)){
     x <- rename(x, x = `_tablet_x`)
  }
  if('_tablet_devalued_N' %in% names(x)){
     x <- rename(x, N = `_tablet_devalued_N`)
  }
  if('_tablet_devalued_n' %in% names(x)){
     x <- rename(x, n = `_tablet_devalued_n`)
  }
  x <- distinct(x)
  class(x) <- union('devalued', class(x))
  x
}

#' Calculate Widgets
#'
#' Calculates widgets.  Generic, with method \code{\link{widgets.devalued}}.
#' @param x object
#' @param ... passed
#' @export
#' @return see methods
#' @keywords internal
#' @examples
#' example(classifiers)
#' widgets(devalued(observations(x)))
widgets <- function(x, ...)UseMethod('widgets')
# https://adv-r.hadley.nz/quasiquotation.html

#' Calculate Widgets for Devalued
#'
#' Calculates widgets for class 'devalued'.
#' In this context, a widget is a text fragment
#' that formats one or more aggregate statistics,
#' all of which must have been present as LHS of
#' some element of 'fun' in the call to devalued().
#' Furthermore, LHS must be unique across 'fac' and
#' 'num' for expected behavior.
#'
#' In the context of this call, "+" is redefined
#' to allow concatenation of text.
#' Evaluation proceeds left to right as usual.
#' @param x devalued
#' @param fac a list of formulas to generate widgets for factors
#' @param num a list of formulas to generate widgets for numerics
#' @param ... formulas with matching LHS replace defaults
#' @importFrom rlang f_lhs f_rhs eval_tidy
#' @importFrom tidyr gather
#' @importFrom dplyr filter
#' @export
#' @keywords internal
#' @return class 'widgets', arranged by groups:
#' \item{_tablet_name}{observation identifier}
#' \item{_tablet_level}{factor level (or special value 'numeric' for numerics)}
#' \item{_tablet_N}{number of records}
#' \item{_tablet_n}{number of records in group}
#' \item{_tablet_stat}{the LHS of formulas in 'fac' and 'num'}
#' \item{_tablet_widget}{the RHS of formulas in 'fac' and 'num' (evaluated)}

widgets.devalued <- function(
   x,
   fac = list(
       ` ` ~ sum + ' (' + pct + '%' + ')'
    ),
   num = list(
      `Mean (SD)` ~ ave + ' (' + std + ')',
      `Median (range)` ~ med + ' (' + min + ', ' + max + ')'
   ),
   ...
){
   `+` <- function(e1, e2){
      if(is.numeric(e1) && is.numeric(e2)){
         res <- .Primitive("+")(e1, e2)
         return(res)
      }
      e1 <- as.character(e1)
      e2 <- as.character(e2)
      res <- paste0(e1, e2)
      return(res)
   }
   for(i in c('_tablet_stat', '_tablet_widget'))if(i %in% names(x))stop('names x cannot include ',i)
   extra <- list(...)
   form <- sapply(extra, function(f)inherits(f, 'formula'))
   if(length(form))extra <- extra[form]
   LHS <- sapply(extra, rlang::f_lhs)
   LHS <- sapply(LHS, rlang::as_string)
   keep <- unlist(sapply(c(fac, num), rlang::f_lhs))
   for(i in seq_along(fac)){ # evaluate
      this <- fac[[i]]
      lhs <- rlang::as_string(rlang::f_lhs(this))
      if(lhs %in% LHS)this <- extra[[match(lhs, LHS)]]
      rhs <- rlang::f_rhs(this)
      x <- mutate(x, !!lhs := ifelse(`_tablet_level` == 'numeric', NA, rlang::eval_tidy(rhs)))
   }
   for(i in seq_along(num)){ # evaluate
      this <- num[[i]]
      lhs <- rlang::as_string(rlang::f_lhs(this))
      if(lhs %in% LHS)this <- extra[[match(lhs, LHS)]]
      rhs <- rlang::f_rhs(this)
      x <- mutate(x, !!lhs := ifelse(`_tablet_level` != 'numeric', NA, rlang::eval_tidy(rhs)))
   }
   x <- gather(x, key = '_tablet_stat', value = '_tablet_widget', !!!keep, factor_key = TRUE)
   x <- filter(x, !is.na(`_tablet_widget`))
   x <- arrange(x, .by_group = TRUE)
   x <- select(x,!!!groups(x), `_tablet_N`:`_tablet_level`, `_tablet_stat`, `_tablet_widget`)
   x <- mutate(x, `_tablet_widget` = as.character(`_tablet_widget`)) # in case logi
   class(x) <- union('widgets', class(x))
   x
}

#' Calculate Without Groups
#'
#' Calculates without groups.  Generic, with method \code{\link{groupless.data.frame}}.
#' It is meaningless to use this toolchain function on classes
#' 'observations', 'devalued', or 'widgets'.
#' @param x object
#' @param ... passed
#' @export
#' @return see methods
#' @keywords internal
groupless <- function(x, ...)UseMethod('groupless')

#' Calculate Widgets Without Groups
#'
#' Calculates widgets without groups.
#' Removes grouping variables and groups, then executes:
#' data.frame -> observations -> devalued -> widgets.
#' @param x data.frame
#' @param ... passed to observations(), devalued(), widgets()
#' @importFrom dplyr groups ungroup select
#' @export
#' @keywords internal
#' @return class 'groupless', with output like \code{\link{widgets.devalued}}
groupless.data.frame <- function(x,...){
   # remove grouping variables and remove groups
   # then execute
   grp <- groups(x)
   x <- ungroup(x)
   #x <- select(x, setdiff(names(x), grp))
   x <- select(x, setdiff(names(x), sapply(grp, rlang::as_string))) # very important when grp name contains spaces!
   x <- observations(x, ...)
   x <- devalued(x, ...)
   x <- widgets(x, ...)
   if(any(x$`_tablet_n` != x$`_tablet_N`))warning('unexpected difference between N and n')
   x$`_tablet_n` <- NULL
   class(x) <- union('groupless', class(x))
   x
}

#' Calculate With Groups
#'
#' Calculates with groups.  Generic, with method \code{\link{groupfull.data.frame}}.
#' It is meaningless to use this toolchain function on classes
#' 'observations', 'devalued', or 'widgets'.
#' @param x object
#' @param ... passed
#' @export
#' @return see methods
#' @keywords internal
groupfull <- function(x, ...)UseMethod('groupfull')


#' Calculate Widgets With Groups
#'
#' Calculates widgets with groups.
#' Executes: data.frame -> observations -> devalued -> widgets.
#' @param x data.frame
#' @param ... passed to observations(), devalued(), widgets()
#' @export
#' @keywords internal
#' @return class 'groupfull', with output like \code{\link{widgets.devalued}}
groupfull.data.frame <- function(x,...){
   # execute data.frame -> observations -> devalued -> widgets
   x <- observations(x, ...)
   x <- devalued(x, ...)
   x <- widgets(x, ...)
   class(x) <- union('groupfull', class(x))
   x
}

#' Calculate With and Without Groups
#'
#' Calculates with and without groups.  Generic, with method \code{\link{groupwise.data.frame}}.
#' It is meaningless to use this toolchain function on classes
#' 'observations', 'devalued', 'widgets', 'groupless', or 'groupfull'.
#' @param x object
#' @param ... passed
#' @importFrom dplyr left_join
#' @export
#' @return see methods
#' @keywords internal
#' @examples
#' example(classifiers)
#' groupwise(x)
groupwise <- function(x, ...)UseMethod('groupwise')

#' Calculate Widgets With and Without Groups
#'
#' Calculates widgets with and without groups.
#' Supplies 'groupfull' and 'groupless' (prefixed) columns instead of 'widgets'.
#' Column attributes 'label' and 'title' (highest priority) are substituted for column name, if present.
#' @param x data.frame
#' @param fun passed to groupfull() and groupless()
#' @param fac passed to groupfull() and groupless()
#' @param num passed to groupfull() and groupless()
#' @param ... passed to groupfull() and groupless()
#' @export
#' @keywords internal
#' @return class 'groupwise', arranged by groups:
#' \item{_tablet_name}{observation identifier}
#' \item{_tablet_level}{factor level (or special value 'numeric' for numerics)}
#' \item{_tablet_N}{number of records}
#' \item{_tablet_n}{number of records in group}
#' \item{_tablet_stat}{the LHS of formulas in 'fac' and 'num'}
#' \item{_tablet_groupfull}{the LHS of formulas in 'fac' and 'num'}
#' \item{_tablet_groupless}{the LHS of formulas in 'fac' and 'num'}

groupwise.data.frame <- function(
   x,
   fun = list(
      sum ~ signif(digits = 3,    sum(x, na.rm = TRUE)),
      pct ~ signif(digits = 3,    sum / n * 100    ),
      ave ~ signif(digits = 3,   mean(x, na.rm = TRUE)),
      std ~ signif(digits = 3,     sd(x, na.rm = TRUE)),
      med ~ signif(digits = 3, median(x, na.rm = TRUE)),
      min ~ signif(digits = 3,    min(x, na.rm = TRUE)),
      max ~ signif(digits = 3,    max(x, na.rm = TRUE))
   ),
   fac = list(
      ` ` ~ sum + ' (' + pct + '%' + ')'
   ),
   num = list(
      `Mean (SD)` ~ ave + ' (' + std + ')',
      `Median (range)` ~ med + ' (' + min + ', ' + max + ')'
   ),
   ...
){
   # promote labels and titles where present
   for(i in seq_len(ncol(x))){
      lab <- attr(x[[i]],'label')
      ttl <- attr(x[[i]], 'title')
      if(length(lab)) names(x)[[i]] <- lab
      if(length(ttl)) names(x)[[i]] <- ttl
   }

   # execute data.frame -> observations -> devalued -> widgets
   y <- groupfull(x, fun = fun, fac = fac, num = num, ...)
   y <- rename(y, `_tablet_groupfull` = `_tablet_widget`)
   z <- groupless(x, fun = fun, fac = fac, num = num, ...)
   z <- rename(z, `_tablet_groupless` = `_tablet_widget`)
   n <- nrow(y)
   y <- suppressMessages(left_join(y, z))
   if(nrow(y) != n)warning('unexpected change in row count')
   discard <- c('groupless','groupfull','widgets','observations','devalued')
   myclass <- setdiff(class(x), discard)
   myclass <- union('groupwise', myclass)
   class(y) <- myclass
   y
}

#' Generate a Tablet
#'
#' Generates a tablet.  Generic. See \code{\link{tablet.data.frame}} and \code{\link{as_kable.tablet}}.
#' @param x object
#' @param ... passed arguments
#' @export
#' @return see methods
#' @keywords internal
#' @examples
#' example(tablet.data.frame)
tablet <- function(x, ...)UseMethod('tablet')

#' Generate a Group-wise Tablet
#'
#' Generates a group-wise tablet. Calculates statistics
#' for all factors and numerics, with and without grouping
#' variables.  Column names represent finest level of
#' grouping, distinguished by attribute 'nest' (the values of
#' higher groups). Column names include 'all' column
#' for same statistics without groups. Result columns
#' have corresponding attribute 'n'. 'lab' supplies a label attribute
#' for each column where the RHS succeeds: by default appending 'n' to result column names.
#' Column attributes 'label' and 'title' (highest priority) are substituted for column name, if present.
#' @param x groupwise
#' @param ... ignored
#' @param all a column name for ungrouped statistics; can have length zero to suppress ungrouped column
#' @param lab a list of formulas to generate column labels; \\n is translated as <br> in html context
#' @importFrom dplyr groups ungroup select
#' @importFrom tidyr spread
#' @importFrom rlang as_string
#' @export
#' @keywords internal
#' @return 'tablet', with columns for each combination of groups, and:
#' \item{_tablet_name}{observation identifier}
#' \item{_tablet_level}{factor level (or special value 'numeric' for numerics)}
#' \item{_tablet_sort}{sorting column}
#' \item{_tablet_stat}{the LHS of formulas in 'fac' and 'num'}
#' \item{All (or value of 'all' argument)}{ungrouped results}
tablet.groupwise <- function(
   x,
   ...,
   all = 'All',
   lab = list(
      lab ~ name + '\n(N = ' + n + ')'
   )
){

  # convert certain columns to attributes
  myclass <- class(x)
  stopifnot(
     (length(all) == 0 ) |   # can be NULL or character(0)
     (is.character(all) & length(all) == 1) # otherwise atomic char
  )
  N <- unique(x$`_tablet_N`)
  if(!length(N))N <- 0L
  x$`_tablet_N` <- NULL
  grp <- groups(x)
  grp <- lapply(grp, rlang::as_string)
  strata <- setdiff(grp, c('_tablet_name','_tablet_level'))
  strata <- rev(strata)
  base <- character(0)
  if(length(strata)){
     base <- strata[[1]]
  }
  if(length(base)){
     # x[[base]] <- factor(x[[base]])
     # levs <- levels(x[[base]])
     # quan <- x$`_tablet_n`[match(levs, x[[base]])]
     # levels(x[[base]]) <- paste(levs, quan, sep = '_tablet_nlev_')

     # nested elements of base may be repeated,
     # with differing n,
     # and therefore must not collapse to mere factor
     x[[base]] <- as.character(x[[base]])
     x[[base]] <- paste(x[[base]], x$`_tablet_n`, sep = '_tablet_nlev_')
  }
  x$`_tablet_n` <- NULL
  tot <- select(
     ungroup(x),
     `_tablet_name`,
     `_tablet_level`,
     `_tablet_stat`,
     `_tablet_groupless`
  )
  tot <- unique(tot)
  if(length(all)){
     names(tot)[names(tot)=='_tablet_groupless'] <- all
  } else {
       tot$`_tablet_groupless` <- NULL
  }

  x <- ungroup(x)

  # distribute columns
  if(length(strata)){
     x$`_tablet_class` <- do.call(
        paste,
        c(
           select(x, !!!strata),
           list(sep = '_tablet_strata_')
        )
     )
     x$`_tablet_class` <- factor(x$`_tablet_class`, levels = unique(x$`_tablet_class`))
     # strsplit is used later.
     # if last class is empty string, it will be discarded.
     # guard empty string with one more instance of separator.
     levels(x$`_tablet_class`) <- paste0(levels(x$`_tablet_class`),'_tablet_strata_')
     x <- select(
        x,
        `_tablet_class`,
        `_tablet_name`,
        `_tablet_level`,
        `_tablet_stat`,
        `_tablet_groupfull`
      )
     x <- spread(x, `_tablet_class`, `_tablet_groupfull`)
  } else {
      x <- tot
  }

  # bind ungrouped stats
  suppressMessages(x <- left_join(x,tot))
  if(length(all))attr(x[[all]], 'n') <- N
  for(i in seq_len(ncol(x))){
     cl <- names(x)[[i]]
     if(grepl('_tablet_strata_', cl)){
        splits <- strsplit(cl, '_tablet_strata_')[[1]]
        names(x)[[i]] <- splits[[1]]
        attr(x[[i]], 'nest') <- splits[-1]
     }
  }
  for(i in seq_len(ncol(x))){
     cl <- names(x)[[i]]
     if(grepl('_tablet_nlev_', cl)){
        splits <- strsplit(cl, '_tablet_nlev_')[[1]]
        names(x)[[i]] <- splits[[1]]
        attr(x[[i]], 'n') <- splits[[2]]
     }
  }

  # markup result columns
  `+` <- function(e1, e2){
     if(is.numeric(e1) && is.numeric(e2)){
        res <- .Primitive("+")(e1, e2)
        return(res)
     }
     e1 <- as.character(e1)
     e2 <- as.character(e2)
     res <- paste0(e1, e2)
     return(res)
  }
  extra <- list(...)
  form <- sapply(extra, function(f)inherits(f, 'formula'))
  if(length(form))extra <- extra[form]
  LHS <- sapply(extra, rlang::f_lhs)
  LHS <- sapply(LHS, rlang::as_string)
  for(i in seq_along(lab)){ # evaluate
     this <- lab[[i]]
     lhs <- rlang::as_string(rlang::f_lhs(this))
     if(lhs %in% LHS)this <- extra[[match(lhs, LHS)]]
     rhs <- rlang::f_rhs(this)
     for(j in seq_len(ncol(x))){
        nm <- names(x)[[j]]
        dat <- attributes(x[[j]])
        dat <- c(dat, list(name = nm))
        out <- try(silent = TRUE, rlang::eval_tidy(rhs, data = dat))
        if(class(out) != 'try-error'){
           attr(x[[j]], 'label') <- out
        }
     }
  }

  # restore levels
  # for vignette transpose example:
  # Error in .subset2(chunks, self$get_current_group()) : attempt to select less than one element in integerOneIndex
  # x <- mutate(x, `_tablet_sort` = as.numeric(`_tablet_level`))
  # x <- mutate(x, `_tablet_level` = as.character(`_tablet_level`))
  # x <- mutate(x, `_tablet_level` = sub('.*_tablet_', '', `_tablet_level`))
  x$`_tablet_sort` <- as.numeric(x$`_tablet_level`)
  x$`_tablet_level` <- as.character(x$`_tablet_level`)
  x$`_tablet_level` <- sub('.*_tablet_', '', x$`_tablet_level`)

  constitutive <- c(
     '_tablet_name',
     '_tablet_level',
     '_tablet_sort',
     '_tablet_stat'
  )

  x <- cbind(
    x[, constitutive, drop = FALSE],
    x[,!(names(x) %in% constitutive), drop = FALSE]
  )


  # this is the only constructor for 'tablet'
  class(x) <- union('tablet', setdiff(class(x), 'groupwise'))
  x
}

#' Create Header List.
#'
#' Creates header list.  Generic, with method \code{\link{headerlist.tablet}}.
#' @param x object
#' @param ... passed
#' @export
#' @return see methods
#' @keywords internal
#' @examples
#' example(classifiers)
#' headerlist(tablet(groupwise(x)))
headerlist <- function(x,...)UseMethod('headerlist')

#' Create Header List for Tablet
#'
#' Creates header list for tablet.
#' @param x tablet
#' @param ... ignored
#' @export
#' @keywords internal
#' @return list of named integer where each element describes an additional header row with names as indicated, repeated integer times
headerlist.tablet <- function(x, ...){
   numheaders <- 0
   out <- list()
   for(i in seq_len(ncol(x))){
      h <- attr(x[[i]],'nest')
      numheaders <- max(numheaders, length(h))
   }
   for(e in seq_len(numheaders)){ # for each header level
      this <- numeric(0)
      for(col in seq_len(ncol(x))){ # for each column
         h <- attr(x[[col]], 'nest')
         if(length(h) >= e){
            head <- h[[e]] # use the right level if you have it
         } else {
            head <- ' ' # otherwise use one space
         }
         if(length(this) == 0){
            this <- 1L
            names(this) <- head
         } else{
            if(rev(names(this))[[1]] == head){
               this[[length(this)]] <- this[[length(this)]] + 1
            } else {
               this[[length(this) + 1]] <- 1
               names(this)[[length(this)]] <- head
            }
         }
      }
      out[[e]] <- this
   }
   return(out)
}


#' Create Index
#'
#' Creates index.  Generic, with method \code{\link{index.tablet}}.
#' @param x object
#' @param ... passed
#' @export
#' @return see methods
#' @keywords internal
#' @examples
#' example(classifiers)
#' index(tablet(groupwise(x)))
index <- function(x,...)UseMethod('index')

#' Create Index for Tablet
#'
#' Creates index for tablet.
#' @param x tablet
#' @param ... ignored
#' @export
#' @keywords internal
#' @return named integer giving row groupings with names as indicated, repeated integer times

index.tablet <- function(x, ...) {
   out <- numeric(0)
   for (row in seq_len(nrow(x))) {
      r <- as.character(x$`_tablet_name`)[[row]]
      if (length(out) == 0) {
         out <- 1L
         names(out) <- r
      } else {
         if (names(out)[[length(out)]] == r) {
            out[[length(out)]] <- out[[length(out)]] + 1
         } else {
            out[[length(out) + 1]] <- 1
            names(out)[[length(out)]] <- r
         }
      }
   }
   return(out)
}


#' Coerce to Kable
#'
#' Coerces to kable output class.  Generic,
#' with method \code{\link{as_kable.tablet}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @return see methods
#' @keywords internal
#' @examples
#' example(classifiers)
#' as_kable(tablet(groupwise(x)))
as_kable <- function(x, ...)UseMethod('as_kable')
# https://colinfay.me/writing-r-extensions/generic-functions-and-methods.html
# https://stackoverflow.com/questions/13984470/possible-to-create-latex-multicolumns-in-xtable


#' Coerce Tablet to Kable
#'
#' Renders a tablet.  Calls \code{\link[kableExtra]{kbl}} and implements
#' special features like grouped columns.
#'
#' @param x \code{\link{tablet}}
#' @param ... passed to \code{\link[kableExtra]{kbl}}
#' @param booktabs passed to \code{\link[kableExtra]{kbl}}
#' @param escape passed to \code{\link[kableExtra]{kbl}}; defaults FALSE to allow header linebreaks
#' @param escape_latex a function to pre-process column names and content if 'escape' is FALSE (e.g., manual escaping, latex only); default \code{\link{escape_latex}}
#' @param escape_html  a function to pre-process column names and content if 'escape' is FALSE (e.g., manual escaping, html only)
#' @param variable a column name for the variables
#' @param col.names passed to \code{\link[kableExtra]{kbl}} after any linebreaking
#' @param linebreak whether to invoke \code{\link[kableExtra]{linebreak}} for column names
#' @param align passed to \code{\link[kableExtra]{linebreak}} for column names
#' @param double_escape passed to \code{\link[kableExtra]{linebreak}} for column names
#' @param linebreaker passed to \code{\link[kableExtra]{linebreak}} for column names in latex; for html, newline is replaced with <br>
#' @param pack_rows named list passed to \code{\link[kableExtra]{pack_rows}} for finer control of variable names
#' @importFrom kableExtra kbl pack_rows add_header_above linebreak
#' @importFrom dplyr rename
#' @export
#' @return like \code{\link[kableExtra]{kbl}}
#' @examples
#' library(boot)
#' library(dplyr)
#' library(magrittr)
#' library(haven)
#' library(yamlet)
#' library(spork)
#' melanoma %>%
#'   select(-time, -year) %>%
#'   mutate(sex = factor(sex), ulcer = factor(ulcer)) %>%
#'   group_by(status) %>%
#'   tablet %>%
#'   as_kable
#'
#' x <- system.file(package = 'tablet', 'shiny-examples/mesa/data/adsl.sas7bdat')
#' x %<>% read_sas %>% data.frame
#' decorations(x) # note weight in pounds
#' x %<>% mutate(weight = signif(digits = 3, weight * 2.2))
#'
#' # calculate BMI by assuming all males are 1.75 m, all females 1.63 cm
#' x %<>% mutate(height = ifelse(sex == 'F', 1.63, 1.75))
#' x %<>% mutate(bmi = signif(digits = 3, weight / (height^2)))
#' x %<>% filter(saffl == 'Y')
#' x %<>% select(trt01a, age, sex, weight, bmi)
#' x %<>% redecorate('
#' trt01a: [ Treatment, [ Placebo, TRT 10 mg, TRT 20 mg ]]
#' age:    [ Age, year ]
#' sex:    [ Sex, [ Female: F, Male: M ]]
#' weight: [ Body Weight, kg ]
#' bmi:    [ Index_body mass, kg/m^2 ]
#' ')
#' x %<>% resolve
#' x %<>% group_by(trt01a)
#'
#' x %>% tablet %>% as_kable
#'
#' # supply default and unit-conditional latex titles
#' x %<>% modify(title = concatenate(as_latex(as_spork(c(.data$label)))))
#' x %<>% modify(
#' age, weight, bmi,
#'   title = concatenate(
#'     sep = '',  # default ok in pdf
#'     as_latex(
#'       as_spork(
#'         c(.data$label, ' (', .data$units, ')')
#'       )
#'     )
#'   )
#' )
#' x %>% tablet %>% as_kable


as_kable.tablet <- function(
   x,
   ...,
   booktabs = TRUE,
   escape = FALSE,
   escape_latex = tablet::escape_latex,
   escape_html = function(x, ...)x,
   variable = ' ',
   col.names = NA,
   linebreak = TRUE,
   align = 'c',
   double_escape = FALSE,
   linebreaker = '\n',
   pack_rows = list()
){

   # if(is.na(escape)){
   #    if (knitr::is_latex_output()){
   #      escape <- FALSE
   #    } else {
   #       escape <- FALSE # for <br>
   #    }
   # }
   stopifnot(is.logical(escape), length(escape) == 1)
   x$`_tablet_sort` <- NULL
   index <- index(x) # draws on _tablet_name, which should be factor or 'latex', 'factor'
   nmsi <- names(index) # isolate to assign class
   if(inherits(x$`_tablet_name`, 'latex')) nmsi <- as_latex(nmsi) # class propagation
   x$`_tablet_name` <- NULL # done
   if(!escape){
      if (knitr::is_latex_output()) {
         nmsi <- escape_latex(nmsi) # invokes class-specific method
      } else {
         nmsi <- escape_html(nmsi)
      }
   }
   # nmsi now as informed as possible ... assign back
   names(index) <- nmsi
   #x$`_tablet_level` <- as.character(x$`_tablet_level`)
   x$`_tablet_stat` <- as.character(x$`_tablet_stat`)
   #x <- mutate(x, `_tablet_level` = ifelse(`_tablet_level` == 'numeric', `_tablet_stat`, `_tablet_level`))
   x$`_tablet_level` <- ifelse(
      x$`_tablet_level` == 'numeric',
      x$`_tablet_stat`,
      x$`_tablet_level`
   )
   x$`_tablet_stat` <- NULL
   # names(x)[names(x) == 'level'] <- ''
   headerlist <- headerlist(x)
   for(i in seq_len(ncol(x))){
      lab <- attr(x[[i]], 'label')
      if(length(lab)){
         names(x)[[i]] <- lab
      }
   }
   #x <- rename(x, !!variable := `_tablet_level`)
   stopifnot(is.character(variable), length(variable) == 1)
   names(x)[names(x) == '_tablet_level'] <- variable
   if(!escape){
      if (knitr::is_latex_output()) {
         x[] <- lapply(x, escape_latex, ...)
         names(x) <- escape_latex(names(x), ...)
      } else {
         x[] <- lapply(x, escape_html, ...)
         names(x) <- escape_html(names(x), ...)
      }
   }


   if(is.na(col.names))col.names <- names(x)
   if (linebreak){
      if(knitr::is_latex_output()) {
         col.names <- linebreak(
            col.names,
            align = align,
            double_escape = double_escape,
            linebreaker = linebreaker
         )
      } else {
         col.names <- gsub('\n','<br>', col.names)
      }
   }
   y <- kableExtra::kbl(
      x,
      booktabs = booktabs,
      escape = escape,
      col.names = col.names,
      ...
   )
   for(i in seq_along(headerlist)){
      y <- add_header_above(y, headerlist[[i]])
   }
   y <- do.call(
      kableExtra::pack_rows,
      c(
         list(y, index = index, escape = escape),
         pack_rows
      )
   )
   y
}

#' Generate a Tablet for Data Frame
#'
#' Generates a 'tablet': a summary table of
#' formatted statistics for factors (is.factor()) and
#' numerics (is.numeric()) in x, with and without grouping
#' variables (if present, see \code{\link[dplyr]{group_by}}).
#' Column names represent finest level of
#' grouping, distinguished by attribute 'nest' (the values of
#' higher other groups, if any) along with the 'all' column
#' for ungrouped statistics. Column attribute 'n' indicates
#' relevant corresponding observation count.
#' Input should not have column names beginning with '_tablet'.
#'
#' Arguments 'fun', 'fac', 'num', and 'lab' are lists
#' of two-sided formulas that are evaluated in
#' an environment where '+' expresses concatenation
#' (for character elements).
#' The values of LHS should be unique across all four lists.
#' 'fun' is a list of aggregate statistics that have access to
#' \code{N} (number of original records),
#' \code{n} (number of group members), and
#' \code{x} (the numeric observations, or 1 for each factor value).
#' Aggregate statistics generated by 'fun' are available
#' for use in 'fac' and 'num'
#' which create visualizations thereof ('widgets'). Column-specific
#' attributes are available to elements of 'lab', including
#' the special attribute \code{name} (the current column name).
#' For 'lab' only, if the RHS succeeds, it becomes the label
#' attribute of the corresponding output column. 'lab' is used
#' here principally to support annotation of *output*
#' columns; if *input* columns have attributes 'label' or 'title'
#' (highest priority) those will have been already substituted
#' for default column names at the appropriate positions in the
#' output.
#'
#' Missingness of observations (and to a lesser extent, levels of
#' grouping variables) merits special consideration.
#' Be aware that na.rm_fac and na.rm_num take their defaults
#' from na.rm.  Furthermore, na.rm_fac may interact with
#' exclude_fac, which is passed to \code{\link{factor}} as \code{exclude}.
#' The goal is to support all possible ways of expressing or ignoring
#' missingness. That said, if aggregate functions are removing
#' NA, the values of arguments beginning with 'na.rm' or 'exclude'
#' may not matter.
#'
#' Output includes the column \code{_tablet_name} which inherits character.
#' Its values are typically the names of the original columns
#' that were factor or numeric but not in groups(x). If the first
#' of these had a label attribute or (priority) a title attribute
#' with class 'latex', then \code{_tablet_name} is assigned the
#' class 'latex' as well. It makes sense therefore to be consistent
#' across input columns regarding the presence or not of a 'latex'
#' label or title. By default, \code{\link{as_kable.tablet}} dispatches
#' class-specific methods for \code{\link{escape_latex}}.
#'
#' @usage
#' \method{tablet}{data.frame}(
#'  x,
#'  ...,
#'  na.rm = FALSE,
#'  all = 'All',
#'  fun = list(
#'   sum ~ signif(digits = 3,     sum(x,  na.rm = TRUE)),
#'   pct ~ signif(digits = 3,     sum / n * 100        ),
#'   ave ~ signif(digits = 3,    mean(x,  na.rm = TRUE)),
#'   std ~ signif(digits = 3,      sd(x,  na.rm = TRUE)),
#'   med ~ signif(digits = 3,  median(x,  na.rm = TRUE)),
#'   min ~ signif(digits = 3,     min(x,  na.rm = TRUE)),
#'   max ~ signif(digits = 3,     max(x,  na.rm = TRUE))
#'  ),
#'  fac = list(
#'   ` ` ~ sum + ' (' + pct + '\%' + ')'
#'  ),
#'  num = list(
#'   `Mean (SD)` ~ ave + ' (' + std + ')',
#'   `Median (range)` ~ med + ' (' + min + ', ' + max + ')'
#'   ),
#'  lab = list(
#'   lab ~ name + '\\n(N = ' + n + ')'
#'  ),
#'  na.rm_fac = na.rm,
#'  na.rm_num = na.rm,
#'  exclude_fac = NULL,
#'  exclude_name = NULL
#' )
#' @param x data.frame (possibly grouped)
#' @param ... substitute formulas for elements of fun, fac, num, lab
#' @param na.rm whether to remove NA in general
#' @param all a column name for ungrouped statistics; can have length zero to suppress ungrouped column
#' @param fun default aggregate functions expressed as formulas
#' @param fac a list of formulas to generate widgets for factors
#' @param num a list of formulas to generate widgets for numerics
#' @param lab a list of formulas to generate label attributes for columns (see details)
#' @param na.rm_fac whether to drop NA 'factor' observations; passed to \code{\link[tidyr]{gather}} as na.rm, interacts with exclude_fac
#' @param na.rm_num whether to drop NA numeric observations; passed to \code{\link[tidyr]{gather}} as na.rm
#' @param exclude_fac which factor levels to exclude; see \code{\link{factor}} (exclude)
#' @param exclude_name whether to drop NA values of column name (for completeness); passed to \code{\link[tidyr]{gather}}
#' @export
#' @return 'tablet', with columns for each combination of groups, and:
#' \item{_tablet_name}{observation identifier: character, possibly 'latex', see details}
#' \item{_tablet_level}{factor level (or special value 'numeric' for numerics)}
#' \item{_tablet_stat}{the LHS of formulas in 'fac' and 'num'}
#' \item{All (or value of 'all' argument)}{ungrouped results}
#' \item{_tablet_sort}{sorting column}
#' @seealso \code{\link{as_kable.tablet}}
#' @examples
#' library(boot)
#' library(dplyr)
#' library(magrittr)
#' melanoma %>%
#'   select(-time, -year) %>%
#'   mutate(sex = factor(sex), ulcer = factor(ulcer)) %>%
#'   group_by(status) %>%
#'   tablet

tablet.data.frame <- function(
   x,
   ...,
   na.rm = FALSE,
   all = 'All',
   fun = list(
      sum ~ signif(digits = 3,    sum(x, na.rm = TRUE)),
      pct ~ signif(digits = 3,    sum / n * 100    ),
      ave ~ signif(digits = 3,   mean(x, na.rm = TRUE)),
      std ~ signif(digits = 3,     sd(x, na.rm = TRUE)),
      med ~ signif(digits = 3, median(x, na.rm = TRUE)),
      min ~ signif(digits = 3,    min(x, na.rm = TRUE)),
      max ~ signif(digits = 3,    max(x, na.rm = TRUE))
   ),
   fac = list(
      ` ` ~ sum + ' (' + pct + '%' + ')'
   ),
   num = list(
      `Mean (SD)` ~ ave + ' (' + std + ')',
      `Median (range)` ~ med + ' (' + min + ', ' + max + ')'
   ),
   lab = list(
     lab ~ name + '\n(N = ' + n + ')'
   ),
   na.rm_fac = na.rm,
   na.rm_num = na.rm,
   exclude_fac = NULL,
   exclude_name = NULL
){
   y <- groupwise( # groupwise.data.frame
      x,
      ...,
      na.rm = na.rm,
      # all = all,
      fun = fun,
      fac = fac,
      num = num,
      # lab = lab,
      na.rm_fac = na.rm_fac,
      na.rm_num = na.rm_num,
      exclude_fac = exclude_fac,
      exclude_name = exclude_name
   )
   y <- tablet(y, ..., all = all, lab = lab ) # tablet.groupwise
   y$`_tablet_name` <- as.character(y$`_tablet_name`)
   # check for prime target inheriting 'latex' and coerce _tablet_name accordingly
   fac <- sapply(x, is.factor)
   num <- sapply(x, is.numeric)
   col <- names(x)[fac | num]
   if(length(col)){
      prime <- col[[1]]
      targets <- intersect(c('title','label'), names(attributes(x[[prime]])))
      if(length(targets)){
         target <- targets[[1]]
         if(inherits(attr(x[[prime]], target), 'latex'))
            class(y$`_tablet_name`) <- union('latex', class(y$`_tablet_name`))
      }
   }
   y
}

#' Splice Some Things Together
#'
#' Spices some things together. Generic, with tablet-oriented method \code{\link{splice.data.frame}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @return see methods
#' @keywords internal
#' @examples
#' example(splice.data.frame)
splice <- function(x, ...)UseMethod('splice')

#' Splice A Data Frame
#'
#' Splices a data.frame.  If the data.frame has groups, tablet() is
#' called for each group in succession, only the last of which requests
#' 'all'.  The results are column-bound, and duplicate columns
#' are removed.
#' @param x data.frame
#' @param all a column name for ungrouped statistics; can have length zero to suppress ungrouped column
#' @param ... passed to \code{\link{tablet}}
#' @return tablet
#' @export
#' @keywords internal
#' @examples
#' library(boot)
#' library(dplyr)
#' library(magrittr)
#' melanoma %>%
#'   select(-time, -year) %>%
#'   mutate(sex = factor(sex), ulcer = factor(ulcer)) %>%
#'   mutate(status2 = ifelse(status == 2, 2, 4)) %>%
#'   group_by(status, status2) %>%
#'   splice
splice.data.frame <- function(x, all = 'All', ...){
   grp <- groups(x)
   if(!length(grp))return(tablet(x, ...))
   x <- ungroup(x)
   part <- list()
   for(g in seq_along(grp)){
      group <- grp[[g]]
      this <- group_by(x, !!group)
      for(i in setdiff(grp, list(group))) this[i] <- NULL
      tot <- all
      if(g < length(grp)) tot <- character(0) # 'all' only for last table
      that <- tablet(this, all = tot, ...)
      part[[group]] <- that
   }
   names(part) <- NULL
   out <- do.call(cbind, part)

   # remove dup columns
   dupcol <- rep(FALSE, ncol(out))
   if(ncol(out))
   for(i in seq_along(out)[-1]){
      for( j in 1:(i-1)){
         if(identical(out[[i]], out[[j]])){
            dupcol[[i]] <- TRUE
         }
      }
   }
   out <- out[,!dupcol, drop = FALSE]
   class(out) <- class(part[[1]])
   out
}

#' Escape Special Characters for Latex
#'
#' Escapes special characters in Latex context.  Generic, with method \code{link{escape_latex.default}}.
#'
#' @export
#' @keywords internal
#' @return see methods
#' @param x typically inherits character
#' @param ... passed arguments
#' @family escape
#' @examples
#' example(escape_latex.default)
escape_latex <- function(x, ...)UseMethod('escape_latex')

#' Escape Special Characters for Latex by Default
#'
#' Escapes text characters that have special meaning in latex.
#' Adapted with gratitude from KableExtra internals.
#' I.e. inactivates material that otherwise looks like latex.
#'
#' This function is used in \code{\link{as_kable.tablet}} in
#' preparation for a call to \code{\link[kableExtra]{kbl}}.
#' At kableExtra 1.3.4, sim_double_escape() only doubles
#' primary (leading) backslashes; the \code{secondary} argument
#' by default pre-doubles later backslashes to prevent
#' certain display errors.  Behavior may change if kableExtra changes.
#'
#' @export
#' @keywords internal
#' @return latex
#' @param x typically inherits character
#' @param secondary logical: whether secondary backslashes should be pre-doubled
#' @param ... ignored
#' @family escape
#' @examples
#' escape_latex('([#$%&_{}])')
escape_latex.default <- function(x, secondary = TRUE, ...){
   x = gsub("\\\\", "\\\\textbackslash", x)
   x = gsub("([#$%&_{}])", "\\\\\\1", x)
   x = gsub("\\\\textbackslash", "\\\\textbackslash{}", x)
   x = gsub("~", "\\\\textasciitilde{}", x)
   x = gsub("\\^", "\\\\textasciicircum{}", x)
   if(secondary){
      # hotfix for kableExtra::sim_double_escape @1.3.4
      # https://github.com/haozhu233/kableExtra/issues/622
      x = gsub('\\',  '\\\\', x, fixed = TRUE)
      x = sub( '\\\\','\\',   x, fixed = TRUE) # first will be doubled later by as_kable etc.
   }
   x <- as_latex(x)
   x
}

#' Escape Latex for Class 'latex'
#'
#' Returns argument typically unmodified.
#' Prevents accidental double-escaping
#' of the same text.
#'
#' This function is used in \code{\link{as_kable.tablet}} in
#' preparation for a call to \code{\link[kableExtra]{kbl}}.
#' At kableExtra 1.3.4, sim_double_escape() only doubles
#' primary (leading) backslashes; the \code{secondary} argument
#' by default pre-doubles later backslashes to prevent
#' certain display errors.  Behavior may change if kableExtra changes.

#' @export
#' @keywords internal
#' @return latex
#' @param x latex
#' @param secondary logical: whether secondary backslashes should be pre-doubled
#' @param ... ignored
#' @family escape
#' @examples
#' identical(
#' escape_latex('([#$%&_{}])'),
#' escape_latex(escape_latex('([#$%&_{}])'))
#' )
#'
escape_latex.latex <- function(x, secondary = TRUE, ...){
   if(secondary){
      # hotfix for kableExtra::sim_double_escape @1.3.4
      # https://github.com/haozhu233/kableExtra/issues/622
      x = gsub('\\',  '\\\\', x, fixed = TRUE)
      x = sub( '\\\\','\\',   x, fixed = TRUE) # first will be doubled later by as_kable etc.
   }
   x <- as_latex(x)
   x
}

