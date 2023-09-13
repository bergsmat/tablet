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

#' Convert to tablette from tablet.
#' 
#' Converts to 'tablette' from 'tablet'.
#' I.e., makes compact data.frame that emulates
#' the layout expected by \code{\link{as_kable}}.
#' Preserves attributes 'n', 'label', 'codelist', and 'latex' class if present.
#'
#' @export
#' @keywords internal
#' @family tablet
#' @param x object of dispatch
#' @param ... passed arguments
#' @return tablette : a data.frame
#'  with columns for each combination of groups, and:
#' \item{_tablet_name}{observation identifier: character, possibly 'latex'; 
#' has a codelist attribute the values of which are the original column names}
#' \item{_tablet_level}{factor level, or the LHS of formulas in 'num'}
# \item{_tablet_stat}{the LHS of formulas in 'fac' and 'num'}
#' \item{All (or value of 'all' argument)}{ungrouped results}
# \item{_tablet_sort}{sorting column}

#' @examples
#' library(boot)
#' library(dplyr)
#' library(magrittr)
#' library(yamlet)
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
#' melanoma %>% header_rows
#' melanoma %>% as_kable
#' melanoma %>% tablette
#' melanoma %>% tablette %>% tablet
#' identical(melanoma, tablet(tablette(melanoma)))
#'   
tablet.tablette <- function(x, ...){
  
  # capture supported attributes
  latex <- inherits(x[[1]],'latex')
  label <- function(x)attr(x,'label')
  count <- function(x)attr(x, 'n')
  codes <- function(x)attr(x, 'codelist')
  labels <- lapply(x, label)
  counts <- lapply(x, count)
  cdlist <- lapply(x, codes)
  
  # coerce to character, preserve label, codelist, and subclass
  for(col in seq_len(ncol(x))){
   # if(!inherits(x[[col]], 'character')){
      label <- attr(x[[col]], 'label')
      codelist <- attr(x[[col]], 'codelist')
      nest <- attr(x[[col]], 'nest')
      x[[col]] %<>% as.character
      x[[col]] %<>% as_dvec(label = label, codelist = codelist, nest = nest)
      # attr(col, 'label') <- label
      # attr(col, 'codelist') <- codelist
    #}
  }
  
  #capture nested column names
  names(x)[[1]] <- ' '
  names(x)[[2]] <- ' '
  nms <- as.list(names(x))
  for(i in seq_along(nms)){
    nest <- attr(x[[i]], 'nest')
    attr(x[[i]], 'nest') <- NULL
    for (j in rev(seq_along(nest))){
      nms[[ i ]][[ j + 1 ]] <- nest[[j]]
    }
  }
  
  #longest header
  head_len <- max(sapply(nms, length))
  
  # pad short headers
  for(i in seq_along(nms)){
    have <- nms[[i]]
    need <- head_len - length(have)
    pad <- rep(' ', need)
    nms[[i]] <- c(have, pad)
  }
  
  # bind headers
  header <- data.frame(do.call(cbind, nms))
  header <- header[rev(seq_len(nrow(header))),]
  
  # sparse headers
  for(i in seq_len(nrow(header))){
    for(j in ncol(header):2){
      if(
        header[i, j] == header[i, j - 1] &
        header[i, j] != ' ' # deliberately blank
        
      ){
        header[i, j] <- ''
      }
    }
  }

  # repeat column groups
  need <- 1 + c(1, x[[1]][-1] != x[[1]][-length(x[[1]])])
  x <- x[rep(rownames(x), times = need),]
  x$tablet_need <- NULL
  
  # sparse
  first <- c(1, x[[1]][-1] != x[[1]][-length(x[[1]])])
  for(i in seq_along(first)){
    prime <- first[[i]]
    if(!prime){
      x[i, 1] <- ''
    } else {
      for(j in 2:ncol(x)){
        x[i,j] <- ''
      }
    }
  }
  
  names(header) <- paste0('c', seq_len(ncol(header)))
  stopifnot(ncol(header) == ncol(x))
  names(x) <- names(header)
  # support latex
  class(header[[1]]) <- class(x[[1]])
  
  y <- bind_rows(x %>% slice(0), header, x)
  rownames(y) <- NULL
  if(latex){
    class(y[[1]]) <- union('latex', class(y[[1]]))
  }
  stopifnot(length(labels) == ncol(y))
  for(col in seq_len(ncol(y))){
    attr(y[[col]], 'label') <- labels[[col]]
    attr(y[[col]], 'n')     <- counts[[col]]
    attr(y[[col]], 'codelist') <- cdlist[[col]]
    
  }
  class(y) <- setdiff(class(y),'tablette')
  y <- as_tablet(y, ...)
  y
}
  
#' Identify Header Rows
#' 
#' Identifies header rows.
#' Generic, with method \code{\link{header_rows.tablet}}.
#' 
#' @export
#' @param x object of dispatch
#' @param ... passed arguments
#' @keywords internal
#' @family tablet
#' @keywords internal
header_rows <- function(x, ...)UseMethod('header_rows')

#' Identify Header Rows for tablet
#' 
#' Identifies header rows for tablet.
#' 
#' @export
#' @param x tablet
#' @param ... ignored
#' @family tablet
#' @return integer: indices for those rows representing headers
header_rows.tablet <- function(x, ...){
  nodes <- !is.na(x[[1]]) & x[[1]] != ' '
  node <- match(TRUE, nodes)
  if(is.na(node))stop('no content in column 1')
  num_head <- node - 1
  dex <- seq_len(num_head)
  dex
}



#' Convert to tablette from tablet
#' 
#' Converts to tablette from tablet.
#' Intends to be the inverse of \code{\link{tablet.tablette}}. 
#' 
#' @export
#' @keywords internal
#' @family tablette
#' @family tablet
#' @return tablet data.frame
#' @param x tablette
#' @param ... passed arguments
#' @examples
#' example(tablet.tablette)
tablette.tablet <- function(x, ...){
  stopifnot(ncol(x) >= 2)
  stopifnot(nrow(x) >= 2)
  latex <- inherits(x[[1]], 'latex')
  
  # standarize name column
  x[[1]][!is.na(x[[1]]) & x[[1]] == ''] <- NA
  
  # capture key structure
  nodes <- !is.na(x[[1]]) & x[[1]] != ' '
  headers <- header_rows(x)
  # node <- match(TRUE, nodes)
  # if(is.na(node))stop('no content in column 1')
  # num_head <- node - 1
  
  # un-sparse column groups
  for(i in 2:nrow(x)){
    if(is.na(x[i, 1])){
      x[i, 1] <- x[ i - 1, 1]
    }
  }
  
  # un-sparse headers
  if(ncol(x) > 3){
    for(i in header_rows(x)){
      for(j in 4:ncol(x)){
        if(
          is.na(x[i,j]) | x[i,j] == ''){
          x[i,j] <- x[i, j - 1]
        }
      }
    }
  }
  
  # remove node rows
  y <- x # preserve attributes
  x <- x[!nodes, , drop = FALSE]
  
  
  # isolate headers
  h <- x[headers, , drop = FALSE]
  x <- x[-headers, , drop = FALSE]
  
  # restore attributes
  for(col in seq_len(ncol(x))){
    for (at in c('class', 'codelist', 'n', 'label')){
      attr(x[[col]], at) <- attr(y[[col]], at)
    }
  }
  
  # build header nests
  h <- h[rev(rownames(h)), , drop = FALSE]
  names(x) <- h[1,]
  names(x)[1:2] <- c('_tablet_name', '_tablet_level')
  h <- h[-1, , drop = FALSE]
  if(ncol(x) >= 3){
    for(j in 3:ncol(x)){
      nest <- h[, j]
      # remove backstop padding
      backstop <- !is.na(nest) & nest == ' '
      nest <- nest[!backstop]
      if(length(nest)){
        attr(x[[j]], 'nest') <- nest
      }
    }
  }
  rownames(x) <- NULL
  if(latex){
    class(x[[1]]) <- union('latex', class(x[[1]]))
  }
  class(x) <- setdiff(class(x),'tablet')
  class(x) <- union('tablette', class(x))
  x
}


#' Coerce to tablet
#' 
#' Coerces to tablet.
#' Generic, with method \code{\link{as_tablet.data.frame}}.
#' 
#' @export
#' @keywords internal
#' @family tablet
#' @return tablet
#' @param x object of dispatch
#' @param ... passed arguments
as_tablet <- function(x, ...)UseMethod('as_tablet')

#' Coerce data.frame to tablet
#' 
#' Coerces data.frame to tablet.  Checks format and assigns the class.
#' See \code{\link{tablet.data.frame}}.
#' 
#' @export
#' @family tablet
#' @return tablet
#' @param x data.frame
#' @param ... passed arguments
as_tablet.data.frame <- function(x, ...){
  stopifnot(ncol(x) >= 2)
  stopifnot(nrow(x) >= 2)
  class(x) <- union('tablet', class(x))
  x
}

