#' Coerce to Gridtable
#'
#' Coerces to "gridtable", i.e. the Pandoc construct "grid table" Generic,
#' with method \code{\link{as_gridtable.character}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @return see methods
#' @family gridtable
#' @keywords internal
#' @examples
#' # as_gridtable() lets you create and render small data tables on-the-fly.
#' 
#' library(magrittr)
#' library(tablet)
#' library(yamlet)
#' library(kableExtra)
#' '+--+--+--+
#' |study | subjects | doses |
#' +==+==+==+
#' | 001 | 20 | 5 mg, 10 mg |
#' +--+--+--+
#' | 002 | 40 | 5 mg, 10 mg |
#' |     |    | 15 mg, 20 mg, 30 mg |
#' +--+--+--+' %>% 
#' as_gridtable
#' 
#' # We see that as_gridtable cleans up our Pandoc grid table markdown.
#' # We copy it here and render it with kableExtra::kbl().
#' 
#' '
#' +-------+----------+---------------------+
#' | study | subjects | doses               |
#' +=======+==========+=====================+
#' | 001   | 20       | 5 mg, 10 mg         |
#' +-------+----------+---------------------+
#' | 002   | 40       | 5 mg, 10 mg         |
#' |       |          | 15 mg, 20 mg, 30 mg |
#' +-------+----------+---------------------+
#' ' %>%
#' as_gridtable %>%
#' as.data.frame(scale = 0.9) %>% # or: options(tablet.gridtable.scale = 0.9)
#' decorations
#' 
#' # We see that a gridtable when converted to data.frame
#' # has scalable column width attributes.
#' # Widths are controlled by longest line of text, including column name.
#' # kbl will try to honor widths.
#' # kbl() will call as.data.frame() implicitly if you pass a gridtable:
#' 
#' '
#' +-------+----------+---------------------+
#' | study | subjects | doses               |
#' +=======+==========+=====================+
#' | 001   | 20       | 5 mg, 10 mg         |
#' +-------+----------+---------------------+
#' | 002   | 40       | 5 mg, 10 mg         |
#' |       |          | 15 mg, 20 mg, 30 mg |
#' +-------+----------+---------------------+
#' ' %>%
#' as_gridtable %>%
#' kbl(
#'   label = "studies",
#'   booktabs = TRUE,
#'   caption = "Study Characteristics" 
#' ) %>%
#'   kable_styling(font_size = 9) %>%
#'   footnote(
#'     threeparttable = TRUE,
#'     general_title = "",
#'     general = "For more details, see study protocols."
#'   )
#'   
#' # We can start with a data.frame:
#' 
#' data.frame(
#'   study = c('001','002'), 
#'   subjects = c(20, 40)
#' ) %>%
#' as_gridtable
#' 
#' # We can integrate decorations:
#' 
#' '
#' +-------+----------+
#' | study | subjects |
#' +=======+==========+
#' | 001   | 20       |
#' +-------+----------+
#' | 002   | 40       |
#' +-------+----------+
#' ' %>%
#' as_gridtable %>%
#' as.data.frame %>%
#' decorate('
#'   study: [ Study, [ ST001: "001", ST002: "002" ]]
#'   subjects: [ N Subjects ]
#' ') %>%
#' resolve %>%
#' kbl(
#'   label = "studies",
#'   booktabs = TRUE,
#'   caption = "Study Characteristics"
#' ) %>%
#' kable_styling()

as_gridtable <- function(x, ...)UseMethod('as_gridtable')

#' Coerce to Calibrated
#'
#' Coerces to calibrated output class. Generic, with flagship method
#' \code{\link{as_calibrated.data.frame}}. The intent is to attach
#' calibrated column widths for downstream table rendering.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @return see methods
#' @family gridtable
#' @keywords internal
as_calibrated <- function(x, ...)UseMethod("as_calibrated")

#' Render with kbl
#'
#' Generic wrapper for \code{\link[kableExtra]{kbl}}. The default method is the
#' existing \code{kableExtra::kbl} definition.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @return see methods
#' @family gridtable
#' @seealso \code{\link[kableExtra]{kbl}}
kbl <- function(x, ...)UseMethod("kbl")

#' Default kbl Method
#'
#' Calls the existing \code{\link[kableExtra]{kbl}} implementation.
#'
#' @param x Object to render.
#' @param format,digits,row.names,col.names,align,caption,label Arguments
#'   passed to \code{\link[kableExtra]{kbl}}.
#' @param format.args,escape,table.attr,booktabs,longtable,tabular,valign
#'   Arguments passed to \code{\link[kableExtra]{kbl}}.
#' @param position,centering,vline,toprule,bottomrule,midrule,linesep
#'   Arguments passed to \code{\link[kableExtra]{kbl}}.
#' @param caption.short,table.envir Arguments passed to
#'   \code{\link[kableExtra]{kbl}}.
#' @param ... Additional arguments passed to \code{\link[kableExtra]{kbl}}.
#' @export
#' @return A kable object.
#' @family gridtable
#' @seealso \code{\link[kableExtra]{kbl}}
kbl.default <- kableExtra::kbl

#' Validate and Normalize a Simple Pandoc Grid Table
#'
#' Validate a character string containing a simple Pandoc grid table and
#' rebuild it by parsing structural `+` and `|` delimiters, discarding the
#' original spacing, and returning an aligned grid table.
#'
#' The accepted grid-table form is intentionally restricted: every boundary row
#' must contain the same number of `+` delimiters; every text row is split on
#' every `|` and must therefore contain the same number of `|` delimiters; each
#' boundary span must contain at least one `-` or `=`; the first boundary row and
#' all body boundary rows must use `-`; the second boundary row must be the only
#' `=` boundary row; and exactly one header text row must appear between the
#' first and second boundary rows; header cells must be non-empty and
#' unique after trimming. Literal `|` characters inside cell text are interpreted
#' as delimiters, not cell content.
#'
#' @param x Character scalar containing one simple Pandoc grid table.
#' @param ... Currently unused.
#'
#' @return Character scalar with class \code{gridtable} containing a canonical Pandoc grid table.
#' @family gridtable
#' @export

as_gridtable.character <- function(x, ...) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop("`x` must be a non-missing character scalar.", call. = FALSE)
  }
  lines <- strsplit(x, "\\r?\\n", perl = TRUE)[[1]]
  
  while (length(lines) > 0L && !nzchar(trimws(lines[1L]))) {
    lines <- lines[-1L]
  }
  while (length(lines) > 0L && !nzchar(trimws(lines[length(lines)]))) {
    lines <- lines[-length(lines)]
  }
  
  lines <- sub("\\s+$", "", lines)
  
  if (length(lines) < 5L) {
    stop("Grid table must contain at least five non-blank lines.", call. = FALSE)
  }
  
  split_delimited <- function(value, delimiter) {
    positions <- gregexpr(delimiter, value, fixed = TRUE)[[1]]
    
    if (positions[1L] == -1L) {
      return(value)
    }
    
    substring(value, c(1L, positions + 1L), c(positions - 1L, nchar(value)))
  }
  
  parse_boundary <- function(line) {
    line <- trimws(line)
    
    if (!startsWith(line, "+") || !endsWith(line, "+")) {
      return(NULL)
    }
    
    inner <- substring(line, 2L, nchar(line) - 1L)
    spans <- split_delimited(inner, "+")
    
    if (!length(spans) || any(!nzchar(spans))) {
      stop("Each boundary span must contain at least one '-' or '='.", call. = FALSE)
    }
    if (any(!grepl("^[-=]+$", spans))) {
      stop("Boundary spans may contain only '-' or '=' characters.", call. = FALSE)
    }
    if (any(grepl("-", spans) & grepl("=", spans))) {
      stop("A boundary span may not mix '-' and '=' characters.", call. = FALSE)
    }
    
    kind <- if (all(grepl("^-+$", spans))) {
      "dash"
    } else if (all(grepl("^=+$", spans))) {
      "equals"
    } else {
      stop("A boundary row may not mix '-' and '=' spans.", call. = FALSE)
    }
    
    list(kind = kind, ncol = length(spans))
  }
  
  parse_text <- function(line, ncol) {
    line <- trimws(line)
    
    if (!startsWith(line, "|") || !endsWith(line, "|")) {
      stop("Each text row must start and end with '|'.", call. = FALSE)
    }
    
    inner <- substring(line, 2L, nchar(line) - 1L)
    cells <- split_delimited(inner, "|")
    
    if (length(cells) != ncol) {
      stop("Each text row must contain the same number of '|' delimiters.", call. = FALSE)
    }
    
    trimws(cells)
  }
  
  boundary <- lapply(lines, parse_boundary)
  is_boundary <- vapply(boundary, Negate(is.null), logical(1L))
  boundary_index <- which(is_boundary)
  
  if (!length(boundary_index) || boundary_index[1L] != 1L ||
      boundary_index[length(boundary_index)] != length(lines)) {
    stop("Grid table must start and end with a boundary row.", call. = FALSE)
  }
  if (length(boundary_index) < 3L) {
    stop("Grid table must contain a top border, header separator, and bottom border.", call. = FALSE)
  }
  
  boundary <- boundary[is_boundary]
  ncol <- boundary[[1L]]$ncol
  
  if (any(vapply(boundary, function(item) item$ncol != ncol, logical(1L)))) {
    stop("Every boundary row must define the same number of columns.", call. = FALSE)
  }
  if (boundary[[1L]]$kind != "dash") {
    stop("The first boundary row must use '-' spans.", call. = FALSE)
  }
  if (boundary[[2L]]$kind != "equals") {
    stop("The second boundary row must be the only '=' header separator.", call. = FALSE)
  }
  if (any(vapply(boundary[-2L], function(item) item$kind != "dash", logical(1L)))) {
    stop("Only the second boundary row may use '=' spans.", call. = FALSE)
  }
  
  if ((boundary_index[2L] - boundary_index[1L]) != 2L) {
    stop("Exactly one header text row must appear before the '=' separator.", call. = FALSE)
  }
  
  text_groups <- vector("list", length(boundary_index) - 1L)
  
  for (i in seq_along(text_groups)) {
    from <- boundary_index[i] + 1L
    to <- boundary_index[i + 1L] - 1L
    
    if (from > to) {
      stop("Each pair of boundary rows must contain at least one text row.", call. = FALSE)
    }
    
    text_groups[[i]] <- do.call(
      rbind,
      lapply(lines[from:to], parse_text, ncol = ncol)
    )
  }
  
  header_group <- text_groups[[1L]]
  header_names <- header_group[1L, ]
  if (any(!nzchar(header_names))) {
    stop("Grid table column names must be defined.", call. = FALSE)
  }
  if (any(duplicated(header_names))) {
    duplicate_names <- unique(header_names[duplicated(header_names)])
    stop(
      sprintf("Grid table column names must be unique; duplicated: %s.", paste(shQuote(duplicate_names), collapse = ", ")),
      call. = FALSE
    )
  }
  
  body_groups <- text_groups[-1L]
  col_widths <- rep(1L, ncol)
  
  for (j in seq_len(ncol)) {
    cells <- c(header_group[, j], unlist(lapply(body_groups, function(group) group[, j]), use.names = FALSE))
    col_widths[j] <- max(1L, nchar(cells, type = "width"))
  }
  
  pad_cell <- function(cell, width) {
    paste0(cell, strrep(" ", max(0L, width - nchar(cell, type = "width"))))
  }
  boundary_line <- function(char) {
    paste0("+", paste(strrep(char, col_widths + 2L), collapse = "+"), "+")
  }
  text_line <- function(cells) {
    paste0(
      "|",
      paste(paste0(" ", mapply(pad_cell, cells, col_widths, USE.NAMES = FALSE), " "), collapse = "|"),
      "|"
    )
  }
  
  normalized <- c(
    boundary_line("-"),
    text_line(header_group[1L, ]),
    boundary_line("=")
  )
  
  for (group in body_groups) {
    normalized <- c(
      normalized,
      apply(group, 1L, text_line),
      boundary_line("-")
    )
  }
  
  out <- paste(normalized, collapse = "\n")
  class(out) <- 'gridtable'
  return(out)
}

#' Convert a Data Frame to gridtable
#'
#' Build a simple Pandoc grid table from a data frame. Column names become the
#' grid-table header row, each data-frame row becomes one body row, and column
#' widths are calibrated from the widest formatted value in each column.
#'
#' @param x A data frame.
#' @param ... Currently unused.
#'
#' @return Character scalar with class \code{gridtable} containing a canonical
#'   Pandoc grid table.
#' @family gridtable
#' @seealso \code{\link{as_gridtable}}, \code{\link{as_gridtable.character}}
#' @method as_gridtable data.frame
#' @export
as_gridtable.data.frame <- function(x, ...) {
  cells <- base::as.data.frame(
    lapply(x, function(column) format(column, trim = TRUE, justify = "none")),
    stringsAsFactors = FALSE,
    optional = TRUE
  )
  names(cells) <- names(x)
  
  cells[] <- lapply(cells, function(column) {
    column[is.na(column)] <- "NA"
    column
  })
  
  if (!ncol(cells)) {
    stop("Grid tables require at least one column.", call. = FALSE)
  }
  if (!nrow(cells)) {
    stop("Grid tables require at least one data row.", call. = FALSE)
  }
  if (any(!nzchar(names(cells)))) {
    stop("Grid table column names must be defined.", call. = FALSE)
  }
  if (any(duplicated(names(cells)))) {
    duplicate_names <- unique(names(cells)[duplicated(names(cells))])
    stop(
      sprintf("Grid table column names must be unique; duplicated: %s.", paste(shQuote(duplicate_names), collapse = ", ")),
      call. = FALSE
    )
  }
  
  values <- rbind(names(cells), as.matrix(cells))
  if (any(grepl("[|\r\n]", values, perl = TRUE))) {
    stop("Grid table cells may not contain '|' or line breaks.", call. = FALSE)
  }
  
  col_widths <- pmax(1L, apply(values, 2L, function(column) {
    max(nchar(column, type = "width"))
  }))
  
  pad_cell <- function(cell, width) {
    paste0(cell, strrep(" ", max(0L, width - nchar(cell, type = "width"))))
  }
  boundary_line <- function(char) {
    paste0("+", paste(strrep(char, col_widths + 2L), collapse = "+"), "+")
  }
  text_line <- function(cells) {
    paste0(
      "|",
      paste(paste0(" ", mapply(pad_cell, cells, col_widths, USE.NAMES = FALSE), " "), collapse = "|"),
      "|"
    )
  }
  
  normalized <- c(
    boundary_line("-"),
    text_line(names(cells)),
    boundary_line("=")
  )
  
  for (row_index in seq_len(nrow(cells))) {
    normalized <- c(
      normalized,
      text_line(unname(unlist(cells[row_index, ], use.names = FALSE))),
      boundary_line("-")
    )
  }
  
  out <- paste(normalized, collapse = "\n")
  class(out) <- "gridtable"
  out
}

#' Print a gridtable
#'
#' Print a gridtable as plain grid-table text.
#'
#' @param x Character scalar with class \code{gridtable}.
#' @param ... Currently unused.
#'
#' @return Invisibly returns \code{x}.
#' @family gridtable
#' @export
print.gridtable <- function(x, ...) {
  writeLines(as.character(x))
  invisible(x)
}

#' Coerce a gridtable to a Calibrated Data Frame
#'
#' Parse a validated gridtable object directly to a calibrated data frame.
#' Columns have \code{width} attributes suggested by the grid table spans.
#' Continuation lines within a grid-table cell are collapsed with single spaces.
#' Cell markup is otherwise left untouched for downstream renderers.
#'
#' @param x Character scalar with class \code{gridtable}.
#' @param row.names Optional row names for the returned data frame.
#' @param optional Currently unused; accepted for compatibility with
#'   \code{as.data.frame}.
#' @param ... Currently unused.
#' @param scale Numeric scalar multiplier applied to grid-table dash counts
#'   when deriving column-level \code{width} attributes.
#'
#' @return A data frame with class \code{c("calibrated", "data.frame")}.
#'   Each column has a \code{width} attribute derived from the final grid
#'   table dash count multiplied by \code{scale} and encoded as an em width.
#' @family gridtable
#' @seealso \code{\link{as_gridtable.character}}, \code{\link{as_calibrated.data.frame}}
#' @method as.data.frame gridtable
#' @export
as.data.frame.gridtable <- function(
    x,
    row.names = NULL,
    optional = FALSE,
    ...,
    scale = getOption('tablet.gridtable.scale', 1)
){
  if (!is.numeric(scale) || length(scale) != 1L || is.na(scale) || !is.finite(scale) || scale <= 0) {
    stop("`scale` must be a positive finite numeric scalar.", call. = FALSE)
  }
  
  split_delimited <- function(value, delimiter) {
    positions <- gregexpr(delimiter, value, fixed = TRUE)[[1]]
    
    if (positions[1L] == -1L) {
      return(value)
    }
    
    substring(value, c(1L, positions + 1L), c(positions - 1L, nchar(value)))
  }
  
  parse_boundary <- function(line) {
    line <- trimws(line)
    if (!startsWith(line, "+") || !endsWith(line, "+")) {
      return(NULL)
    }
    inner <- substring(line, 2L, nchar(line) - 1L)
    split_delimited(inner, "+")
  }
  
  parse_text <- function(line, ncol) {
    line <- trimws(line)
    if (!startsWith(line, "|") || !endsWith(line, "|")) {
      stop("Each text row must start and end with '|'.", call. = FALSE)
    }
    inner <- substring(line, 2L, nchar(line) - 1L)
    cells <- split_delimited(inner, "|")
    if (length(cells) != ncol) {
      stop("Each text row must contain the same number of '|' delimiters.", call. = FALSE)
    }
    trimws(cells)
  }
  
  collapse_cell <- function(values) {
    values <- trimws(values)
    values <- values[nzchar(values)]
    if (!length(values)) {
      return("")
    }
    trimws(gsub("[[:space:]]+", " ", paste(values, collapse = " ")))
  }
  
  table_text <- as.character(as_gridtable(as.character(x)))
  lines <- strsplit(table_text, "\\r?\\n", perl = TRUE)[[1]]
  lines <- lines[nzchar(trimws(lines))]
  
  boundary <- lapply(lines, parse_boundary)
  is_boundary <- vapply(boundary, Negate(is.null), logical(1L))
  boundary_index <- which(is_boundary)
  boundary <- boundary[is_boundary]
  ncol <- length(boundary[[1L]])
  
  column_widths <- paste0(
    format(nchar(boundary[[1L]], type = "width") * 0.44 * scale, trim = TRUE, scientific = FALSE),
    "em"
  )
  
  text_groups <- vector("list", length(boundary_index) - 1L)
  for (i in seq_along(text_groups)) {
    from <- boundary_index[i] + 1L
    to <- boundary_index[i + 1L] - 1L
    text_groups[[i]] <- do.call(
      rbind,
      lapply(lines[from:to], parse_text, ncol = ncol)
    )
  }
  
  header_names <- text_groups[[1L]][1L, ]
  body_groups <- text_groups[-1L]
  values <- do.call(
    rbind,
    lapply(body_groups, function(group) {
      vapply(seq_len(ncol), function(column_index) {
        collapse_cell(group[, column_index])
      }, character(1L))
    })
  )
  
  out <- base::as.data.frame(values, stringsAsFactors = FALSE, optional = optional)
  names(out) <- header_names
  if (!is.null(row.names)) {
    row.names(out) <- row.names
  }
  
  as_calibrated(out, column_widths)
}

#' Calibrate Data Frame Column Widths
#'
#' Assign column-level \code{width} attributes to a data frame and return
#' a calibrated data frame for downstream table rendering. Widths are
#' recycled as necessary; a warning is issued when the number of supplied
#' widths differs from the number of columns.
#'
#' @param x A data frame.
#' @param width Character or numeric vector of column widths. Values are
#'   recycled as necessary.
#' @param ... Currently unused.
#'
#' @return A data frame with class \code{calibrated} and column-level
#'   \code{width} attributes.
#' @family gridtable
#' @seealso \code{\link{as_calibrated}}, \code{\link[kableExtra]{column_spec}}
#' @export
as_calibrated.data.frame <- function(x, width, ...) {
  if (missing(width)) {
    stop("`width` must be supplied.", call. = FALSE)
  }
  if (!is.atomic(width) || is.null(width) || is.matrix(width) || is.array(width)) {
    stop("`width` must be an atomic vector.", call. = FALSE)
  }
  
  width <- as.character(width)
  n_columns <- ncol(x)
  
  if (!length(width) && n_columns > 0L) {
    stop("`width` must contain at least one value.", call. = FALSE)
  }
  if (anyNA(width) || any(!nzchar(width))) {
    stop("`width` values must be non-missing and non-empty.", call. = FALSE)
  }
  if (length(width) < n_columns) {
    warning("Fewer widths than columns; recycling `width`.", call. = FALSE)
  } else if (length(width) > n_columns) {
    warning("More widths than columns; ignoring extra `width` values.", call. = FALSE)
  }
  
  if (n_columns > 0L) {
    width <- rep(width, length.out = n_columns)
    
    for (column_index in seq_len(n_columns)) {
      attr(x[[column_index]], "width") <- width[[column_index]]
    }
  }
  
  class(x) <- union("calibrated", class(x))
  x
}

#' Render a Calibrated Data Frame with kbl
#'
#' Render a calibrated data frame with \code{kbl} and optionally apply
#' column-level \code{width} attributes with \code{\link[kableExtra]{column_spec}}.
#'
#' @param x A data frame with class \code{calibrated}.
#' @param ... Additional arguments passed to \code{kbl.default}.
#' @param use_widths Logical scalar indicating whether column-level width
#'   attributes should be applied with \code{\link[kableExtra]{column_spec}}. Defaults to TRUE.
#'
#' @return A kable object.
#' @family gridtable
#' @seealso \code{\link{kbl}}, \code{\link{kbl.default}}, \code{\link[kableExtra]{column_spec}}
#' @importFrom kableExtra column_spec
#' @export
kbl.calibrated <- function(x, ..., use_widths = TRUE) {
  if (!is.logical(use_widths) || length(use_widths) != 1L || is.na(use_widths)) {
    stop('use_widths must be TRUE or FALSE.', call. = FALSE)
  }
  
  widths <- vapply(x, function(column) {
    width <- attr(column, 'width', exact = TRUE)
    
    if (is.null(width)) {
      return(NA_character_)
    }
    if (length(width) != 1L || is.na(width) || !nzchar(as.character(width))) {
      stop('Column width attributes must be non-missing scalar values.', call. = FALSE)
    }
    
    as.character(width)
  }, character(1L))
  
  out <- kbl.default(x, ...)
  
  if (!use_widths) {
    return(out)
  }
  
  for (column_index in which(!is.na(widths))) {
    out <- kableExtra::column_spec(
      out,
      column = column_index,
      width = widths[[column_index]]
    )
  }
  
  out
}


#' Render a gridtable with kbl
#'
#' Coerce a gridtable to a calibrated data frame with \code{as.data.frame}
#' before redispatching to \code{kbl}. This lets gridtable-derived column
#' widths be consumed by \code{kbl.calibrated}.
#'
#' @param x Character scalar with class \code{gridtable}.
#' @param ... Additional arguments passed to \code{kbl}.
#'
#' @return A kable object.
#' @family gridtable
#' @seealso \code{\link{kbl}}, \code{\link{kbl.calibrated}}, \code{\link{as.data.frame.gridtable}}
#' @export
kbl.gridtable <- function(x, ...) {
  kbl(as.data.frame(x), ...)
}


#' Render tablet with kbl()
#'
#' Renders a tablet.  Calls \code{\link[kableExtra]{kbl}} and implements
#' special features like grouped columns. 
#' See also \code{\link{tablet.data.frame}}.  
#' As of 0.9.0, calling \code{\link{kbl}} is equivalent to 
#' calling \code{\link{as_kable}}.
#'
# Column \code{_tablet_name} must inherit 'character' and
# by default (in a latex render context) its values
# will eventually be processed by \code{escape_latex}.
# Thus, elements of \code{_tablet_name} that appear to be latex
# will be handled internally by \code{\link{escape_latex.latex}}
# (which tries not to re-escape metacharacters).
#'
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
#' @param linebreaker passed to \code{\link[kableExtra]{linebreak}} for column names in latex; for html, linebreaker is replaced with <br/>
#' @param pack_rows named list passed to \code{\link[kableExtra]{pack_rows}} for finer control of variable names
#' @param secondary passed to escape_latex
#' @importFrom kableExtra kbl pack_rows add_header_above linebreak
#' @importFrom dplyr rename group_vars
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
#'   kbl
#'
#' x <- system.file(package = 'tablet', 'shiny-examples/mesa/data/adsl.sas7bdat')
#' x %<>% read_sas %>% data.frame
#' decorations(x) 
# note weight in pounds
# x %<>% mutate(weight = signif(digits = 3, weight * 2.2))
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
#' x %>% tablet %>% kbl
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
#' x %>% tablet %>% kbl


kbl.tablet <- function(
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
    pack_rows = list(escape = escape),
    secondary = FALSE
){
  x <- tablette(x, ...)
  # if(is.na(escape)){
  #    if (knitr::is_latex_output()){
  #      escape <- FALSE
  #    } else {
  #       escape <- FALSE # for <br>
  #    }
  # }
  stopifnot(is.logical(escape), length(escape) == 1)
  # x$`_tablet_sort` <- NULL
  index <- index(x)
  # draws on _tablet_name, which should be character
  nmsi <- names(index) # isolate to assign class
  #stopifnot(is.character(x$`_tablet_name`))
  # class(nmsi) <- class(x$`_tablet_name`) # class propagation irrelevant with 0.7.2
  x$`_tablet_name` <- NULL # done
  if(!escape){
    if (knitr::is_latex_output()) {
      # invokes type-specific function,
      # possibly escaping or ignoring latex metacharacters
      # revisit if kableExtra changes
      nmsi <- escape_latex(nmsi, secondary = TRUE, primary = TRUE) 
      if (linebreak){
        nmsi <- linebreak(
          nmsi,
          align = 'l',
          double_escape = TRUE,
          linebreaker = linebreaker
        )
      }
    } else {
      nmsi <- escape_html(nmsi)
    }
  }
  # nmsi now as informed as possible ... assign back
  names(index) <- nmsi
  #x$`_tablet_level` <- as.character(x$`_tablet_level`)
  # x$`_tablet_stat` <- as.character(x$`_tablet_stat`)
  # x$`_tablet_level` <- ifelse(
  #    x$`_tablet_level` == 'numeric',
  #    x$`_tablet_stat`,
  #    x$`_tablet_level`
  # )
  # x$`_tablet_stat` <- NULL
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
  
  
  # escape is false by default to allow internal discretion
  # here we handle the escaping of column names
  if(!escape){
    if (knitr::is_latex_output()) {
      # @ 0.6.10: apparently secondary should be FALSE, now default. 
      x[] <- lapply(x, escape_latex, secondary = secondary, ...)
      these <- names(x)
      # if('latex' %in% attr(x,'name_class')){
      #   class(these) <- c('latex','character')
      # }
      these <- escape_latex(these, secondary = FALSE, ...)
      names(x) <- these
    } else {
      x[] <- lapply(x, escape_html, ...)
      names(x) <- escape_html(names(x), ...)
    }
  }
  
  # names of each element in headerlist derive from the content
  # of grouping variables, and are expected to have 
  # exactly the same escaping needs as names(x)
  if(!escape){
    for(i in seq_along(headerlist)){
      if (knitr::is_latex_output()) {
        these <- names(headerlist[[i]])
        # if('latex' %in% attr(x,'name_class')){
        #   class(these) <- c('latex','character')
        # }
        these <- escape_latex(these, secondary = FALSE, ...)
        names(headerlist[[i]]) <- these
      } else {
        names(headerlist[[i]]) <- escape_html(names(headerlist[[i]]), ...)
      }
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
      col.names <- gsub(linebreaker,'<br/>', col.names)
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
    this <- headerlist[[i]]
    
    if(linebreak){
      if(knitr::is_latex_output()){
        names(this) <- linebreak(
          names(this), 
          align = align, 
          double_escape = double_escape, 
          linebreaker = linebreaker
        )
      } else {
        names(this) <- gsub(linebreaker, '<br/>', names(this))
      }
    }
    
    # kableExtra:::pdfTable_add_header_above()
    # uses str_replace(string, pattern, replacement)
    # which silently deletes orphan backslashes,
    # i.e. backslashes not followed by an integer
    # therefore, names(this) must have all backslash doubled
    # @ 0.6.7 2024-03-22
    # next action only for latex?
    if(knitr::is_latex_output()){
      names(this) <- gsub('\\','\\\\', names(this), fixed = TRUE)
    }
    y <- add_header_above(y, this, escape = escape)
  }
  
  # at 0.5.7, skip this if length(index) == 0
  # attempt to prevent error in mesa() when
  # tabulating virtual category agegr1 in isolation:
  # Error in `$<-.data.frame`(`*tmp*`, "start", value = 1) : replacement has 1 row, data has 0
  
  #if(length(index) > 0){
  y <- do.call(
    kableExtra::pack_rows,
    c(
      list(y, index = index), # @0.4.9 removing ', escape = escape
      pack_rows
    )
  )
  # }
  y
}
