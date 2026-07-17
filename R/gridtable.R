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
#' We can integrate decorations:
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
#' Render a validated gridtable object to a calibrated data frame.
#' Columns have \code{width} attributes suggested by the grid table spans.
#'
#' @param x Character scalar with class \code{gridtable}.
#' @param row.names Optional row names for the returned data frame.
#' @param optional Logical scalar passed to \code{as.data.frame} when
#'   normalizing the parsed table.
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
#' @importFrom rmarkdown pandoc_convert
#' @importFrom rvest html_table
#' @importFrom xml2 read_html
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
  
  grid_table_column_widths <- function(table_text, scale) {
    lines <- strsplit(table_text, "\\r?\\n", perl = TRUE)[[1]]
    lines <- trimws(lines)
    lines <- lines[nzchar(lines)]
    boundary_rows <- lines[startsWith(lines, "+") & endsWith(lines, "+")]
    
    if (!length(boundary_rows)) {
      stop("No grid table boundary row found for width extraction.", call. = FALSE)
    }
    
    first_boundary <- boundary_rows[1L]
    inner <- substring(first_boundary, 2L, nchar(first_boundary) - 1L)
    spans <- strsplit(inner, "\\+", perl = TRUE)[[1]]
    
    if (!length(spans) || any(!grepl("^-+$", spans))) {
      stop("The first boundary row must contain only '-' spans for width extraction.", call. = FALSE)
    }
    
    dash_counts <- nchar(spans, type = "width")
    paste0(format(dash_counts * 0.44 * scale, trim = TRUE, scientific = FALSE), "em")
  }
  
  x <- as.character(x)
  
  column_widths <- grid_table_column_widths(x, scale = scale)
  input <- tempfile(fileext = ".md")
  output <- tempfile(fileext = ".html")
  on.exit(unlink(c(input, output)), add = TRUE)
  
  writeLines(x, input, useBytes = TRUE)
  
  rmarkdown::pandoc_convert(
    input = input,
    to = "html",
    from = "markdown",
    output = output,
    verbose = FALSE
  )
  
  tables <- rvest::html_table(
    xml2::read_html(output),
    header = TRUE,
    trim = TRUE,
    convert = FALSE
  )
  
  if (!length(tables)) {
    stop("No grid table found in Markdown string.", call. = FALSE)
  }
  
  out <- base::as.data.frame(tables[[1L]], stringsAsFactors = FALSE, optional = optional)
  if (!is.null(row.names)) {
    row.names(out) <- row.names
  }
  
  out[] <- lapply(out, function(column) {
    if (is.character(column)) {
      trimws(gsub("[[:space:]]+", " ", column))
    } else {
      column
    }
  })
  
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


#' Render a tablet with kbl
#'
#' Delegate tablet rendering to \code{\link{as_kable.tablet}}.
#'
#' @param x A data frame with class \code{tablet}.
#' @param ... Additional arguments passed to \code{\link{as_kable.tablet}}.
#'
#' @return A kable object.
#' @family gridtable
#' @seealso \code{\link{kbl}}, \code{\link[tablet]{as_kable}}
#' @export
kbl.tablet <- function(x, ...) {
  as_kable.tablet(x, ...)
}