library(shiny)
library(shinyFiles)
library(fs)
library(haven)
library(tablet)
library(dplyr)
library(magrittr)
library(yaml)
library(sortable)
library(kableExtra)
library(tools)
library(csv)

MAX_BINS <- 20

# ---- helpers -----------------------------------------------------------

printer <- function(x) writeLines(as.character(x))

read_data <- function(path) {
  ext <- tolower(tools::file_ext(path))
  x <- switch(
    ext,
    xpt       = haven::read_xpt(path),
    sas7bdat  = haven::read_sas(path),
    csv       = csv::as.csv(path),
    stop('unsupported file type: ', ext, call. = FALSE)
  )
  x <- as.data.frame(x)
  # preserve variable labels, but strip haven_labelled value-label baggage
  x[] <- lapply(x, function(col) {
    lab <- attr(col, 'label', exact = TRUE)
    if (inherits(col, 'haven_labelled')) col <- as.vector(col)
    if (!is.null(lab)) attr(col, 'label') <- lab
    col
  })
  x
}

ext_swap <- function(path, new_ext) {
  sub('\\.[^.]+$', paste0('.', new_ext), path)
}

# deparse() line-wraps long vectors, returning a multi-element character
# vector; sprintf() then recycles over it silently. Force a single line.
dep1 <- function(x) paste(deparse(x), collapse = ' ')

default_conf <- function() {
  list(
    filepath   = character(0),
    confpath   = character(0),
    checksum   = character(0),
    facet1     = character(0),
    facet2     = character(0),
    bins       = list(list(columns = character(0), separator = '/\n', width = '2cm')),
    title      = 'Title',
    outputid   = 'L-00-00',
    lhead1     = 'Company',
    lhead2     = 'Project',
    rhead1     = 'Confidential',
    rhead2     = 'Draft',
    footnotes  = '',
    cont       = '(continued)',
    output_dir = character(0),
    render_pdf = TRUE,
    tablet     = as.character(utils::packageVersion('tablet'))
  )
}

label_of <- function(data, nm) {
  lab <- attr(data[[nm]], 'label', exact = TRUE)
  if (is.null(lab) || !nzchar(lab)) nm else lab
}

# build the R source text of display() plus the facet-looping "sections" chunk
build_qmd_body <- function(conf) {
  x <- conf$x
  bins <- Filter(function(b) length(b$columns) > 0, conf$bins)
  if (!length(bins)) stop('at least one non-empty bin is required', call. = FALSE)

  keys <- character(0)
  headers <- character(0)
  widths <- character(0)
  collapsed_keys <- character(0)
  collapse_lines <- character(0)

  for (bin in bins) {
    cols <- bin$columns
    key <- cols[[1]]
    sep <- bin$separator
    keys <- c(keys, key)
    headers <- c(headers, paste(vapply(cols, label_of, character(1), data = x), collapse = sep))
    widths <- c(widths, bin$width)
    if (length(cols) > 1) {
      collapsed_keys <- c(collapsed_keys, key)
      col_refs <- paste0('x[[', vapply(cols, deparse, character(1)), ']]')
      collapse_lines <- c(
        collapse_lines,
        sprintf(
          "x[[%s]] <- kableExtra::linebreak(tablet::escape_latex(paste(%s, sep = %s)), align = 'l')",
          dep1(key), paste(col_refs, collapse = ', '), dep1(sep)
        )
      )
      drop_cols <- cols[-1]
      collapse_lines <- c(
        collapse_lines,
        sprintf(
          'x <- dplyr::select(x, -dplyr::all_of(%s))',
          dep1(drop_cols)
        )
      )
    }
  }

  read_datafile <- switch(
    tolower(tools::file_ext(conf$filepath)),
    xpt      = 'x <- haven::read_xpt(datafile)',
    sas7bdat = 'x <- haven::read_sas(datafile)',
    csv      = 'x <- csv::as.csv(datafile)',
    stop('unsupported file type', call. = FALSE)
  )

  setup_lines <- c(
    sprintf('datafile <- %s', dep1(conf$filepath)),
    read_datafile,
    'x <- as.data.frame(x)',
    "x[] <- lapply(x, function(col) if (inherits(col, 'haven_labelled')) as.vector(col) else col)"
  )

  footnote_lines <- character(0)
  feet <- Filter(nzchar, strsplit(conf$footnotes, '\n')[[1]])
  if (length(feet)) {
    footnote_lines <- sprintf(
      "out <- kableExtra::footnote(out, general = %s, general_title = ' ', threeparttable = TRUE, fixed_small_size = TRUE)",
      dep1(feet)
    )
  }

  display_lines <- c(
    'display <- function(x) {',
    paste0('  ', collapse_lines),
    sprintf(
      "  x <- dplyr::mutate(x, dplyr::across(dplyr::where(is.character) & !dplyr::any_of(%s), tablet::escape_latex))",
      dep1(collapsed_keys)
    ),
    sprintf('  x <- dplyr::select(x, dplyr::all_of(%s))', dep1(keys)),
    sprintf('  x <- tablet::as_calibrated(x, width = %s)', dep1(unname(widths))),
    '  out <- tablet::kbl(',
    '    x,',
    '    booktabs = TRUE,',
    '    longtable = TRUE,',
    "    linesep = '',",
    '    escape = FALSE,',
    sprintf("    col.names = kableExtra::linebreak(tablet::escape_latex(%s), align = 'l')", dep1(unname(headers))),
    '  )',
    "  out <- kableExtra::kable_styling(out, latex_options = 'repeat_header')",
    paste0('  ', footnote_lines),
    '  out',
    '}'
  )

  facet1 <- if (length(conf$facet1)) dep1(conf$facet1) else 'NULL'
  facet2 <- if (length(conf$facet2)) dep1(conf$facet2) else 'NULL'

  sections_lines <- c(
    sprintf('facet1 <- %s', facet1),
    sprintf('facet2 <- %s', facet2),
    'f1_values <- if (!is.null(facet1)) sort(unique(as.character(x[[facet1]]))) else NA_character_',
    'for (f1 in f1_values) {',
    '  x1 <- x',
    '  if (!is.na(f1)) {',
    "    cat('\\n\\n## ', f1, '\\n\\n', sep = '')",
    '    x1 <- dplyr::filter(x1, as.character(.data[[facet1]]) == f1)',
    '    x1 <- dplyr::select(x1, -dplyr::all_of(facet1))',
    '  }',
    '  f2_values <- if (!is.null(facet2)) sort(unique(as.character(x1[[facet2]]))) else NA_character_',
    '  for (f2 in f2_values) {',
    '    x2 <- x1',
    '    if (!is.na(f2)) {',
    "      cat('\\n\\n### ', f2, '\\n\\n', sep = '')",
    '      x2 <- dplyr::filter(x2, as.character(.data[[facet2]]) == f2)',
    '      x2 <- dplyr::select(x2, -dplyr::all_of(facet2))',
    '    }',
    '    print(display(x2))',
    "    cat('\\n\\n')",
    '  }',
    '}'
  )

  paste(
    c(
      '```{r}',
      '#| label: data',
      '#| include: false',
      '',
      setup_lines,
      '```',
      '',
      '```{r}',
      '#| label: toolchain',
      '#| include: false',
      '',
      display_lines,
      '```',
      '',
      '```{r}',
      '#| label: sections',
      "#| results: asis",
      '',
      sections_lines,
      '```'
    ),
    collapse = '\n'
  )
}

build_qmd <- function(conf, template_path) {
  template <- paste(readLines(template_path, warn = FALSE), collapse = '\n')
  body <- build_qmd_body(conf)
  checksum <- unname(tools::md5sum(conf$filepath))
  tokens <- c(
    '__TITLE__'        = conf$title,
    '__LHEAD1__'       = conf$lhead1,
    '__LHEAD2__'       = conf$lhead2,
    '__RHEAD1__'       = conf$rhead1,
    '__RHEAD2__'       = conf$rhead2,
    '__OUTPUTID__'     = conf$outputid,
    '__DATABASENAME__' = basename(conf$filepath),
    '__CHECKSUM__'     = checksum,
    '__FILEPATH__'     = conf$filepath,
    '__BODY__'         = body
  )
  for (i in seq_along(tokens)) {
    template <- gsub(names(tokens)[i], tokens[[i]], template, fixed = TRUE)
  }
  template
}

# ---- ui -----------------------------------------------------------------

ui <- shinyUI(
  navbarPage(
    'Listado',
    tabPanel(
      'Input',
      sidebarLayout(
        sidebarPanel(
          width = 5,
          shinyFilesButton(
            id = 'source',
            label = 'data or configuration',
            title = 'choose data (*.xpt, *.sas7bdat, *.csv) or configuration (*.conf) file:',
            multiple = FALSE
          ),
          textOutput('filepath'),
          br(),
          textOutput('confpath'),
          br(),
          textOutput('checksum'),
          br(),
          uiOutput('saveconfig')
        ),
        mainPanel(width = 0)
      )
    ),
    tabPanel(
      'Columns',
      sidebarLayout(
        sidebarPanel(
          width = 3,
          uiOutput('facet1'),
          uiOutput('facet2'),
          uiOutput('add_bin')
        ),
        mainPanel(
          width = 9,
          uiOutput('buckets'),
          uiOutput('bin_settings')
        )
      )
    ),
    tabPanel(
      'Headers/Footers',
      sidebarLayout(
        sidebarPanel(
          width = 6,
          uiOutput('title_ui'),
          uiOutput('outputid_ui'),
          uiOutput('lhead1_ui'),
          uiOutput('lhead2_ui'),
          uiOutput('rhead1_ui'),
          uiOutput('rhead2_ui'),
          uiOutput('cont_ui'),
          uiOutput('footnotes_ui')
        ),
        mainPanel(width = 0)
      )
    ),
    tabPanel(
      'Generate',
      sidebarLayout(
        sidebarPanel(
          width = 6,
          shinyDirButton(
            id = 'outdir',
            label = 'choose output directory',
            title = 'choose output directory:'
          ),
          textOutput('outdirpath'),
          br(),
          uiOutput('out_filename_ui'),
          uiOutput('render_pdf_ui'),
          actionButton('generate', 'Generate'),
          br(), br(),
          verbatimTextOutput('generate_status')
        ),
        mainPanel(width = 0)
      )
    )
  )
)

# ---- server ---------------------------------------------------------------

server <- shinyServer(function(input, output, session) {

  defaults <- default_conf()
  conf <- do.call(reactiveValues, defaults)

  reset_conf <- function(keep_filepath = FALSE) {
    fresh <- default_conf()
    for (nm in names(fresh)) {
      if (keep_filepath && nm == 'filepath') next
      conf[[nm]] <- fresh[[nm]]
    }
    conf$x <- data.frame()
  }
  conf$x <- data.frame()

  volumes <- getVolumes()
  moreVolumes <- function() c(
    volumes(),
    examples = system.file('shiny-examples/listado/data', package = 'tablet'),
    home = fs::path_home(),
    R = R.home()
  )
  ui_volumes <- reactive({
    vols <- moreVolumes()
    for (p in list(conf$filepath, conf$confpath)) {
      if (length(p) && !any(is.na(p))) {
        sel_path <- dirname(p)
        if (!sel_path %in% vols) {
          vols <- setNames(c(sel_path, vols), c(basename(sel_path), names(vols)))
        }
      }
    }
    vols
  })

  # ---- input tab: file chooser ------------------------------------------

  shinyFileChoose(
    input, 'source', roots = ui_volumes, session = session,
    filetypes = c('sas7bdat', 'csv', 'xpt', 'conf')
  )

  observeEvent(input$source, {
    req(input$source)
    newsource <- parseFilePaths(ui_volumes, input$source)$datapath
    if (!is.character(newsource) || !length(newsource) || !file.exists(newsource)) return()

    if (grepl('\\.conf$', newsource)) {
      conf$confpath <- newsource
    } else {
      reset_conf()
      conf$filepath <- newsource
    }
  })

  # loading data (and auto-loading / auto-creating sibling .conf)

  observeEvent(conf$filepath, {
    if (!length(conf$filepath)) return()
    if (!file.exists(conf$filepath)) return()

    conf$x <- read_data(conf$filepath)
    conf$checksum <- unname(tools::md5sum(conf$filepath))

    sibling <- ext_swap(conf$filepath, 'conf')
    if (file.exists(sibling)) {
      conf$confpath <- sibling
    } else {
      # auto-create a default configuration alongside the data file
      vals <- default_conf()
      vals$filepath <- relativizePath(conf$filepath, dirname(sibling))
      vals[c('x', 'checksum')] <- NULL
      res <- try(write_yaml(vals, sibling), silent = TRUE)
      if (!inherits(res, 'try-error')) {
        conf$confpath <- sibling
      }
    }
  })

  # loading a .conf directly (or one just auto-created / matched above)

  observeEvent(conf$confpath, {
    if (!length(conf$confpath) || !file.exists(conf$confpath)) return()

    saved <- list()
    tryCatch(
      saved <- read_yaml(conf$confpath),
      error = function(e) showNotification(duration = NULL, type = 'error', as.character(e))
    )
    if (!length(saved) || is.null(saved$filepath)) return()

    datapath <- absolutizePath(saved$filepath, dirname(conf$confpath))
    if (!file.exists(datapath)) {
      showNotification(duration = NULL, type = 'error', 'configured file path not found')
      return()
    }

    if (!identical(datapath, conf$filepath)) {
      conf$x <- read_data(datapath)
      conf$checksum <- unname(tools::md5sum(datapath))
    }
    conf$filepath   <- datapath
    conf$facet1     <- if (is.null(saved$facet1)) character(0) else saved$facet1
    conf$facet2     <- if (is.null(saved$facet2)) character(0) else saved$facet2
    conf$bins       <- if (is.null(saved$bins) || !length(saved$bins)) list(list(columns = character(0), separator = '/\n', width = '2cm')) else saved$bins
    conf$title      <- saved$title      %||% conf$title
    conf$outputid   <- saved$outputid   %||% conf$outputid
    conf$lhead1     <- saved$lhead1     %||% conf$lhead1
    conf$lhead2     <- saved$lhead2     %||% conf$lhead2
    conf$rhead1     <- saved$rhead1     %||% conf$rhead1
    conf$rhead2     <- saved$rhead2     %||% conf$rhead2
    conf$footnotes  <- saved$footnotes  %||% conf$footnotes
    conf$cont       <- saved$cont       %||% conf$cont
    conf$output_dir <- if (is.null(saved$output_dir) || !length(saved$output_dir)) character(0) else absolutizePath(saved$output_dir, dirname(conf$confpath))
    conf$render_pdf <- if (is.null(saved$render_pdf)) conf$render_pdf else saved$render_pdf

    if (!is.null(saved$tablet) && !identical(saved$tablet, conf$tablet)) {
      showNotification(
        duration = NULL, type = 'warning',
        paste('configuration was last saved by tablet version', saved$tablet, 'but currently using', conf$tablet)
      )
    }
  })

  `%||%` <- function(a, b) if (is.null(a)) b else a

  output$filepath <- renderPrint({
    if (!length(conf$filepath)) cat('No input data selected.') else cat(conf$filepath)
  })
  output$confpath <- renderPrint({
    if (!length(conf$confpath)) cat('No configuration file yet.') else cat(conf$confpath)
  })
  output$checksum <- renderPrint({
    if (!length(conf$checksum)) cat('') else cat('MD5:', conf$checksum)
  })

  output$saveconfig <- renderUI({
    if (!length(conf$filepath)) return(NULL)
    shinySaveButton(
      id = 'saveconf',
      label = 'save configuration',
      title = 'save configuration as:',
      filetype = list(conf = 'conf'),
      filename = paste0(tools::file_path_sans_ext(basename(conf$filepath)), '.conf')
    )
  })

  observeEvent(input$saveconf, {
    shinyFileSave(input, 'saveconf', roots = ui_volumes, session = session)
    fileinfo <- parseSavePath(ui_volumes, input$saveconf)
    if (!nrow(fileinfo)) return()
    path <- as.character(fileinfo$datapath)

    vals <- isolate(reactiveValuesToList(conf))
    vals <- vals[c(
      'filepath', 'facet1', 'facet2', 'bins',
      'title', 'outputid', 'lhead1', 'lhead2', 'rhead1', 'rhead2',
      'footnotes', 'cont', 'output_dir', 'render_pdf', 'tablet'
    )]
    vals$filepath <- relativizePath(vals$filepath, dirname(path))
    if (length(vals$output_dir)) vals$output_dir <- relativizePath(vals$output_dir, dirname(path))

    res <- try(write_yaml(vals, path), silent = TRUE)
    ok <- !inherits(res, 'try-error')
    showNotification(
      duration = if (ok) 5 else 10,
      type = if (ok) 'default' else 'error',
      ui = paste(if (ok) 'wrote' else 'did not write', path)
    )
    if (ok) conf$confpath <- path
  })

  # ---- columns tab: facets + bins ----------------------------------------

  used_columns <- reactive({
    unique(c(conf$facet1, conf$facet2, unlist(lapply(conf$bins, `[[`, 'columns'))))
  })

  output$facet1 <- renderUI({
    nms <- names(conf$x)
    if (!length(nms)) return(NULL)
    choices <- c('(none)', setdiff(nms, unique(c(conf$facet2, unlist(lapply(conf$bins, `[[`, 'columns'))))))
    selectInput('facet1', 'Facet 1', choices = choices, selected = if (length(conf$facet1)) conf$facet1 else '(none)')
  })

  output$facet2 <- renderUI({
    if (!length(conf$facet1)) return(NULL)
    nms <- names(conf$x)
    choices <- c('(none)', setdiff(nms, unique(c(conf$facet1, unlist(lapply(conf$bins, `[[`, 'columns'))))))
    selectInput('facet2', 'Facet 2', choices = choices, selected = if (length(conf$facet2)) conf$facet2 else '(none)')
  })

  observeEvent(input$facet1, {
    conf$facet1 <- if (identical(input$facet1, '(none)')) character(0) else input$facet1
    if (identical(input$facet1, '(none)')) conf$facet2 <- character(0)
  }, ignoreInit = TRUE)

  observeEvent(input$facet2, {
    conf$facet2 <- if (identical(input$facet2, '(none)')) character(0) else input$facet2
  }, ignoreInit = TRUE)

  output$buckets <- renderUI({
    nms <- names(conf$x)
    if (!length(nms)) return(NULL)
    available <- setdiff(nms, used_columns())

    rank_lists <- lapply(seq_along(conf$bins), function(i) {
      add_rank_list(
        text = paste('Bin', i),
        labels = conf$bins[[i]]$columns,
        input_id = paste0('bin_', i)
      )
    })

    do.call(
      bucket_list,
      c(
        list(
          header = 'Columns (drag into one or more Bins to collapse them into a single display column)',
          group_name = 'listado_bucket_group',
          orientation = 'horizontal',
          add_rank_list(text = 'Available', labels = available, input_id = 'available')
        ),
        rank_lists
      )
    )
  })

  # sync each possible bin rank list back into conf$bins; prune empties
  lapply(seq_len(MAX_BINS), function(i) {
    observeEvent(input[[paste0('bin_', i)]], {
      if (i > length(conf$bins)) return()
      bins <- conf$bins
      bins[[i]]$columns <- input[[paste0('bin_', i)]]
      bins <- Filter(function(b) length(b$columns) > 0, bins)
      if (!length(bins)) bins <- list(list(columns = character(0), separator = '/\n', width = '2cm'))
      conf$bins <- bins
    }, ignoreNULL = TRUE)
  })

  observeEvent(input$add_bin, {
    n <- length(conf$bins)
    if (n == 0 || length(conf$bins[[n]]$columns) > 0) {
      conf$bins <- c(conf$bins, list(list(columns = character(0), separator = '/\n', width = '2cm')))
    }
  })

  output$add_bin <- renderUI({
    actionButton('add_bin', 'Add bin')
  })

  output$bin_settings <- renderUI({
    if (!length(conf$bins)) return(NULL)
    rows <- lapply(seq_along(conf$bins), function(i) {
      fluidRow(
        column(2, strong(paste('Bin', i))),
        column(5, textInput(paste0('bin_sep_', i), 'separator', value = conf$bins[[i]]$separator)),
        column(5, textInput(paste0('bin_width_', i), 'width', value = conf$bins[[i]]$width))
      )
    })
    do.call(tagList, rows)
  })

  lapply(seq_len(MAX_BINS), function(i) {
    observeEvent(input[[paste0('bin_sep_', i)]], {
      if (i > length(conf$bins)) return()
      bins <- conf$bins
      bins[[i]]$separator <- input[[paste0('bin_sep_', i)]]
      conf$bins <- bins
    }, ignoreInit = TRUE)

    observeEvent(input[[paste0('bin_width_', i)]], {
      if (i > length(conf$bins)) return()
      bins <- conf$bins
      bins[[i]]$width <- input[[paste0('bin_width_', i)]]
      conf$bins <- bins
    }, ignoreInit = TRUE)
  })

  # ---- headers/footers tab ------------------------------------------------

  output$title_ui     <- renderUI(textInput('title', 'Title', value = conf$title))
  output$outputid_ui  <- renderUI(textInput('outputid', 'Output Identifier', value = conf$outputid))
  output$lhead1_ui    <- renderUI(textInput('lhead1', 'Left Header 1', value = conf$lhead1))
  output$lhead2_ui    <- renderUI(textInput('lhead2', 'Left Header 2', value = conf$lhead2))
  output$rhead1_ui    <- renderUI(textInput('rhead1', 'Right Header 1', value = conf$rhead1))
  output$rhead2_ui    <- renderUI(textInput('rhead2', 'Right Header 2', value = conf$rhead2))
  output$cont_ui       <- renderUI(textInput('cont', 'Continued', value = conf$cont))
  output$footnotes_ui <- renderUI(textAreaInput('footnotes', 'Footnotes', value = conf$footnotes, resize = 'both'))

  observeEvent(input$title, conf$title <- input$title, ignoreInit = TRUE)
  observeEvent(input$outputid, conf$outputid <- input$outputid, ignoreInit = TRUE)
  observeEvent(input$lhead1, conf$lhead1 <- input$lhead1, ignoreInit = TRUE)
  observeEvent(input$lhead2, conf$lhead2 <- input$lhead2, ignoreInit = TRUE)
  observeEvent(input$rhead1, conf$rhead1 <- input$rhead1, ignoreInit = TRUE)
  observeEvent(input$rhead2, conf$rhead2 <- input$rhead2, ignoreInit = TRUE)
  observeEvent(input$cont, conf$cont <- input$cont, ignoreInit = TRUE)
  observeEvent(input$footnotes, conf$footnotes <- input$footnotes, ignoreInit = TRUE)

  # ---- generate tab ---------------------------------------------------------

  shinyDirChoose(input, 'outdir', roots = ui_volumes, session = session)

  observeEvent(input$outdir, {
    path <- parseDirPath(ui_volumes, input$outdir)
    if (length(path)) conf$output_dir <- path
  })

  output$outdirpath <- renderPrint({
    if (!length(conf$output_dir)) {
      if (length(conf$filepath)) {
        cat('No output directory chosen yet (defaults to', dirname(conf$filepath), 'if unset).')
      } else {
        cat('No output directory chosen.')
      }
    } else {
      cat(conf$output_dir)
    }
  })

  output$out_filename_ui <- renderUI({
    req(conf$filepath)
    default_name <- paste0(
      tools::file_path_sans_ext(basename(conf$filepath)), '_',
      format(Sys.time(), '%Y%m%dT%H%M%S'), '.qmd'
    )
    textInput('out_filename', 'Output filename', value = default_name)
  })

  output$render_pdf_ui <- renderUI({
    checkboxInput('render_pdf', 'Render to PDF (requires quarto on PATH)', value = conf$render_pdf)
  })
  observeEvent(input$render_pdf, conf$render_pdf <- input$render_pdf, ignoreInit = TRUE)

  observeEvent(input$generate, {
    msgs <- character(0)

    if (!length(conf$filepath)) {
      showNotification('choose a data file first', type = 'error')
      return()
    }
    nonempty_bins <- Filter(function(b) length(b$columns) > 0, conf$bins)
    if (!length(nonempty_bins)) {
      showNotification('at least one non-empty bin is required', type = 'error')
      return()
    }
    outdir <- if (length(conf$output_dir)) conf$output_dir else dirname(conf$filepath)
    filename <- input$out_filename
    if (!length(filename) || !nzchar(filename)) {
      showNotification('choose an output filename', type = 'error')
      return()
    }
    qmd_path <- file.path(outdir, filename)

    snapshot <- isolate(reactiveValuesToList(conf))
    snapshot$bins <- nonempty_bins

    doc <- tryCatch(
      build_qmd(snapshot, system.file('shiny-examples/listado/template.qmd', package = 'tablet')),
      error = function(e) e
    )
    if (inherits(doc, 'error')) {
      output$generate_status <- renderText(paste('failed to build .qmd:', conditionMessage(doc)))
      showNotification('failed to build .qmd', type = 'error')
      return()
    }

    writeLines(doc, qmd_path)
    msgs <- c(msgs, paste('wrote', qmd_path))

    if (isTRUE(input$render_pdf)) {
      res <- tryCatch(
        system(paste('quarto render', shQuote(qmd_path), '--to pdf'), intern = TRUE),
        error = function(e) e
      )
      if (inherits(res, 'error')) {
        msgs <- c(msgs, paste('quarto render failed:', conditionMessage(res)))
      } else {
        pdf_path <- sub('\\.qmd$', '.pdf', qmd_path)
        if (file.exists(pdf_path)) {
          msgs <- c(msgs, paste('rendered', pdf_path))
        } else {
          msgs <- c(msgs, 'quarto render did not produce a PDF; see output below', paste(res, collapse = '\n'))
        }
      }
    }

    output$generate_status <- renderText(paste(msgs, collapse = '\n'))
  })
})

shinyApp(ui, server)

# copyright 2026 Tim Bergsma bergsmat@gmail.com
