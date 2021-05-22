library(shiny)
library(shinyFiles)
library(fs)
library(haven)
library(tablet)
library(dplyr)
library(magrittr)
library(yamlet)
library(yaml)
library(sortable)
library(kableExtra)
library(knitr)
library(latexpdf)
library(tools)
library(csv)

ui <- shinyUI(
  navbarPage(
    'Mesa',
    tabPanel(
      'Input',
      sidebarLayout(
        sidebarPanel(
          width = 12,
          shinyFilesButton(
            id = 'source',
            label = 'choose a dataset or metadata file',
            title = 'choose a dataset or metadata file:',
            multiple = FALSE
          ),
          textOutput('filepath'),
          shinyFilesButton(
            id = 'config',
            label = 'choose a table configuration file',
            title = 'choose a table configuration file:',
            multiple = FALSE
          ),
          uiOutput('saveconfig'),
          textOutput('confpath'),
          uiOutput('splice'),
          uiOutput('keep'),
          uiOutput('buckets')
        ),
        mainPanel(
          width = 0
        ) # end main panel
      ) # end sidebar layout
    ),
    tabPanel(
      'Data',
      sidebarLayout(
        sidebarPanel(
          width = 0
        ),
        mainPanel(
          width = 12,
          DT::dataTableOutput("data"),
        )
      )
    ),
    tabPanel(
      'Preview',
      sidebarLayout(
        sidebarPanel(
          width = 2,
          uiOutput('savecsv')
          # ,
          # actionButton(
          #   'csv',
          #   'Save as CSV'
          # )
        ),
        mainPanel(width = 10,
                  htmlOutput('preview')
        )
      )
    ),
    tabPanel(
      'PDF',
      sidebarLayout(
        sidebarPanel(
          width = 0
        ),
        mainPanel(
          width = 12,
          uiOutput('pdfview')
        )
      )
    ),
    tabPanel(
      'Annotations',
      sidebarLayout(
        sidebarPanel(
          width = 12,
          actionButton('submit', 'submit'),
          uiOutput('outputid'),
          uiOutput('caption'),
          uiOutput('footnotes'),
          uiOutput('lhead1'),
          uiOutput('lhead2'),
          uiOutput('rhead1'),
          uiOutput('rhead2'),
          uiOutput('na_string')
        ),
        mainPanel(width = 0) #end main panel
      )
    )
  ) # end page
) # end ui

server <- shinyServer(function(input, output, session) {

  # declare the objects that control the application
  conf <- reactiveValues(
    filepath   = character(0),
    confpath   = character(0),
    selected   = character(0),
    filter_by  = character(0),
    keep       = list(), # a named list of filter_by levels to keep
    group_by   = character(0),
    sequential = FALSE,
    title      = 'Title',
    outputid   = 'T-00-00',
    lhead1     = 'Company',
    lhead2     = 'Project',
    rhead1     = 'Confidential',
    rhead2     = 'Draft',
    footnotes  = '(footnotes here)',
    na_string  = 'NA',
    x          = data.frame()
  )

  reset_conf <- function(){
    conf$filepath   <- character(0)
    conf$confpath   <- character(0)
    conf$selected   <- character(0)
    conf$filter_by  <- character(0)
    conf$keep       <- list() # a named list of filter_by levels to keep
    conf$group_by   <- character(0)
    conf$sequential <- FALSE
    conf$title      <- 'Title'
    conf$outputid   <- 'T-00-00'
    conf$lhead1     <- 'Company'
    conf$lhead2     <- 'Project'
    conf$rhead1     <- 'Confidential'
    conf$rhead2     <- 'Draft'
    conf$footnotes  <- '(footnotes here)'
    conf$na_string  <- 'NA'
    conf$x          <- data.frame()
  }

  file_ok <- function(x){
    if(!length(x))return(FALSE)
    if(!file.exists(x))return(FALSE)
    return(TRUE)
  }
  # https://github.com/thomasp85/shinyFiles/issues/85

  volumes <- getVolumes()
  moreVolumes <- function()c(
    volumes(),
    examples = system.file('shiny-examples/mesa/data', package = 'tablet'),
    home = fs::path_home(),
    R = R.home()
  )
  ui_volumes <- reactive( {
    volumes <- moreVolumes()
    if(length(conf$filepath) & !any(is.na(conf$filepath))){
      sel_path <- dirname(conf$filepath)
      if(!sel_path %in% volumes){
        vnames <- c(basename(sel_path), names(volumes))
        volumes <- setNames(c(sel_path, volumes), vnames)
      }
    }
    if(length(conf$confpath) & !any(is.na(conf$confpath))){
      sel_path <- dirname(conf$confpath)
      if(!sel_path %in% volumes){
        vnames <- c(basename(sel_path), names(volumes))
        volumes <- setNames(c(sel_path, volumes), vnames)
      }
    }
    volumes
  })

  # safe_volumes <- function(){
  #   browser()
  #   ui_volumes()
  # }

  # set up the file choosers

  shinyFileChoose(
    input,
    'source',
    roots = ui_volumes,
    session = session,
    filetypes = c('sas7bdat', 'csv', 'xpt', 'yaml')
  )

  shinyFileChoose(
    input,
    'config',
    roots = ui_volumes,
    session = session,
    filetypes = c('conf')
  )


  # https://stackoverflow.com/questions/39517199/how-to-specify-file-and-path-to-save-a-file-with-r-shiny-and-shinyfiles

  observe({
    shinyFileSave(input, "save", roots = ui_volumes, session = session)
    fileinfo <- parseSavePath(ui_volumes, input$save)
    if (nrow(fileinfo) > 0) {
      path <- as.character(fileinfo$datapath)
      vals <- isolate(
        reactiveValuesToList(conf)[
          !names(conf) %in% c(
            'x',
            'confpath'
          )
        ]
      )
      write_yaml(vals, path) # only reads on save
      conf$confpath <- path
    }
  })
  observe({
    shinyFileSave(input, "savetable", roots = ui_volumes, session = session)
    fileinfo <- parseSavePath(ui_volumes, input$save)
    if (nrow(fileinfo) > 0) {
      path <- as.character(fileinfo$datapath)
      data <- isolate(summarized())
      as.csv(data, path)
    }
  })

  #https://stackoverflow.com/questions/40547786/shiny-can-dynamically-generated-buttons-act-as-trigger-for-an-event

  observers <- list()

  observeEvent(input$selected,{
    conf$selected <- input$selected
  })

  observeEvent(input$filter_by,{
    conf$filter_by <- input$filter_by
  })

  observeEvent(input$group_by,{
    conf$group_by <- input$group_by
  })

  observeEvent(input$splice,{
    conf$sequential <- ifelse(input$splice == 'sequential', TRUE, FALSE)
  })

  observeEvent(input$submit,{
    conf$title <- input$caption
  })

  # observeEvent(input$csv,{
  #   as.csv(summarized(), paste0(conf$outputid,'.csv'))
  # })

  observeEvent(input$submit,{
    conf$outputid <- input$outputid
  })

  observeEvent(input$submit,{
    conf$lhead1 <- input$lhead1
  })

  observeEvent(input$submit,{
    conf$lhead2 <- input$lhead2
  })

  observeEvent(input$submit,{
    conf$rhead1 <- input$rhead1
  })

  observeEvent(input$submit,{
    conf$rhead2 <- input$rhead2
  })

  observeEvent(input$submit,{
    conf$footnotes <- input$footnotes
  })

  observeEvent(input$submit,{
    conf$na_string <- input$na_string
  })

  observeEvent(input$source, {
    if(is.integer(input$source)) return()
    conf$filepath <- as.character(
      as.data.frame(
        parseFilePaths(
          ui_volumes, input$source
        )
      )[1,'datapath']
    )
  })

  observeEvent(input$config, {
    #browser()
    conf$confpath <- as.character(
      as.data.frame(
        parseFilePaths(
          ui_volumes, input$config
        )
      )[1,'datapath']
    )
  })

  observeEvent(conf$confpath,{
    #browser()
    if(!length(conf$confpath))return()
    if(!file.exists(conf$confpath))return()
   # browser()
    #if(is.null(oldconf()))return()

    saved <- read_yaml(conf$confpath)
    #browser()
    # items not saved should be re-initialized
    if(!('filepath' %in% names(saved))) {
      showNotification(duration = NULL, type = 'error', 'configuration does not specify file path')
      reset_conf()
      return()
    }
    if(!file.exists(saved$filepath)){
      showNotification(duration = NULL, type = 'error', 'configured file path not found')
      reset_conf()
      return()
    }

    # at this point:
    #   * confpath has changed
    #   * confpath is readable/parseable
    #   * configuration has a filepath
    #   * filepath exists

    # update internal configuration from saved configuration
    conf$filepath <- saved$filepath
    conf$selected <- saved$selected
    conf$filter_by <- saved$filter_by
    conf$keep      <- saved$keep
    conf$group_by   <- saved$group_by
    conf$sequential<- saved$sequential
    conf$title      <- saved$title
    conf$lhead1     <- saved$lhead1
    conf$lhead2    <- saved$lhead2
    conf$rhead1     <- saved$rhead1
    conf$rhead2    <- saved$rhead2
    conf$footnotes <- saved$footnotes
    conf$na_string <- saved$na_string
    conf$outputid <- saved$outputid
    #conf$x          = data.frame()
    # if filepath has changed, data will be re-read

  })

  output$filepath <- renderPrint({
    if (!length(conf$filepath)) {
      cat('No input data selected.')
    } else {
      cat(conf$filepath)
    }
  })

  observeEvent(conf$filepath, {
    if(!length(conf$filepath))return()
    theFile <- conf$filepath
    is_data <- grepl('\\.sas7bdat|xpt|csv$', theFile)
    is_meta <- grepl('\\.yaml$', theFile)

    datafile <- theFile
    if(is_meta) {
      datafile <- sub('yaml$','sas7bdat',theFile)
      if(!file.exists(datafile)) datafile <- sub('yaml$','xpt',theFile)
      if(!file.exists(datafile)) datafile <- sub('yaml$','csv',theFile)
    } # try everything

    metafile <- theFile
    if(is_data) metafile <- sub('sas7bdat|xpt|csv$', 'yaml', theFile)
    has_data <- file.exists(datafile)
    has_meta <- file.exists(metafile)

    d <- data.frame()

    if(has_data){
      if(grepl('sas7bdat$', datafile)) d <- data.frame(read_sas(datafile))
      if(grepl('xpt$', datafile)) d <- data.frame(read.xport(datafile))
      if(grepl('csv$', datafile)) d <- data.frame(as.csv(datafile))
    }

    # at this point, best data has been defined. Define default metadata.

    m <- decorations(d)

    # Either read the metadata or write it.
    if(has_meta){
      # try for better meta.
      tryCatch(
        m <- read_yamlet(metafile),
        error = function(e) showNotification(duration = NULL, type = 'error', as.character(e))
      )
    } else {
      write_yamlet(m, metafile)
    }

    # now we have best available metadata
    has_meta <- TRUE

    # make data look like metadata (which may be superset)

    have <- names(d)
    need <- names(m)
    make <- setdiff(need, have)
    for(col in make) d[[col]] <- rep(NA_real_, nrow(d))

    # ensure positive nrow
    if(nrow(d) == 0) d <- d['',,drop = FALSE]

    # drop unspecified
    d %<>% select(!!!names(m))

    # apply meta
    d <- redecorate(d, m)

    # # Promote NA to a level of the factor
    d %<>% resolve(exclude = NULL)

    # store on the session
    conf$x <- d

  })

  filtered <- reactive({
    #browser()
    x <- conf$x
    cols <- conf$filter_by
    for(filter in cols){
      scope <- input[[paste0('mesa_filter_', filter)]]
      if(length(scope)){ # only filter if at least one choice was made!
        # save these for drawing the UI
        conf$keep[[filter]] <- scope
        index <- x[[filter]] %in% scope
        x <- x[index,,drop = FALSE]
      }
    }
    x
  })

  factorized <- reactive({
    x <- filtered()
    x %<>% mutate_if(is.character, classified)
    x
  })

  selected <- reactive({
    x <- factorized()
    if(length(conf$group_by)) x %<>% group_by(!!!syms(conf$group_by))
    x %<>% select(!!!syms(conf$selected))
    x
  })

  args <- reactive({
    x <- list(x = selected())
    extra <- list(
      # all = 'All',
      fun = list(
        sum ~ signif(digits = 3,     sum(x,  na.rm = TRUE)),
        pct ~ signif(digits = 3,     sum / n * 100        ),
        ave ~ signif(digits = 3,    mean(x,  na.rm = TRUE)),
        std ~ signif(digits = 3,      sd(x,  na.rm = TRUE)),
        med ~ signif(digits = 3,  median(x,  na.rm = TRUE)),
        min ~ signif(digits = 3,     min(x,  na.rm = TRUE)),
        max ~ signif(digits = 3,     max(x,  na.rm = TRUE)),
        smn ~ sum(!is.na(x))
      ),
      num = list(
        n ~ smn,
        `Mean (SD)` ~ ave + ' (' + std + ')',
        Median ~ med,
        `Min, Max` ~ min + ', ' + max
      )
    )
    bundle <- c(x, extra)
    bundle
  })

  summarized <- reactive({
    fun <- tablet
    if(conf$sequential) fun <- splice
    fun
    args <- args()
    do.call(fun,args)
  })

  html <- reactive({
    options(knitr.kable.NA = conf$na_string)
    x <- summarized()
    x %<>% as_kable(caption = conf$title)
    x %<>% kable_classic(full_width = F, html_font = "Cambria")
    x %<>% kable_styling(fixed_thead = T)
    x
  })

  tex <- reactive({
    old <- opts_knit$get('out.format')
    opts_knit$set(out.format = 'latex')
    options(knitr.kable.NA = conf$na_string)
    x <- summarized()
    if(!nrow(x)){
      showNotification(duration = NULL, type = 'error', 'no rows selected')
      return(character(0))
    }
    x %<>% as_kable(format = 'latex', caption = conf$title, longtable = TRUE)
    x %<>% footnote(general = conf$footnotes,fixed_small_size = TRUE,general_title = " ",threeparttable = TRUE)
    x %<>% as.character
    x %<>% as.document(
      thispagestyle = '',
      pagestyle = '',
      preamble = c(
        '\\documentclass{article}',
        '\\usepackage[utf8]{inputenc}',
        '\\usepackage[T1]{fontenc}',
        '\\usepackage[showseconds=false]{datetime2}',
        '\\usepackage[landscape]{geometry}',
        '\\usepackage{fancyhdr}',
        '\\fancyhf{}',
        '\\renewcommand{\\headrulewidth}{0pt}',
        '\\pagestyle{fancy}',
        paste0('\\lhead{', conf$lhead1,' \\\\ ',conf$lhead2, '}'),
        '%\\chead{Table 0.0.0.xxx}',
        paste0('\\rhead{', conf$rhead1,' \\\\ ',conf$rhead2, '}'),
        #paste0('\\lfoot{\\textit{',file_path_sans_ext(conf$filepath),'}}'),
        paste0('\\lfoot{\\textit{~', sub(getwd(),'',conf$confpath, fixed = TRUE),'}}'),

        '\\rfoot{\\today{~at~\\DTMcurrenttime}}',
        '\\usepackage{booktabs}',
        '\\usepackage{longtable}',
        '\\usepackage{array}',
        '\\usepackage{multirow}',
        '\\usepackage{wrapfig}',
        '\\usepackage{float}',
        '\\usepackage{colortbl}',
        '\\usepackage{pdflscape}',
        '\\usepackage{tabu}',
        '\\usepackage{threeparttable}',
        '\\usepackage{threeparttablex}',
        '\\usepackage[normalem]{ulem}',
        '\\usepackage{xcolor}',
        '\\usepackage[labelformat=empty]{caption}',
        '\\usepackage{makecell}'
      )
    )
    opts_knit$set(out.format = old)
    x
  })

  output$saveconfig <- renderUI({
    shinySaveButton(
      id = 'save',
      label = 'save configuration as ...',
      title = 'save configuration as:',
      filetype = list(conf = 'conf'),
      filename = paste0(conf$outputid, '.conf')
    )

  })

  output$savecsv <- renderUI({
    shinySaveButton(
      id = 'savetable',
      label = 'save table as ...',
      title = 'save table as:',
      filetype = list(conf = 'csv'),
      filename = paste0(conf$outputid, '.csv')
    )

  })

  output$buckets <- renderUI({
    if(!length(conf$x))return()
    nms <- names(conf$x)
    selected <- intersect(conf$selected, nms)
    group_by <- intersect(conf$group_by, nms)
    filter_by <-intersect(conf$filter_by, nms)

    used <- union(selected, group_by)
    used <- union(used, filter_by)
    available <- sort(setdiff(nms, used)) # definitive set
    suggested <- union(input$available, available) # user's sort order
    available <- intersect(suggested, available) # defer to user where possible

    bucket_list(
      header = 'Data Item Roles',
      group_name = 'bucket_list_group',
      orientation = 'horizontal',
      add_rank_list(
        text = 'Available',
        labels = available,
        input_id = 'available'
      ),
      add_rank_list(
        text = 'Summarize',
        labels = selected,
        input_id = 'selected'
      ),
      add_rank_list(
        text = 'Group By',
        labels = group_by,
        input_id = 'group_by'
      ),
      add_rank_list(
        text = 'Filter By',
        labels = filter_by,
        input_id = 'filter_by'
      )
    )
  })

  output$splice <- renderUI({
    radioButtons(
      'splice',
      'Grouping Style',
      inline = TRUE,
      choices = c('nested','sequential'),
      selected = ifelse(conf$sequential,'sequential','nested')
    )

  })

  output$caption <- renderUI({
    textAreaInput('caption','Title', value = conf$title, resize = 'both')
  })

  output$outputid <- renderUI({
    textInput('outputid','Output Identifier', value = conf$outputid)
  })

  output$lhead1 <- renderUI({
    textInput('lhead1','Left Header 1', value = conf$lhead1)
  })

  output$lhead2 <- renderUI({
    textInput('lhead2','Left Header 2', value = conf$lhead2)
  })

  output$rhead1 <- renderUI({
    textInput('rhead1','Right Header 1', value = conf$rhead1)
  })

  output$rhead2 <- renderUI({
    textInput('rhead2','Right Header 2', value = conf$rhead2)
  })

  output$footnotes <- renderUI({
    textAreaInput('footnotes','Footnotes', value = conf$footnotes, resize = 'both')
  })

  output$na_string <- renderUI({
    textInput('na_string','text substitute for NA', value = conf$na_string)
  })

  output$keep <- renderUI({
    if(!length(input$filter_by))return()

    myFilter <- function(var, dat){
      nms <- as.character(sort(unique(dat[[var]])))
      lbl <- attr(dat[[var]], 'label')

      checkboxGroupInput(
        inline = TRUE,
        inputId = paste0('mesa_filter_',var),
        label = lbl,
        choices = nms,
        selected = conf$keep[[var]]
      )
    }

    myObserver <- function(var){
      observers[[var]] <<- observeEvent(input[[paste0('mesa_filter_',var)]], {
        conf$keep[[var]] <- input[[paste0('mesa_filter_',var)]]
      })
    }

    # pre-assign an observer if not already
    lapply(input$filter_by, myObserver)

    lapply(input$filter_by, myFilter, dat = conf$x)
  })

  output$data <- DT::renderDataTable({
    out <- conf$x
    #out %<>% resolve # already done
    out %<>% modify(name = paste(name, label, sep = ': '))
    out %<>% modify(name = paste0(name, label, ': (', .data$units, ')'))
    out
  })

  output$preview <- renderText({
    if(!length(input$selected))return('Output goes here.')
    # ensure html
    opts_knit$set(out.format = 'html')
    x <- html()
    x
  })

  pdf_location <- reactive({
    x <- tex()
    if(!length(x))return('1x1.png')
    stem <- isolate(conf$outputid) # basename(tempfile())
    #browser()
    path <- as.pdf(
      x,
      dir = 'www',
      stem = stem
    )
    basename(path)
  })

  # https://stackoverflow.com/questions/19469978/displaying-a-pdf-from-a-local-drive-in-shiny

  output$pdfview <- renderUI({
    #browser()
    if(!nrow(conf$x))return('PDF displays here.')
    tags$iframe(
      style="height:600px; width:100%; scrolling:yes",
      src = paste0('/',pdf_location())
    )
  })

  output$confpath <- renderPrint({
    #browser()
    if (!length(conf$confpath)) {
      cat('No configuration selected.')
    } else {
      cat(conf$confpath)
    }
  })


})

# Create Shiny app ----
shinyApp(ui, server)

# copyright 2021 Tim Bergsma bergsmat@gmail.com
