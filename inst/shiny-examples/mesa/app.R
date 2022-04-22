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
library(shinyAce)
library(spork)

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
            label = 'data',
            title = 'choose data or metadata (*.yaml) file:',
            multiple = FALSE
          ),
          textOutput('filepath'),
          shinyFilesButton(
            id = 'config',
            label = 'configuration',
            title = 'choose configuration file:',
            multiple = FALSE
          ),
          textOutput('confpath'),
          uiOutput('saveconfig'),
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
      'Variables',
      sidebarLayout(
        sidebarPanel(
          width = 2,
          uiOutput('saveMeta'),
          textOutput('metapath')
        ),
        mainPanel(
          width = 12,
          uiOutput('meta')
        )
      )
    ),
    tabPanel(
      'Shell',
      sidebarLayout(
        sidebarPanel(
          width = 12,
          actionButton('submit', 'Save'),
          uiOutput('outputid'),
          uiOutput('caption'),
          uiOutput('footnotes'),
          uiOutput('lhead1'),
          uiOutput('lhead2'),
          uiOutput('rhead1'),
          uiOutput('rhead2'),
          uiOutput('cont')
        ),
        mainPanel(width = 0) #end main panel
      )
    ),
    tabPanel(
      'Preview',
      sidebarLayout(
        sidebarPanel(
          width = 2,
          uiOutput('labelhtml'),
          uiOutput('savecsv')
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
          width = 2,
          uiOutput('repeatheader'),
          uiOutput('repeatfootnote'),
          #uiOutput('spork'),
          # uiOutput('na_string'),
          uiOutput('labeltex'),
          uiOutput('savepdf')
        ),
        mainPanel(
          width = 10,
          uiOutput('pdfview')
        )
      )
    )
  ) # end page
) # end ui

server <- shinyServer(function(input, output, session) {

  # declare the objects that control the application

  conf <- reactiveValues(
    filepath   = character(0),
    metapath   = character(0),
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
    cont  = '(continued)',
    footnotes  = '(footnotes here)',
#    na_string  = 'NA',
    x          = data.frame(),
    mv         = 0,
    editor     = NULL,
    labelhtml  = 'no',
    labeltex   = 'no',
    repeathead = 'no',
    repeatfoot = 'no',
    tablet     = as.character(packageVersion('tablet'))
  )

  reset_conf <- function(){
    printer('reset_conf')
    conf$filepath   <- character(0)
    conf$metapath   <- character(0)
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
    conf$cont       <- '(continued)'
    conf$footnotes  <- '(footnotes here)'
 #   conf$na_string  <- 'NA'
    conf$x          <- data.frame()
    conf$imputed    <- character()
    conf$mv         <- 0
    conf$editor     <- NULL
    labelhtml       <- 'no'
    labeltex        <- 'no'
    repeathead      <- 'no'
    repeatfoot      <- 'no'
    tablet          <- as.character(packageVersion('tablet'))
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
  ui_volumes <- reactive({
    printer('reactive ui_volumes')
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

  # set up the file choosers
  # https://stackoverflow.com/questions/53641749/how-to-use-shinyfilechoose-to-create-an-reactive-object-to-load-a-data-frame

  # file_selected <- reactive({
  #   shinyFileChoose(input, "file", roots = volumes, session = session)
  #   req(input$file)
  #   if (is.null(input$file))
  #     return(NULL)
  #   return(parseFilePaths(volumes, input$file)$datapath)
  # })

  # choose data (or metadata)
  shinyFileChoose(
    input,
    'source',
    roots = ui_volumes,
    session = session,
    filetypes = c('sas7bdat', 'csv', 'xpt', 'yaml')
  )
  observeEvent(input$source, {
    printer('observeEvent:input$source')
    req(input$source)
    if(is.null(input$source)) return(NULL)
    newsource <- parseFilePaths(ui_volumes, input$source)$datapath
    if(is.character(newsource)){
      if(length(newsource)){
        if(file.exists(newsource)){
          reset_conf()
          conf$filepath <- newsource
        }
      }
    }
  })

  # choose config

  shinyFileChoose(
    input,
    'config',
    roots = ui_volumes,
    session = session,
    filetypes = c('conf')
  )

  observeEvent(input$config,{
    printer('observeEvent:input$config')
    req(input$config)
    if(is.null(input$config)) return(NULL)
    newconfig <- parseFilePaths(ui_volumes, input$config)$datapath
    if(is.character(newconfig)){
      if(length(newconfig)){
        if(file.exists(newconfig)) {
          conf$confpath <- newconfig
        }
      }
    }
  })

  # https://stackoverflow.com/questions/39517199/how-to-specify-file-and-path-to-save-a-file-with-r-shiny-and-shinyfiles

  output$saveconfig <- renderUI({
    printer('output$saveconfig')
    shinySaveButton(
      id = 'saveconf',
      label = 'save configuration',
      title = 'save configuration as:',
      filetype = list(conf = 'conf'),
      filename = paste0(conf$outputid, '.conf')
    )

  })

  # save the current config
  observeEvent(input$saveconf, {
    printer('observeEvent:input$saveconf')
    req(input$saveconf)
    shinyFileSave(input, 'saveconf', roots = ui_volumes, session = session)
    fileinfo <- parseSavePath(ui_volumes, input$saveconf)
    if (nrow(fileinfo) > 0) {
      path <- as.character(fileinfo$datapath)

      vals <- isolate(
        reactiveValuesToList(conf)[
          !names(conf) %in% c(
            'x',
            'confpath',
            'editor',
            'mv',
            'imputed'
          )
        ]
      )
      # dictate storage order!
      vals <- vals[c(
        'filepath',
        'metapath',
        'selected',
        'group_by',
        'filter_by',
        'keep',
        'sequential',
        'outputid',
        'title',
        'lhead1',
        'lhead2',
        'rhead1',
        'rhead2',
        'footnotes',
        'repeathead',
        'repeatfoot',
        'cont',
        'labelhtml',
        'labeltex',
        'tablet'
      )]

      # note: below is the only place in the application where the configuration is written to storage.
      # filepath and metapath, like confpath, are stored internally as absolute paths.
      # but on write they are expressed relative to confpath directory,
      # and on read they are understood relative to confpath directory (and converted to absolute).

      if(length(vals$filepath))vals$filepath <- relativizePath(vals$filepath, dirname(path))
      if(length(vals$metapath))vals$metapath <- relativizePath(vals$metapath, dirname(path))
      res <- try(write_yaml(vals, path)) # only reads on save
      res <- !inherits(res, 'try-error')
      dur <- 10
      if(res) dur <- 5
      showNotification(
        duration = dur,
        type = ifelse(res, 'default', 'error'),
        ui = paste(
          ifelse(res, 'wrote', 'did not write'),
          path
        )
      )
      if(res){
        conf$confpath <- path
      }
    }
  })

  output$savecsv <- renderUI({
    printer('output$savecsv')
    shinySaveButton(
      id = 'savetable',
      label = 'save table',
      title = 'save table as:',
      filetype = list(csv = 'csv'),
      filename = paste0(conf$outputid, '.csv')
    )
  })

  # save the preview table
  observeEvent(input$savetable, {
    printer('observeEvent: input$savetable')
    req(input$savetable)
    shinyFileSave(input, 'savetable', roots = ui_volumes, session = session)
    fileinfo <- parseSavePath(ui_volumes, input$savetable)
    if (nrow(fileinfo) > 0) {
      path <- as.character(fileinfo$datapath)
      data <- isolate(summarized())
      res <- try(as.csv(data, path))
      res <- !inherits(res, 'try-error')
      dur <- 10
      if(res) dur <- 5
      showNotification(
        duration = dur,
        type = ifelse(res, 'default', 'error'),
        ui = paste(
          ifelse(res, 'wrote', 'did not write'),
          path
        )
      )
    }
  })

  output$savepdf <- renderUI({
    printer('output$savepdf')
    shinySaveButton(
      id = 'savepdf',
      label = 'save pdf',
      title = 'save pdf as:',
      filetype = list(pdf = 'pdf'),
      filename = paste0(conf$outputid, '.pdf')
    )
  })

  # save the pdf as
  observeEvent(input$savepdf, {
    printer('observeEvent:input$savepdf')
    req(input$savepdf)
    shinyFileSave(input, 'savepdf', roots = ui_volumes, session = session)
    fileinfo <- parseSavePath(ui_volumes, input$savepdf)
    if (nrow(fileinfo) > 0) {
      path <- as.character(fileinfo$datapath)
      from <- isolate(pdf_location())
      from <- file.path('www', from)
      res <- file.copy(from, path, overwrite = TRUE)
      dur <- 10
      if(res) dur <- 5
      showNotification(
        duration = dur,
        type = ifelse(res, 'default', 'error'),
        ui = paste(
          ifelse(res, 'wrote', 'did not write'),
          path
        )
      )
    }
  })

  #https://stackoverflow.com/questions/40547786/shiny-can-dynamically-generated-buttons-act-as-trigger-for-an-event

  observers <- list()

  observeEvent(input$selected,{
    printer('observeEvent: input$selected')
    conf$selected <- input$selected
  })

  observeEvent(input$filter_by,{
    printer('observeEvent:input$filter_by')
    conf$filter_by <- input$filter_by
  })

  observeEvent(input$group_by,{
    printer('observeEvent:input$group_by')
    conf$group_by <- input$group_by
  })

  observeEvent(input$splice,{
    printer('observeEvent:Input$splice')
    conf$sequential <- ifelse(input$splice == 'sequential', TRUE, FALSE)
  })

  observeEvent(input$submit,{
    printer('observeEvent:input$submit(conf$title)')
    conf$title <- input$caption
  })

  # observeEvent(input$csv,{
  #   as.csv(summarized(), paste0(conf$outputid,'.csv'))
  # })

  observeEvent(input$submit,{
    printer('observeEvent:input$submit(conf$outputid)')
    conf$outputid <- input$outputid
  })

  observeEvent(input$submit,{
    printer('observeEvent:input$submit(conf$lhead1)')
    conf$lhead1 <- input$lhead1
  })

  observeEvent(input$submit,{
    printer('observeEvent:input$submit(conf$lhead2)')
    conf$lhead2 <- input$lhead2
  })

  observeEvent(input$submit,{
    printer('observeEvent:input$submit(conf$rhead1)')
    conf$rhead1 <- input$rhead1
  })

  observeEvent(input$submit,{
    printer('observeEvent:input$submit(conf$rhead2)')
    conf$rhead2 <- input$rhead2
  })

  observeEvent(input$submit,{
    printer('observeEvent:input$submit(conf$cont)')
    conf$cont <- input$cont
  })

  observeEvent(input$submit,{
    printer('observeEvent:input$submit(conf$footnotes)')
    conf$footnotes <- input$footnotes
  })

  # observeEvent(input$na_string,{
  #   conf$na_string <- input$na_string
  # })
  observeEvent(input$repeathead,{
    printer('observeEvent:input$repeathead')
    conf$repeathead <- input$repeathead
  })
  observeEvent(input$repeatfoot,{
    printer('observeEvent:input$repeatfoot')
    conf$repeatfoot <- input$repeatfoot
  })
  observeEvent(input$labelhtml,{
    printer('observeEvent:input$labelhtml')
    conf$labelhtml <- input$labelhtml
  })
  observeEvent(input$labeltex,{
    printer('observeEvent:input$labeltex')
    printer(paste0(conf$labeltex,'<-', input$labeltex))
    conf$labeltex <- input$labeltex
  })

  observeEvent(conf$confpath,{
    printer('observeEvent:conf$confpath')
    if(!length(conf$confpath)){
      return()
    }

    if(!file.exists(conf$confpath)){
      return()
    }

    saved <- list()
    tryCatch(
      saved <- read_yaml(conf$confpath),
      error = function(e) showNotification(duration = NULL, type = 'error', as.character(e))
    )

    if(!length(saved)){
      showNotification(duration = NULL, type = 'message', 'resetting configuration')
      reset_conf()
      return()
    }

    # items not saved should be re-initialized
    if(!('filepath' %in% names(saved))) {
      showNotification(duration = NULL, type = 'error', 'configuration does not specify file path')
      reset_conf()
      return()
    }

    if(
      !file.exists(
        absolutizePath(
          saved$filepath,
          dirname(conf$confpath)
        )
      )
    ){
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

    # note: below is the only place in the application where the configuration is read from storage.
    # filepath and metapath, like confpath, are stored internally as absolute paths.
    # but on write they are expressed relative to confpath directory,
    # and on read they are understood relative to confpath directory (and converted to absolute).

    if(!is.null(saved$filepath))conf$filepath <- absolutizePath(saved$filepath, dirname(conf$confpath))
    if(!is.null(saved$metapath))conf$metapath <- absolutizePath(saved$metapath, dirname(conf$confpath))
    if(!is.null(saved$selected))conf$selected <- saved$selected
    if(!is.null(saved$filter_by))conf$filter_by <- saved$filter_by
    if(!is.null(saved$keep))conf$keep      <- saved$keep
    if(!is.null(saved$group_by))conf$group_by   <- saved$group_by
    if(!is.null(saved$sequential))conf$sequential<- saved$sequential
    if(!is.null(saved$title))conf$title      <- saved$title
    if(!is.null(saved$lhead1))conf$lhead1     <- saved$lhead1
    if(!is.null(saved$lhead2))conf$lhead2    <- saved$lhead2
    if(!is.null(saved$rhead1))conf$rhead1     <- saved$rhead1
    if(!is.null(saved$rhead2))conf$rhead2    <- saved$rhead2
    if(!is.null(saved$cont))conf$cont <- saved$cont
    if(!is.null(saved$footnotes))conf$footnotes <- saved$footnotes
#    if(!is.null(saved$na_string))conf$na_string <- saved$na_string
    if(!is.null(saved$outputid))conf$outputid <- saved$outputid
    if(!is.null(saved$repeathead))conf$repeathead <- saved$repeathead
    if(!is.null(saved$repeatfoot))conf$repeatfoot <- saved$repeatfoot
    if(!is.null(saved$labelhtml))conf$labelhtml <- saved$labelhtml
    if(!is.null(saved$labeltex))conf$labeltex <- saved$labeltex

    # version control
    if(!is.null(saved$tablet)){
      if(!identical(saved$tablet, conf$tablet)){
        showNotification(
          duration = NULL,
          type = 'warning',
          paste(
            'configuration was last saved by tablet version', saved$tablet,
            'but currently using', conf$tablet
          )
        )
      }
    }

    #conf$x          = data.frame()
    # if filepath has changed, data will be re-read: see observeEvent(conf$filepath)
  })

  # when conf$filepath changes, we rebuild the data
  # also need to trigger when metadata changes on disk
  # i.e. when we have saved it.

  # https://stackoverflow.com/questions/34731975/how-to-listen-for-more-than-one-event-expression-within-a-shiny-eventreactive-ha

  printer <- function(x)writeLines(as.character(x))

  observeEvent({
      conf$filepath # new data selected
      conf$mv # metadata re-written
      1 # prevents NULL from squelching the observation
    },
    {
    printer('observeEvent:conf$filepath')

    # invalidate the keep/filter observers if data changes
    observers <<- list()

    # invalidate configuration if an attempt is made to supplant data
    # conf$confpath <- character(0)
    # this does not work!

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

   # d <- data.frame() # 2022/04/13 make html() responsive to coerced columns
    d <- conf$x

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
    conf$metapath <- metafile # make visible

    # make data look like metadata (which may be superset)

    have <- names(d)
    need <- names(m)
    make <- setdiff(need, have)
    #browser()
    for(col in make) d[[col]] <- rep(NA_real_, nrow(d))


    # ensure positive nrow # removed at 0.5.4
    # if(nrow(d) == 0) d <- d['',,drop = FALSE]

    # drop unspecified
    d %<>% select(!!!names(m))

    # apply meta
    d <- redecorate(d, m)

    # # Promote NA to a level of the factor
    # d %<>% resolve(exclude = NULL)
    d %<>% resolve()

    # store on the session
    conf$x <- d
    conf$imputed <- sapply(select(d, !!!make), attr, 'label')

  })

  output$filepath <- renderPrint({
    #printer('observeEvent:output$filepath')
    if (!length(conf$filepath)) {
      cat('No input data selected.')
    } else {
      cat(conf$filepath)
    }
  })

  filtered <- reactive({
    printer('filtered')
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
    printer('factorized')
    x <- filtered()
    x %<>% mutate_if(is.character, classified)
    suppressWarnings(x %<>% modify(title = label))
    suppressWarnings(x %<>% modify(title = paste0(label, ' (', .data$units, ')')))

    # conditionally creating scripted labels has the
    # unintended effect of making the pdf and preview displays
    # co-dependent, since the output of factorized() changes
    # when either flag changes.
    # Meanwhile, it is easy and cheap to calculate html/tex labels
    # unconditionally, but use them conditionally.
    # Accordingly, we unconditionalize the following code.

    # if(length(input$labelhtml) == 1){
    #   printer('factorized - labelhtml')
    #   if(input$labelhtml == TRUE){

    suppressWarnings(x %<>% modify(original = name))
    suppressWarnings(x %<>% modify(html = as_html(as_spork(.data$name)))) # default
    suppressWarnings(x %<>% modify(html = as_html(as_spork(.data$label))))
    suppressWarnings(x %<>% modify(
      html = concatenate(as_html(as_spork(c(.data$label, ' (', .data$units,')'))))
    ))
    #   }
    # }else{printer('factorized - no labelhtml')}
    # if(length(input$labeltex) == 1){
    #   printer('factorized - labeltex')
    #
    #   if(input$labeltex == TRUE){
        # browser()
        # we need default 'latex' tex attributes for all columns ...
    suppressWarnings(x %<>% modify(tex = as_latex(as_spork(.data$name))))
    suppressWarnings(x %<>% modify(tex = as_latex(as_spork(.data$label))))
    suppressWarnings(x %<>% modify(
      # should retain class 'latex'
      # currently pre-doubled by escape_latex.latex
      tex = concatenate(as_latex(as_spork(c(.data$label, ' (', .data$units,')'))))
    ))
    #   }
    # }else{printer('factorized - no labeltex')}
    x
  })

  selected <- reactive({
    printer('selected')
    x <- factorized()
    if(length(conf$group_by)) x %<>% group_by(!!!syms(conf$group_by))
    x %<>% select(!!!syms(conf$selected))
    x
  })

  args <- reactive({
    printer('args')
    x <- list(x = selected())
    extra <- list(
      all_levels = TRUE,
      # all = 'All',
      fun = list(
        sum ~ sum(x,  na.rm = TRUE),
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
        Median ~ paste(med),
        `Min, Max` ~ min + ', ' + max
      ),
      fac = list(
        ` ` ~ ifelse(sum == 0, '0', sum + ' (' + pct + '%' + ')')
      )
    )
    bundle <- c(x, extra)
    bundle
  })

  summarized <- reactive({
    printer('summarized')
    fun <- tablet
    if(conf$sequential) fun <- splice
    args <- args()
    do.call(fun,args)
  })

  html <- reactive({
    printer('html')
   # options(knitr.kable.NA = conf$na_string)
    options(knitr.kable.NA = 0)

    fun <- tablet
    if(conf$sequential) fun <- splice
    args <- args()

    if(!is.null(input$labelhtml)){
      if(input$labelhtml == 'yes'){
        args$x %<>% modify(title = .data$html)
      }
    } else {
      printer('no labelhtml yet')
      return()
    }
    x <- do.call(fun, args)
    # remove NA groups
    na <- which(names(x) == 'NA')
    for(i in rev(na))x[[na]] <- NULL

    # strikethru imputed columns for visual clarity
    codelist <- attr(x$`_tablet_name`, 'codelist')
    x$`_tablet_original` <- unlist(codelist[x$`_tablet_name`])
    # very elegant, but blows away attributes
    # x %<>% mutate(
    #   across(
    #     .cols = -starts_with('_tablet_'),
    #     .fns = ~ ifelse(`_tablet_original` %in% names(conf$imputed), '-', .x)
    #   )
    # )
    nms <- names(x)
    nontargets <- grepl('^_tablet_', nms)
    targets <- !nontargets
    #targets <- x %>% select(-starts_with('_tablet_')) %>% names
    imputed <- x$`_tablet_original` %in% names(conf$imputed)
    if(length(imputed) & length(targets)) x[imputed, targets] <- '-'
    x$`_tablet_original` <- NULL
    x %<>% as_kable(caption = conf$title)
    x %<>% kable_classic(full_width = F, html_font = "Cambria")
    x %<>% kable_styling(fixed_thead = T)
    x
  })

  tex <- reactive({
    printer('tex')
    #browser()
    old <- opts_knit$get('out.format')
    opts_knit$set(out.format = 'latex')
    # options(knitr.kable.NA = escape_latex(conf$na_string))
    options(knitr.kable.NA = 0)

    fun <- tablet
    if(conf$sequential) fun <- splice
    args <- args()
    # browser()
    if(!is.null(input$labeltex)){
      # browser()
      if(input$labeltex == 'yes'){
        printer('using spork')
        args$x %<>% modify(title = .data$tex) # should have class 'latex', unescaped
        #args$x %<>% modify(codelist = lapply(codelist, kableExtra:::escape_latex2))
      } else {
        # args$x %<>% modify(title = kableExtra:::escape_latex(title))
        # otherwise trap specials and pre-double secondary backslash
        args$x %<>% modify(title = tablet::escape_latex(title))
        #args$x %<>% modify(codelist = lapply(codelist, kableExtra:::escape_latex2))
      }
    } else {
      printer('no labeltex yet')
      return()
      # next maybe unnecessary if as_kable auto-escapes names(index) in >= 0.4.2
      # args$x %<>% modify(title = kableExtra:::escape_latex(title))

    }

    # call tablet
    x <- do.call(fun, args)
    # remove NA groups
    na <- which(names(x) == 'NA')
    for(i in rev(na))x[[na]] <- NULL

    # strikethru imputed columns for visual clarity
    codelist <- attr(x$`_tablet_name`, 'codelist')
    x$`_tablet_original` <- unlist(codelist[x$`_tablet_name`])
    # very elegant, but blows away attributes
    # x %<>% mutate(
    #   across(
    #     .cols = -starts_with('_tablet_'),
    #     .fns = ~ ifelse(`_tablet_original` %in% names(conf$imputed), '-', .x)
    #   )
    # )
    targets <- x %>% select(-starts_with('_tablet_')) %>% names
    imputed <- x$`_tablet_original` %in% names(conf$imputed)
    if(length(imputed) & length(targets)) x[imputed, targets] <- '-'
    x$`_tablet_original` <- NULL
    if(!nrow(x)){
      showNotification(duration = 5, type = 'error', 'nothing selected')
      return(character(0))
    }

    # _tablet_name has been thoroughly pre-escaped for all cases.
    # however, it is created as factor.
    # we flag it as latex to invoke the right method in as_kable(escape_latex = tablet::escape_latex)
    x %<>% mutate(`_tablet_name` = as_latex(`_tablet_name`))
    x %<>% as_kable(format = 'latex', caption = escape_latex(conf$title), longtable = TRUE)
    if(length(input$repeatheader) == 1){
      if(input$repeatheader == 'yes'){
        x %<>% kable_styling(latex_options = 'repeat_header', repeat_header_text = '')
      }
    }
    x %<>% footnote(general = unlist(strsplit(conf$footnotes, '\n')),fixed_small_size = TRUE, general_title = " ",threeparttable = TRUE)
    x %<>% as.character

    # insert footnote on every page

    cont <- input$cont
    mycont <- NULL
    if(!is.null(cont)){
      if(nchar(cont) > 0){
        mycont <- c(
          paste0('\\multicolumn{1}{r}{\\emph{', cont, '}}\\\\'),
          '\\midrule'
        )
      }
    }

    insertion <- c(
      '\\endhead',
      '\\midrule',
      mycont,
      '\\insertTableNotes'
    )
    insertion <- paste(insertion, collapse = '\n')
    if(length(input$repeatfootnote) == 1){
      if(input$repeatfootnote == 'yes'){
        x %<>% sub('\\endhead', insertion, ., fixed = TRUE)
      }
    }
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
        paste0('\\lhead{', escape_latex(conf$lhead1),' \\\\ ',escape_latex(conf$lhead2), '}'),
        '%\\chead{Table 0.0.0.xxx}',
        paste0('\\rhead{', escape_latex(conf$rhead1),' \\\\ ',escape_latex(conf$rhead2), '}'),
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

  output$buckets <- renderUI({
    printer('output$buckets')
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
    printer('output$splice')
    radioButtons(
      'splice',
      'Grouping Style',
      inline = TRUE,
      choices = c('nested','sequential'),
      selected = ifelse(conf$sequential,'sequential','nested')
    )

  })

  output$repeatheader <- renderUI({
    printer('output$repeatheader')
    radioButtons(
      'repeatheader',
      'repeat header on each page',
      inline = TRUE,
      choices = c('no','yes'),
      selected = conf$repeathead
    )
  })

  output$repeatfootnote <- renderUI({
    printer('output$repeatfootnote')
    radioButtons(
      'repeatfootnote',
      'repeat footnote on each page',
      inline = TRUE,
      choices = c('no','yes'),
      selected = conf$repeatfoot
    )
  })

  output$labelhtml <- renderUI({
    printer('output$labelhtml')
    radioButtons(
      'labelhtml',
      'scripted labels',
      inline = TRUE,
      choices = c('no','yes'),
      selected = conf$labelhtml
    )
  })

  output$labeltex <- renderUI({
    printer('output$labeltex')
    radioButtons(
      'labeltex',
      'scripted labels',
      inline = TRUE,
      choices = c('no','yes'),
      selected = conf$labeltex
    )
  })

  output$caption <- renderUI({
    printer('output$caption')
    textAreaInput('caption','Title', value = conf$title, resize = 'both')
  })

  output$outputid <- renderUI({
    printer('output$outputid')
    textInput('outputid','Output Identifier', value = conf$outputid)
  })

  output$lhead1 <- renderUI({
    printer('output$lhead1')
    textInput('lhead1','Left Header 1', value = conf$lhead1)
  })

  output$lhead2 <- renderUI({
    printer('output$lhead2')
    textInput('lhead2','Left Header 2', value = conf$lhead2)
  })

  output$rhead1 <- renderUI({
    printer('output$rhead1')
    textInput('rhead1','Right Header 1', value = conf$rhead1)
  })

  output$rhead2 <- renderUI({
    printer('output$rhead2')
    textInput('rhead2','Right Header 2', value = conf$rhead2)
  })

  output$cont <- renderUI({
    printer('output$cont')
    textInput('cont','Continued', value = conf$cont)
  })

  output$footnotes <- renderUI({
    printer('output$footnotes')
    textAreaInput('footnotes','Footnotes', value = conf$footnotes, resize = 'both')
  })

  # output$na_string <- renderUI({
  #   textInput('na_string','text substitute for NA', value = conf$na_string)
  # })

  output$keep <- renderUI({
    printer('output$keep')
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
    printer('output$data')
    if(!ncol(conf$x))return(structure(data.frame(` `='data goes here.', check.names = F), row.names = ' '))
    out <- conf$x
    #out %<>% resolve # already done
    out %<>% modify(name = paste(name, label, sep = ': '))
    out %<>% modify(name = paste0(name, ' (', .data$units, ')'))
    out
  })

  output$preview <- renderText({
    #printer('output$preview')
    if(!length(input$selected))return('Output goes here.')
    # ensure html
    opts_knit$set(out.format = 'html')
    x <- suppressMessages(html())
    x
  })

  pdf_location <- reactive({
    printer('pdf_location')
    x <- suppressWarnings(tex())
    if(!length(x))return('1x1.png')
    stem <- isolate(conf$outputid) # basename(tempfile())

    # backup
    writeLines(x, con = 'www/cache.tex')

    # clean slate
    unlink(file.path('www', paste0(stem, '.tex')))
    unlink(file.path('www', paste0(stem, '.pdf')))

    # some tables need to be run twice!  Not sure why!
    # particularly for repeat headers with nesting.
    # browser()
    path <- try(
      as.pdf(
        x,
        dir = 'www',
        stem = stem,
        clean = FALSE,
        ignore.stdout = TRUE
      )
    )

    # ignore incomplete pdf
    unlink(file.path('www', paste0(stem, '.pdf')))

    path <- try(
      as.pdf(
        x,
        dir = 'www',
        stem = stem,
        clean = TRUE,
        ignore.stdout = TRUE
      )
    )


    if(inherits(path, 'try-error')){
      showNotification(as.character(path), type = 'error', duration = 5)
    }

    if(!file.exists(path)) return('1x1.png')
    basename(path)

  })

  # https://stackoverflow.com/questions/19469978/displaying-a-pdf-from-a-local-drive-in-shiny

  output$pdfview <- renderUI({
    printer('output$pdfview')
    if(!ncol(conf$x))return('PDF displays here.')
    loc <- pdf_location()
    printer('directory')
    printer(getwd())
    printer('file')
    printer(loc)
    tags$iframe(
      style="height:600px; width:100%; scrolling:yes",
      src = paste0('/', loc)
    )
  })

  output$confpath <- renderPrint({
    #printer('output$confpath')
    req(conf$confpath)
    cat(conf$confpath)
    # if(!length(path)){
    #   cat('No configuration selected.')
    # } else {
    #   if(is.na(path)){
    #     cat('No configuration selected.')
    #   } else {
    #     cat(path)
    #   }
    # }
  })

  output$metapath <- renderPrint({
    #printer('output$metapath')
    if (!length(conf$metapath)) {
    cat('No data selected.')
    } else {
      cat(conf$metapath)
    }
  })

  # https://stackoverflow.com/questions/54304518/how-to-edit-a-yml-file-in-shiny

  output$saveMeta <- renderUI({
    printer('output$saveMeta')
    actionButton("saveMeta", label = "Save")
  })

  output$meta <- renderUI({
    printer("output$meta")
    current <- conf$editor
    if(!length(conf$metapath))return(current)
    if(is.na(conf$metapath))return(current)
    val <- NULL
    tryCatch(
      val <- readLines(conf$metapath),
      error = function(e) showNotification(
        duration = NULL,
        type = 'error',
        as.character(e)
      )
    )
    if(!is.null(val)){
      val <- aceEditor(
        outputId = "meta",
        value = val,
        mode = 'yaml',
        tabSize = 2
      )
      conf$editor <- val
    } else {
      val <- conf$editor
    }
    val
  })

  observeEvent(input$saveMeta, {

    printer('observeEvent: input$saveMeta')
    path <- isolate(conf$metapath)
    res <- try(yaml.load(input$meta))
    if(!inherits(res,'try-error')){
      res <- try(read_yamlet(input$meta))
    }
    err <- as.character(res)
    res <- !inherits(res, 'try-error')
    msg <- paste(ifelse(res, 'wrote', 'did not write'), path)
    if(!res) msg <- paste(msg, err)
    if(res){
      write(x = input$meta, file = path)
      # trigger redecoration
      conf$mv <- conf$mv + 1
    }
    dur <- 10
    if(res) dur <- 5
    showNotification(
      duration = dur,
      type = ifelse(res, 'default', 'error'),
      ui = msg
    )
  })
})
# Create Shiny app ----
shinyApp(ui, server)

# copyright 2021 Tim Bergsma bergsmat@gmail.com
