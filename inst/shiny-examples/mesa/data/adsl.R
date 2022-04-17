#' ---
#' title: "Test"
#' author: "Tim Bergsma"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::pdf_document:
#'     toc: true
#'     keep_tex: true
#' ---

library(tablet)
library(haven)
library(yamlet)
library(magrittr)
library(dplyr)
library(kableExtra)
library(knitr)

# make adsl with imputed bmi, imputed race, and two-row footnote
x <- read_sas('adsl.sas7bdat')
m <- read_yamlet('adsl.yaml')

# https://github.com/haozhu233/kableExtra/issues/703
names(m$race$guide)[[3]] <- 'Oriental'

# fortify to mimic app.R
have <- names(x)
need <- names(m)
make <- setdiff(need, have)
for(col in make) x[[col]] <- rep(NA_integer_, nrow(x))

# ensure positive nrow
if(nrow(x) == 0) x <- x['',,drop = FALSE]

# drop unspecified
x %<>% select(!!!names(m))

# apply meta
x <- redecorate(x, m)

# # Promote NA to a level of the factor
# x %<>% resolve(exclude = NULL)
x %<>% resolve()

foot <-
'a clinicaltrial.gov
b some other comment'
options(knitr.kable.NA = 0)
#opts_knit$set(out.format = 'latex')
# debug(tablet:::widgets.devalued)
#x %>% group_by(trt01a, trt01aa) %>% select(race) %>% tablet
x$trt01a[] <- NA
x$trt01aa[] <- NA
#debug(categoricals)
#debug(numerics)
#debug(groupfull)
x <- x %>%
  filter(saffl == 'Y') %>%
  group_by(trt01a, trt01aa) %>%
  select(
  #  age, agegr, sex, weight, bmi,
    race, bmi
  ) %>%
  tablet(
    all_levels = TRUE,
    fun = list(
      sum ~  sum(x,  na.rm = TRUE),
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
      `Mean (SD)` ~  ave + ' (' + std + ')',
      Median ~  paste(med),
      `Min, Max` ~  min + ', ' + max
    ),
    fac = list(
      ` ` ~ ifelse(sum == 0, '0', sum + ' (' + pct + '%' + ')')
    )
  )

# reverse lookup on make

codelist <- attr(x$`_tablet_name`, 'codelist')
x$`_tablet_original` <- unlist(codelist[x$`_tablet_name`])
# very elegant, but blows away attributes
# x %<>% mutate(
#   across(
#     .cols = -starts_with('_tablet_'),
#     .fns = ~ ifelse(`_tablet_original` %in% names(conf$imputed), '-', .x)
#   )
# )
targets <- seq_along(x)[!(grepl('_tablet_', names(x)))]
imputed <- x$`_tablet_original` %in% make
if(length(imputed) & length(targets)) x[imputed, targets] <- '-'
x$`_tablet_original` <- NULL

x %>%
  as_kable %>%
  footnote(
    general = # escape_latex(
      c('a something','b something')
      # )
    ,
    fixed_small_size = TRUE,
    general_title = " ",
    threeparttable = TRUE
  ) %>%
  kable_styling(latex_options = 'scale_down')

