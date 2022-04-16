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
d <- read_sas('adsl.sas7bdat')
m <- read_yamlet('adsl.yaml')

# fortify to mimic app.R
have <- names(d)
need <- names(m)
make <- setdiff(need, have)
for(col in make) d[[col]] <- rep(NA_integer_, nrow(d))

# ensure positive nrow
if(nrow(d) == 0) d <- d['',,drop = FALSE]

# drop unspecified
d %<>% select(!!!names(m))

# apply meta
d <- redecorate(d, m)

# # Promote NA to a level of the factor
# d %<>% resolve(exclude = NULL)
d %<>% resolve()

foot <-
'a clinicaltrial.gov
b some other comment'
options(knitr.kable.NA = 0)
#opts_knit$set(out.format = 'latex')
# debug(tablet:::widgets.devalued)
#d %>% group_by(trt01a, trt01aa) %>% select(race) %>% tablet
d$trt01a[] <- NA
d$trt01aa[] <- NA
t <- d %>%
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

# reverse lookup on
imputed <- sapply(select(d, !!!make), attr, 'label')
#to substitute '-' for all imputeds
# t <- t[names(t)[!duplicated(names(t))]]
# t %<>% mutate(
#   across(
#     .cols = -starts_with('_tablet_'),
#     .fns = ~ ifelse(`_tablet_name` %in% imputed, '-', .x)
#   )
# )

t %>%
  as_kable %>%
  footnote(
    general = # escape_latex(
      c('a something','b something')
      # )
    ,
    fixed_small_size = TRUE,
    general_title = " ",
    threeparttable = TRUE
  ) # %>% as.character %>% writeLines

