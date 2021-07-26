## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
#knitr::opts_chunk$set(package.startup.message = FALSE)
options(xtable.comment = FALSE)

## ----software, include = FALSE------------------------------------------------
library(tidyr)
library(dplyr)
library(magrittr)
library(kableExtra)
library(boot)
library(yamlet)
library(tablet)
# options(knitr.table.format = "latex") # not needed since kableExtra 0.9.0

## ----software2, eval = FALSE--------------------------------------------------
#  library(tidyr)
#  library(dplyr)
#  library(magrittr)
#  library(kableExtra)
#  library(boot)
#  library(yamlet)
#  library(tablet)

## ---- data--------------------------------------------------------------------
x <- melanoma
x %<>% select(-time, -year)

## ---- easy--------------------------------------------------------------------
x %>%
  mutate(
    sex = factor(sex), 
    ulcer = factor(ulcer)
  ) %>%
  tablet %>%
  as_kable

## -----------------------------------------------------------------------------
x <- melanoma

x %<>% decorate('
time:      [ Survival Time Since Operation, day ]
status:
 - End of Study Patient Status
 -
  - Alive: 2
  - Melanoma Death: 1
  - Unrelated Death: 3
sex:       [ Sex, [ Male: 1, Female: 0 ]]
age:       [ Age at Time of Operation, year ]
year:      [ Year of Operation, year ]
thickness: [ Tumor Thickness, mm ]
ulcer:     [ Ulceration, [ Absent: 0, Present: 1 ]]
')
x %<>% select(-time, -year)
x %<>% group_by(status)
x %<>% resolve
x %<>% modify(
  age, thickness, 
  title = paste0(label, ' (', units, ')')
)

## ----meta---------------------------------------------------------------------
x %>% tablet %>% as_kable

## ----xtable, results = 'asis'-------------------------------------------------
library(xtable)
  x %>% 
    filter(!(status == 'Alive' & sex == 'Male')) %>%
    tablet %>% as_xtable %>% 
    print(
      booktabs = TRUE, 
      include.rownames = FALSE 
    )

## ---- grouped-----------------------------------------------------------------
x %<>% mutate(class = status)                          # copy the current group
x %<>% modify(class, label = 'class')                  # change its label
levels(x$status) <- c('Alive','Melanoma','Unrelated')  # tweak current group
levels(x$class)  <- c(' ',    'Death',   'Death')      # cluster groups
x %<>% group_by(class, status)                         # nest groups
x %>% tablet %>% as_kable                              # render

## ---- transposed--------------------------------------------------------------
x %<>% group_by(status, sex)
x %<>% select(-class)
x %>% 
  tablet %>% 
  as_kable %>% 
  kable_styling(latex_options = 'scale_down')


## ---- transposed2-------------------------------------------------------------
x %<>% group_by(status, ulcer)
x %>% 
  tablet %>% 
  as_kable %>% 
  kable_styling(latex_options = 'scale_down')


## ---- transposed3-------------------------------------------------------------
x %<>% group_by(status, ulcer, sex)
x %>% 
  tablet %>% 
  as_kable %>% 
  kable_styling(latex_options = 'scale_down') # %>% landscape ?


## ---- aesthetics--------------------------------------------------------------
x %<>% group_by(status)
x %>% 
  tablet(
    fac = NULL,
    lab ~ name,
    `Median (range)` ~ med + ' (' + min + ' - ' + max + ')'
  ) %>% 
  as_kable


