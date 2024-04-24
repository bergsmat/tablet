library(testthat)
test_that('tablet package result is stable',{
  library(boot)
  library(magrittr)
  library(dplyr)
  library(yamlet)
  library(tidyr)
  library(knitr)
  library(kableExtra)
  library(tablet)
  # no groups

  # expect_equal_to_reference(
  #   file = '001.rds',
  #   data.frame() %>% tablet
  # )
  #
  # expect_equal_to_reference(
  #   file = '002.rds',
  #   data.frame(x = numeric()) %>% tablet
  # )
  #
  # expect_equal_to_reference(
  #   file = '003.rds',
  #   data.frame(x = numeric(0)) %>% tablet
  # )

  expect_equal_to_reference(
    file = '004.rds',
    data.frame(x = numeric(1)) %>% tablet %>% tablette
  )

  # expect_equal_to_reference(
  #   file = '005.rds',
  #   data.frame(x = factor()) %>% tablet
  # )

  expect_equal_to_reference(
    file = '006.rds',
    data.frame(x = factor('foo')) %>% tablet %>% tablette
  )
  # just one group
  # expect_equal_to_reference(
  #   file = '007.rds',
  #   data.frame(x = numeric()) %>% group_by(x) %>% tablet
  # )

  # expect_equal_to_reference(
  #   file = '008.rds',
  #   data.frame(x = numeric(0)) %>% group_by(x) %>% tablet
  # )

  # expect_equal_to_reference(
  #   file = '009.rds',
  #   data.frame(x = numeric(1)) %>% group_by(x) %>% tablet
  # )

  # expect_equal_to_reference(
  #   file = '010.rds',
  #   data.frame(x = factor()) %>% group_by(x) %>% tablet
  # )

  # expect_equal_to_reference(
  #   file = '011.rds',
  #   data.frame(x = factor('foo')) %>% group_by(x) %>% tablet
  # )

  # one group one value
  # expect_equal_to_reference(
  #   file = '012.rds',
  #   data.frame(x = numeric(), y = numeric()) %>% group_by(y) %>% tablet
  # )

  # expect_equal_to_reference(
  #   file = '013.rds',
  #   data.frame(x = numeric(), y = numeric()) %>% group_by(y) %>% tablet
  # )

  # expect_equal_to_reference(
  #   file = '014.rds',
  #   data.frame(x = factor(), y = numeric()) %>% group_by(y) %>% tablet
  # )

  expect_equal_to_reference(
    file = '015.rds',
    data.frame(x = numeric(1), y = numeric(1)) %>% group_by(y) %>% tablet %>% tablette
  )

  expect_equal_to_reference(
    file = '016.rds',
    data.frame(x = factor('foo'), y = numeric(1)) %>% group_by(y) %>% tablet %>% tablette
  )

  # problematic names
  expect_error(
    data.frame(`_tablet_name` = 1, check.names = F) %>% tablet
  )

  expect_error(
    data.frame(`_tablet_level` = 1, check.names = F) %>% tablet
  )

})

# test_that('tablet lists stats in the order specified by num and fac',{
#   library(boot)
#   library(dplyr)
#   library(magrittr)
#   lev <- melanoma %>%
#    select(-time, -year) %>%
#    mutate(sex = factor(sex), ulcer = factor(ulcer)) %>%
#    group_by(status) %>%
#    tablet(
#      num = list(
#        `Median (range)` ~ med + ' (' + min + ', ' + max + ')',
#        `Mean (SD)` ~ ave + ' (' + std + ')'
#      )
#    ) %>% 
#     tablette %$% 
#     `_tablet_stat` %>% levels
#   expect_identical(lev, c(" ","Median (range)", "Mean (SD)"))
# })

test_that('tablet works for decorated or undecorated solitary numerics or categoricals',{
  library(magrittr)
  library(tidyr)
  library(dplyr)
  library(yamlet)
  library(boot)
  x <- as_decorated(melanoma)
  x %<>% mutate(sex = factor(sex))
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  y <- decorate(file) %>% resolve

  expect_silent(x %>% as.data.frame %>% select(sex) %>% tablet)
  expect_silent(x %>% select(sex) %>% tablet)

  expect_silent(x %>% as.data.frame %>% select(age) %>% tablet)
  expect_silent(x %>% select(age) %>% tablet)

  expect_silent(y %>% as.data.frame %>% select(Age) %>% tablet)
  expect_silent(y %>% select(Age) %>% tablet)

  (y %>% as.data.frame %>% select(Race) %>% tablet)
  (y %>% select(Race) %>% tablet)

  (y %>% as.data.frame %>% select(Race) %>% numerics)
  (y %>% select(Race) %>% numerics)


})

test_that('splice returns tablet',{
  library(boot)
  library(dplyr)
  library(magrittr)
  x <- melanoma %>%
   select(-time, -year) %>%
   mutate(sex = factor(sex), ulcer = factor(ulcer)) %>%
   mutate(status2 = ifelse(status == 2, 2, 4)) %>%
   group_by(status, status2) %>%
   splice
  expect_true(inherits(x,'tablet'))

})

test_that('class latex on first target propagates to c1',{
  library(dplyr)
  library(magrittr)
  library(haven)
  library(yamlet)
  library(spork)

  x <- system.file(package = 'tablet', 'shiny-examples/mesa/data/adsl.sas7bdat')
  x %<>% read_sas %>% data.frame
  decorations(x) # note weight in pounds
  x %<>% mutate(weight = signif(digits = 3, weight * 2.2))

  # calculate BMI by assuming all males are 1.75 m, all females 1.63 cm
  x %<>% mutate(height = ifelse(sex == 'F', 1.63, 1.75))
  x %<>% mutate(bmi = signif(digits = 3, weight / (height^2)))
  x %<>% filter(saffl == 'Y')
  x %<>% select(trt01a, age, sex, weight, bmi)
  x %<>% redecorate('
  trt01a: [ Treatment, [ Placebo, TRT 10 mg, TRT 20 mg ]]
  age:    [ Age, year ]
  sex:    [ Sex, [ Female: F, Male: M ]]
  weight: [ Body Weight, kg ]
  bmi:    [ Index_body mass, kg/m^2 ]
  ')
  x %<>% resolve
  x %<>% group_by(trt01a)

  x %>% tablet %>% as_kable

  # supply default and unit-conditional latex titles
  x %<>% modify(title = concatenate(as_latex(as_spork(c(.data$label)))))
  x %<>% modify(
  age, weight, bmi,
    title = concatenate(
      sep = '',  # default ok in pdf
      as_latex(
        as_spork(
          c(.data$label, ' (', .data$units, ')')
        )
      )
    )
  )
  
  x %>% decorations
  y <- tablet(x)
  z <- tablette(y)
  expect_true(inherits(z[[1]], 'latex'))
})

test_that('as_xtable does not sort records',{
  library(tablet)
  library(magrittr)
  library(dplyr)
  x <- data.frame(
    id = c(1,2,3,4), 
    group = c('adult', 'adult', 'ped', 'ped'),
    auc = c(50, 40, 30, 20)
  )
  
  all <- x %>% select(all = auc) %>% tablet
  adt <- x %>% filter(group == 'adult') %>% select(adults = auc) %>% tablet
  ped <- x %>% filter(group == 'ped') %>% select(pediatrics = auc) %>% tablet
  
  y <- suppressWarnings(bind_rows(all, adt, ped))
  z <- as_xtable(y)
  z <- capture.output(z)
  z <- z[!grepl('^%', z)]
  
  expect_equal_to_reference(file = '017.rds',z)

})
