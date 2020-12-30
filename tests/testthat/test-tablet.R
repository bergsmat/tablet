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
    data.frame(x = numeric(1)) %>% tablet
  )

  # expect_equal_to_reference(
  #   file = '005.rds',
  #   data.frame(x = factor()) %>% tablet
  # )

  expect_equal_to_reference(
    file = '006.rds',
    data.frame(x = factor('foo')) %>% tablet
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
    data.frame(x = numeric(1), y = numeric(1)) %>% group_by(y) %>% tablet
  )

  expect_equal_to_reference(
    file = '016.rds',
    data.frame(x = factor('foo'), y = numeric(1)) %>% group_by(y) %>% tablet
  )

  # problematic names
  expect_error(
    data.frame(`_tablet_name` = 1, check.names = F) %>% tablet
  )

  expect_error(
    data.frame(`_tablet_level` = 1, check.names = F) %>% tablet
  )

})
