gridtable_example <- function() {
  paste(
    "+------+------------+----------------------+" ,
    "| Key  | 001        | 002                  |" ,
    "+======+============+======================+" ,
    "| Dose | 5 mg       | 5 mg, 10 mg          |" ,
    "+------+------------+----------------------+" ,
    sep = "\n"
  )
}

expect_gridtable_runtime <- function() {}

column_widths <- function(x) {
  unname(vapply(x, attr, character(1), which = "width", exact = TRUE))
}

content_data_frame <- function(x) {
  out <- base::as.data.frame(
    lapply(seq_along(x), function(column_index) as.character(x[[column_index]])),
    stringsAsFactors = FALSE,
    optional = TRUE
  )
  names(out) <- names(x)
  row.names(out) <- NULL
  out
}

test_that("grid tables are normalized and validated", {
  grid <- as_gridtable(gridtable_example())

  expect_true(inherits(grid, "gridtable"))
  expect_true(is.character(grid))
  expect_match(as.character(grid), "^\\+[-]+\\+[-]+\\+[-]+\\+")

  duplicate_names <- paste(
    "+---+---+" ,
    "| A | A |" ,
    "+===+===+" ,
    "| x | y |" ,
    "+---+---+" ,
    sep = "\n"
  )

  expect_error(as_gridtable(duplicate_names), "unique")
  expect_error(as_gridtable("not a grid table"), "at least five")
})

test_that("data frames convert to grid tables", {
  x <- data.frame(study = 1:2, subjects = c(30, 40))

  grid <- as_gridtable(x)
  expected <- paste(
    "+-------+----------+",
    "| study | subjects |",
    "+=======+==========+",
    "| 1     | 30       |",
    "+-------+----------+",
    "| 2     | 40       |",
    "+-------+----------+",
    sep = "\n"
  )

  expect_true(inherits(grid, "gridtable"))
  expect_identical(as.character(grid), expected)
  expect_identical(as_gridtable(as.character(grid)), grid)
})

test_that("normalized grid tables preserve content through data-frame round trip", {
  expect_gridtable_runtime()

  grid <- as_gridtable(gridtable_example())
  roundtrip <- as_gridtable(as.data.frame(grid))

  expect_true(inherits(roundtrip, "gridtable"))
  expect_identical(as.character(roundtrip), as.character(grid))
})

test_that("simple data frames preserve content through gridtable round trip", {
  expect_gridtable_runtime()

  x <- data.frame(study = 1:2, subjects = c(30, 40))
  roundtrip <- as.data.frame(as_gridtable(x))

  expect_identical(class(roundtrip), c("calibrated", "data.frame"))
  expect_identical(content_data_frame(roundtrip), content_data_frame(x))
})

test_that("gridtable coercion preserves LaTeX markup", {
  grid <- as_gridtable(paste(
    "+--+--+",
    "| Parameter | Description |",
    "+==+==+",
    "| C$_{min,ss,i}$ (ng/mL) | Minimum |",
    "+--+--+",
    "| $AUC_{0-\\tau,ss,i}$ (ng $\\cdot$ h/mL) | Area |",
    "+--+--+",
    sep = "\n"
  ))
  expected <- c(
    "C$_{min,ss,i}$ (ng/mL)",
    "$AUC_{0-\\tau,ss,i}$ (ng $\\cdot$ h/mL)"
  )

  out <- as.data.frame(grid)
  latex <- paste(kbl(grid, format = "latex", booktabs = TRUE, escape = FALSE), collapse = "\n")

  expect_identical(as.vector(out$Parameter), expected)
  expect_true(grepl(expected[[1L]], latex, fixed = TRUE))
  expect_true(grepl(expected[[2L]], latex, fixed = TRUE))
})

test_that("grid tables coerce to calibrated data frames", {
  expect_gridtable_runtime()

  out <- as.data.frame(as_gridtable(gridtable_example()))

  expect_identical(class(out), c("calibrated", "data.frame"))
  expect_equal(names(out), c("Key", "001", "002"))
  expect_identical(as.vector(out[["Key"]]), "Dose")
  expect_identical(as.vector(out[["001"]]), "5 mg")
  expect_identical(as.vector(out[["002"]]), "5 mg, 10 mg")
  expect_equal(column_widths(out), c("2.64em", "2.64em", "5.72em"))
})

test_that("gridtable width scaling follows the package option", {
  expect_gridtable_runtime()

  grid <- as_gridtable(gridtable_example())
  base_widths <- as.numeric(sub("em$", "", column_widths(as.data.frame(grid))))

  old_options <- options(tablet.gridtable.scale = 2)
  on.exit(options(old_options), add = TRUE)
  scaled_widths <- as.numeric(sub("em$", "", column_widths(as.data.frame(grid))))

  expect_equal(scaled_widths, base_widths * 2)
})

test_that("as_calibrated assigns and recycles widths", {
  x <- data.frame(a = 1, b = 2, c = 3)

  exact <- as_calibrated(x, c("1em", "2em", "3em"))
  expect_identical(class(exact), c("calibrated", "data.frame"))
  expect_equal(column_widths(exact), c("1em", "2em", "3em"))

  expect_warning(few <- as_calibrated(x, "4em"), "Fewer widths")
  expect_equal(column_widths(few), c("4em", "4em", "4em"))

  expect_warning(many <- as_calibrated(x, c("1em", "2em", "3em", "4em")), "More widths")
  expect_equal(column_widths(many), c("1em", "2em", "3em"))

  expect_error(as_calibrated(x, NA_character_), "non-missing")
  expect_error(as_calibrated(x, character()), "at least one")
})

test_that("kbl consumes calibrated widths", {
  skip_if_not_installed("kableExtra")

  calibrated <- as_calibrated(data.frame(a = 1, b = 2), c("3em", "4em"))

  with_widths <- paste(kbl(calibrated, format = "latex", booktabs = TRUE), collapse = "\n")
  without_widths <- paste(kbl(calibrated, format = "latex", booktabs = TRUE, use_widths = FALSE), collapse = "\n")

  expect_match(with_widths, "p\\{3em\\}.*p\\{4em\\}")
  expect_false(grepl("p\\{3em\\}", without_widths))
})

test_that("kbl.gridtable calibrates before rendering", {
  expect_gridtable_runtime()
  skip_if_not_installed("kableExtra")

  grid <- as_gridtable(gridtable_example())

  with_widths <- paste(kbl(grid, format = "latex", booktabs = TRUE), collapse = "\n")
  without_widths <- paste(kbl(grid, format = "latex", booktabs = TRUE, use_widths = FALSE), collapse = "\n")

  expect_true(grepl("p\\{", with_widths))
  expect_false(grepl("p\\{", without_widths))
})

test_that("sloppy grid-table input is normalized", {
  sloppy <- paste(
    "+-+--+",
    "| study | subjects |",
    "+=+==+",
    "|1| 30|",
    "+-+--+",
    "| 2 |40 |",
    "+-+--+",
    sep = "\n"
  )
  expected <- paste(
    "+-------+----------+",
    "| study | subjects |",
    "+=======+==========+",
    "| 1     | 30       |",
    "+-------+----------+",
    "| 2     | 40       |",
    "+-------+----------+",
    sep = "\n"
  )
  
  grid <- as_gridtable(sloppy)
  
  expect_true(inherits(grid, "gridtable"))
  expect_identical(as.character(grid), expected)
})

test_that("gridtable widths respond to displayed column content", {
  expect_gridtable_runtime()
  
  grid <- as_gridtable(paste(
    "+-+-+",
    "| A | B |",
    "+=+=+",
    "| short | much longer entry |",
    "+-+-+",
    sep = "\n"
  ))
  
  out <- as.data.frame(grid)
  widths <- as.numeric(sub("em$", "", column_widths(out)))
  
  expect_gt(widths[[2]], widths[[1]])
})
