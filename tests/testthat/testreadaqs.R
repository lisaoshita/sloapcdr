# ===================================================
# Testing if read.aqs returns the correct data frames
# ===================================================


# NOTES:
# If level 1 is ran without time.zone argument specified:
# Warning message:
# In strptime(x, format, tz = tz) :
#  unknown timezone 'zone/tz/2018c.1.0/zoneinfo/America/Los_Angeles' - cause issues?
# library(sloapcdr) outputs same timezone warning message


context("Testing form of data frames returned")
library(sloapcdr)


# ------------------
# Tests for level 0
# ------------------

test_that("read.aqs at level 0 returns data frames correctly", {

  datfile <- system.file("tests", "rd_data.txt", package = "sloapcdr")

  # with remove = FALSE
  expect_equal(class(read.aqs(file = datfile, level = 0)), "data.frame")
  expect_equal(ncol(read.aqs(file = datfile, level = 0)), 28)
  expect_equal(nrow(read.aqs(file = datfile, level = 0)), 6)
  expect_equal(class(read.aqs(file = datfile, level = 0)$Sample.Value), "numeric")
  # with remove = TRUE
  expect_equal(ncol(read.aqs(file = datfile, level = 0, remove = TRUE)), 9)
})


# ------------------
# Tests for level 1
# ------------------

test_that("read.aqs at level 1 returns data frames correctly", {

  datfile <- system.file("tests", "rd_data.txt", package = "sloapcdr")

  # with remove = FALSE
  expect_equal(class(read.aqs(file = datfile, level = 1)$Date.Time)[1], "POSIXct")
  expect_equal(ncol(read.aqs(file = datfile, level = 1)), 10)
  # with remove = TRUE
  expect_equal(ncol(read.aqs(file = datfile, level = 1, remove = T)), 6)
})


# ------------------
# Tests for level 2
# ------------------

test_that("read.aqs at level 2 returns data frames correctly", {

  datfile <- system.file("tests", "rd_data.txt", package = "sloapcdr")

  # with remove = FALSE
  expect_equal(ncol(read.aqs(file = datfile, level = 2)), 3)
  # with remove = TRUE
  expect_equal(ncol(read.aqs(file = datfile, level = 2, remove = TRUE)), 3)

})


# ------------------
# Tests for level 3
# ------------------

test_that("read.aqs at level 3 returns data frames correctly", {

  datfile <- system.file("tests", "rd_data.txt", package = "sloapcdr")

  # with remove = FALSE
  expect_equal(ncol(read.aqs(file = datfile, level = 3)), 3)
  # with remove = TRUE
  expect_equal(ncol(read.aqs(file = datfile, level = 3)), 3)

})


# ------------------
# Tests for level 4
# ------------------

test_that("read.aqs at level 4 returns data frames correctly", {

  datfile <- system.file("tests", "rd_data.txt", package = "sloapcdr")

  # with remove = FALSE
  expect_equal(ncol(read.aqs(file = datfile, level = 4)), 6) # test dimensions
  expect_equal(nrow(read.aqs(file = datfile, level = 4)), 6)

  # test if first column is Date.Time
  df <- sloapcdr::read.aqs(datfile, level = 4)
  date <- colnames(df)[1]
  expect_equal(date, "Date.Time")

  # with remove = TRUE
  expect_equal(ncol(read.aqs(file = datfile, level = 4, remove = TRUE)), 6)
  expect_equal(nrow(read.aqs(file = datfile, level = 4, remove = TRUE)), 6)

})

# TO DO:
# for level 0 - if remove = T, test if all remaining columns have different values
# test if monitor labels were concatenated in the right order??
# if correct labels were applied?


