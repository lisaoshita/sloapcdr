
# ===================================================
# Testing if read.aqs returns the correct data frames
# ===================================================



context("Testing returned data frames")
library(sloapcdr)




test_that("read.aqs at level 0 returns data frames correctly", {

  expect_identical(read.aqs(filename = "H:/TECH/Lisa/R/sloapcdr/tests/testthat/testdata.txt",
                            level = 0,
                            remove = FALSE),
                   df)

})
