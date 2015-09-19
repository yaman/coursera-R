library(testthat)
library(covr)
source('~/week2/R/pollutantmean.r')

context("normalizedFiles")
test_that("should return exactly one filename",{
  expect_that(length(normalizedFiles("specdata",1:1)),equals(1))
})

test_that("should return exactly two filename",{
  expect_that(length(normalizedFiles("specdata",1:2)),equals(2))
})

test_that("should return exactly 332 filename",{
  expect_that(length(normalizedFiles("specdata",1:332)),equals(332))
})

test_that("should return filename 001.csv",{
  filenames <- normalizedFiles("specdata",1:1)
  expect_that(filenames[1],equals("specdata/001.csv"))
})

test_that("should return filename 010.csv",{
  filenames <- normalizedFiles("specdata",10:10)
  expect_that(filenames[1],equals("specdata/010.csv"))
})

test_that("should return filename 332.csv",{
  filenames <- normalizedFiles("specdata",332:332)
  expect_that(filenames[1],equals("specdata/332.csv"))
})

context("pollutantmean")
test_that("should return 4.064 when calculating first 10 files sulfate mean",{
  actual <- pollutantmean("~/week2/specdata","sulfate",1:10)
  expect_that(actual[1],equals(4.064))
})

test_that("should return 1.706 when calculating 70:72 files nitrate mean",{
  actual <- pollutantmean("~/week2/specdata", "nitrate", 70:72)
  expect_that(actual[1],equals(1.706))
})

package_coverage()