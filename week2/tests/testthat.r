library(testthat)
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

