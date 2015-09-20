library(testthat)
library(covr)
source('~/week2/R/pollutantmean.R')
source('~/week2/R/complete.R')

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

test_that("should return 1.281 when calculating 23 file nitrate mean",{
  actual <- pollutantmean("~/week2/specdata", "nitrate", 23)
  expect_that(actual[1],equals(1.281))
})

context("complete")
test_that("should return 1 1 117 for query 1",{
  actual <- complete("~/week2/specdata", 1) 
  expect_that(actual[1,1], equals(1)) 
  expect_that(actual[1,2], equals(117))
})

test_that("should return 2 1041, 4 474, 8 192, 10 148, 12 96 for query 2, 4, 8, 10, 12",{
  actual <- complete("~/week2/specdata", c(2, 4, 8, 10, 12)) 
  expect_that(actual[1,1], equals(2)) 
  expect_that(actual[1,2], equals(1041))
  
  expect_that(actual[2,1], equals(4)) 
  expect_that(actual[2,2], equals(474))
  
  expect_that(actual[3,1], equals(8)) 
  expect_that(actual[3,2], equals(192))
  
  expect_that(actual[4,1], equals(10)) 
  expect_that(actual[4,2], equals(148))
  
  expect_that(actual[5,1], equals(12)) 
  expect_that(actual[5,2], equals(96))
})

test_that("should return 30 932, 29 711, 28 475, 27 338, 26 586, 25 463 for query 30:25",{
  actual <- complete("~/week2/specdata", 30:25) 
  expect_that(actual[1,1], equals(30)) 
  expect_that(actual[1,2], equals(932))
  
  expect_that(actual[2,1], equals(29)) 
  expect_that(actual[2,2], equals(711))
  
  expect_that(actual[3,1], equals(28)) 
  expect_that(actual[3,2], equals(475))
  
  expect_that(actual[4,1], equals(27)) 
  expect_that(actual[4,2], equals(338))
  
  expect_that(actual[5,1], equals(26)) 
  expect_that(actual[5,2], equals(586))
  
  expect_that(actual[6,1], equals(25)) 
  expect_that(actual[6,2], equals(463))
})


test_that("should return 1 3 243 for query 3",{
  actual <- complete("~/week2/specdata", 3) 
  expect_that(actual[1,1], equals(3)) 
  expect_that(actual[1,2], equals(243))
})

 