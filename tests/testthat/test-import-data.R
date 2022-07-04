context("import dataset")
library(mde)

test_that("can import datasets", {
  datapath <- 'testdata.dta'
  type <- 'dta'
  fileid <- 'F1'
  freqlimit <- 50
  resp <- import(datapath, type, fileid, freqlimit)
  expect_equal(resp[["result"]], 'ok')
  expect_equal(resp[["cnt"]], 2)
})

test_that("can write csv file", {
  datapath <- 'testdata.dta'
  type <- 'dta'
  outfile <- 'test.csv'
  resp <- writeCSV(datapath, type, outfile)
  expect_equal(resp[["result"]], 'ok')
})

