context("calculate string length")
library(mde)

test_that("can calculate string length", {
  listOfVariables <- fromJSON('[ {"internalName" : "gender", "type" : "character"} ]')
  datafile <- 'test.csv'
  resp <- stringLength(listOfVariables, datafile)
  expect_equal(resp[["result"]], 'ok')
})
