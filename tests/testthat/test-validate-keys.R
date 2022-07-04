context("validate keys")
library(mde)

test_that("can validate keys", {
  keyVariables <- fromJSON('["gender"]')
  datafile <- 'test.csv'
  resp <- validateKeys(keyVariables, datafile)
  expect_equal(resp[["result"]], 'ok')
})
