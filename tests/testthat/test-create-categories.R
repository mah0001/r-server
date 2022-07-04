context("create categories from statistics")
library(mde)

test_that("can create categories from statistics", {
  listOfVariables <- fromJSON('["gender"]')
  datafile <- 'test.csv'
  catgryMaxLimit <- 50
  resp <- createCategories(listOfVariables, datafile, catgryMaxLimit)
  expect_equal(resp[["result"]], 'ok')
})
