context("destring dataset")
library(mde)

test_that("can destring datasets", {
  varDescr <- fromJSON('[
                          {"name":["age"],"internalName":["age"],"type":["character"],"dataType":["character"]},
                          {"name":["gender"],"internalName":["gender"],"type":["character"],"dataType":["character"]},
                          {"name":["income"],"internalName":["income"],"type":["character"],"dataType":["character"]}
                       ]')
  datafile <- 'test.csv'
  variables <- fromJSON('["age", "gender", "income"]')
  resp <- destring(varDescr, datafile, datafile, variables)
  expect_equal(resp[["result"]], 'ok')
})
