context("resequence dataset")
library(mde)

test_that("can resequence datasets", {
  varDescr <- fromJSON('[
                       {"name":["age"],"internalName":["age"],"dcml":["0"],"type":["numeric"],"catVal":[],"width":["16"],"StartPos":[null],"EndPos":[null],"dataType":[null]},
                       {"name":["gender"],"internalName":["gender"],"dcml":["0"],"type":["numeric"],"catVal":[],"width":["16"],"StartPos":[null],"EndPos":[null],"dataType":[null]},
                       {"name":["income"],"internalName":["income"],"dcml":["0"],"type":["numeric"],"catVal":[],"width":["16"],"StartPos":[null],"EndPos":[null],"dataType":[null]}
                       ]')
  datafile <- 'test.csv'
  resp <- resequence(varDescr, datafile)
  expect_equal(resp[["result"]], 'ok')
})

