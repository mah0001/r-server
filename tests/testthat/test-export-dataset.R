context("export dataset")
library(mde)

test_that("can export datasets", {
  varDescr <- fromJSON('[
                       {"name":["age"],"internalName":["age"],"labl":["age"],"val":[],"dcml":["0"],"width":["16"],"type":["numeric"],"intrvl":["contin"],"dataType":["double"]},
                       {"name":["gender"],"internalName":["gender"],"labl":["gender"],"val":[],"dcml":["0"],"width":["16"],"type":["numeric"],"intrvl":["contin"],"dataType":["double"]},
                       {"name":["income"],"internalName":["income"],"labl":["income"],"val":[],"dcml":["0"],"width":["16"],"type":["numeric"],"intrvl":["contin"],"dataType":["double"]}
                       ]')
  datafile <- 'test.csv'
  type <- 'csv'
  outfile <- 'test.csv'
  resp <- export(varDescr, datafile, type, 12, outfile)
  expect_equal(resp[["result"]], 'ok')
})

