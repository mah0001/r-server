context("export to fixed width text")
library(mde)

test_that("can export to fixed width text format", {
  varDescr <- fromJSON('[
                       {"name":["age"],"internalName":["age"],"labl":["age"],"val":[],"dcml":["0"],"width":["16"],"type":["numeric"],"intrvl":["contin"],"StorageType":"","ReadFormat":"","dataType":["double"]},
                       {"name":["gender"],"internalName":["gender"],"labl":["gender"],"val":[],"dcml":["0"],"width":["16"],"type":["numeric"],"intrvl":["contin"],"StorageType":"","ReadFormat":"","dataType":["double"]},
                       {"name":["income"],"internalName":["income"],"labl":["income"],"val":[],"dcml":["0"],"width":["16"],"type":["numeric"],"intrvl":["contin"],"StorageType":"","ReadFormat":"","dataType":["double"]}
                       ]')
  datafile <- 'test.csv'
  outpath <- 'test.txt'
  resp <- exportFWF(varDescr, datafile, outpath)
  expect_equal(resp[["result"]], 'ok')
})
