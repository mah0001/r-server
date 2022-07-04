context("calculate category frequency")
library(mde)

test_that("can calculate category frequency", {
  varDescr <- fromJSON('[
                        {"variables":["gender"],"catgry":[{"catStat":{"text":""},"catValu":1,"labelled":true,"labl":"male"},{"catStat":{"text":""},"catValu":2,"labelled":true,"labl":"female"}]}
                       ]')
  datafile <- 'test.csv'
  resp <- categoryFrequency(varDescr, datafile)
  expect_equal(resp[["result"]], 'ok')
})
