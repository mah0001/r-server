% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/category_frequency.R
\name{categoryFrequency}
\alias{categoryFrequency}
\title{get category frequency}
\usage{
categoryFrequency(varRepoInput, csvpath)
}
\arguments{
\item{varRepoInput}{variable repository details}

\item{csvpath}{input csv file path}
}
\value{
A list of updated variable statistics
}
\description{
calculate frequencies of variable categories when apply categories from variable repo
}
\examples{
 categoryFrequency({"variables":["REGION"],"catgry":[{"catStat":{"text":""},"catValu":1,"labelled":true,"labl":"One"},{"catStat":{"text":""},"catValu":2,"labelled":true,"labl":"Two"},{"catStat":{"text":""},"catValu":3,"labelled":true,"labl":"Three"}]},'D:\\\\mde\\\\f1.csv')

}
