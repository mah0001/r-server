#' @title convert TSV to CSV format
#'
#' @description Convert tab separated values to csv format
#'
#' @param varDescr variable description
#' @param datafile input TSV file path

#'
#' @return A list of status
#'
#' @examples  convertTSV([{"name":["indid"],"internalName":["indid"],"type":["numeric"],"dataType":[null]}],'D:\\mde\F1.tsv')
#'
#' @export convertTSV

convertTSV <- function(varDescr, datafile){

  tryCatch({
    # flatten the jsonData (valRange and valFOrmat objects are the only ones flattend)
    flattenData <- flatten(varDescr, recursive = TRUE)

    # read data file
    DF_DATA <- .read_data(flattenData, file=datafile, type="tsv")

    ## write csv with the options given below
    # file  - file path with encoding scheme
    # quote - non numeric values should be enclosed in quotes
    # na    - na will be replaced with *
    write_csv(DF_DATA, datafile, na = "*", append = FALSE)

    return (list(result='ok'))

  }, warning = function(war) {
    return(list(result='warning', message= paste('warning >> ',war)))
  }, error = function(err) {
    return(list(result='error', message= paste('error >> ',err)))
  })

}
