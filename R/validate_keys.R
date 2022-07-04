#' @title validate keys
#'
#' @description Check whether the base keys are unique or not
#'
#' @param keyVariables list of key variables
#' @param datafile input data(csv) file path

#'
#' @return A list contains the index of duplicate values
#'
#' @examples  validateKeys(c("hhid","indid"),'D:\\mde\\HND_2012_L2L.csv')
#'
#' @export validateKeys

validateKeys <- function( keyVariables, datafile ) {

  tryCatch({

    # read matching variables from CSV
    matchingDF <- .read_matchingVariables(keyVariables, datafile, default="c")

    # find the duplicate rows
    duplicateRows <- which(duplicated(matchingDF))

    # find first and second occurance duplicate rows
    if(length(duplicateRows) > 0){
      duplicateIndex <- duplicateRows[1]
      duplicateRow <- matchingDF[duplicateIndex,]
      if (!is.data.frame(matchingDF)){
        duplicateRow <- data.frame(duplicateRow, stringsAsFactors=FALSE)
        colnames(duplicateRow) <- keyVariables
      }
      matchingIndex <- row.names(match_df(matchingDF, duplicateRow, on = keyVariables))
      firstIndex <- matchingIndex[1]
      return(list(result='ok', data=c(firstIndex, duplicateIndex)))
    } else {
      return(list(result='ok', data=c(0)))
    }
  }, warning = function(war) {
    return(list(result='warning', message= paste('warning >> ',war)))
  }, error = function(err) {
    return(list(result='error', message= paste('error >> ',err)))
  })
}
