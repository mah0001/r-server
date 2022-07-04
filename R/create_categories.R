#' @title Create categories from variable statistics
#'
#' @description Create categories from variable statistics
#'
#' @param listOfVariables list of variables
#' @param datafile input data(csv) file path
#' @param catgryMaxLimit category maximum limit

#'
#' @return A list of updated variables
#'
#' @examples  createCategories(["personnr"],'D:\\mde\\F1.csv', 50)
#'
#' @export createCategories

createCategories <- function( listOfVariables, datafile, catgryMaxLimit ) {

  tryCatch({

    # read matching variables from CSV
    matchingDF <- .read_matchingVariables(listOfVariables, datafile)

    # updated variable list
    updatedVariableList <- lapply(listOfVariables, function(varName){

      freqTable <- na.omit(count(matchingDF[[varName]]))
      colnames(freqTable) <- c("Value","freq")

      # if number of distinct values less than the limit(1500) then create categories
      if(nrow(freqTable) < catgryMaxLimit){
        catgry <- lapply(freqTable$Value, function(value){
          freq <- freqTable[freqTable$Value==value, 'freq']
          list(catValu=value,labl=value,labelled=TRUE,catStat=list(type="freq",text=freq))
        })
      } else {
        catgry <- NA
      }

      list(name=varName, catgry=catgry)
    })

    return(list(result='ok', data=updatedVariableList))
  }, warning = function(war) {
    return(list(result='warning', message= paste('warning >> ',war)))
  }, error = function(err) {
    return(list(result='error', message= paste('error >> ',err)))
  })
}
