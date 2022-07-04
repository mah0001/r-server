#' @title resequence variables
#'
#' @description Calculate variable width, startpos endpos and update the variable info
#'
#' @param varDescr variable description
#' @param datafile input data(csv) file path

#'
#' @return A list of status
#'
#' @examples  resequence([{"name":["hhid"],"internalName":["hhid"],"dcml":["0"],"type":["numeric"],"catVal":[],"width":["16"],"StartPos":[null],"EndPos":[null],"dataType":[null]}],'D:\\F1.csv')
#'
#' @export resequence

resequence <- function(varDescr, datafile){

  tryCatch({
    # flatten the jsonData (valRange and valFOrmat objects are the only ones flattend)
    flattenData <- flatten(varDescr, recursive = TRUE)

    # read data file
    DF_DATA <- .read_data(flattenData, file=datafile)

    # Calculate width, start column and end column
    startCol <- 0
    endCol <- 0
    for (i in 1:nrow(flattenData)){

      variable.name <- flattenData$internalName[[i]]

      # Get category values
      category.values <- flattenData$catVal[[i]]

      # no. of decimal positions
      dcml <- flattenData$dcml[[i]]

      if( flattenData$type[[i]] == "numeric"){
        # to handle the scenario where dcml greater than the length of the variable
        # for ex: value =  1, dcml = 2, then value will be converted to 1.00'
        DF_DATA[[variable.name]] <- .format_num(DF_DATA[[variable.name]], dcml=dcml)
      } else if(flattenData$type[[i]] == "date") {
        DF_DATA[[variable.name]] <- format(DF_DATA[[variable.name]])
      }

      # calculate variable length
      length <- .varibale_width(DF_DATA[[variable.name]], flattenData$type[[i]], category.values)

      #length <- nchar(maxValue)
      startCol <- endCol +1
      endCol <- endCol + length

      # Update variable information JSON
      flattenData$width[[i]] <- length
      flattenData$StartPos[[i]] <- startCol
      flattenData$EndPos[[i]] <- endCol

    }

    return (list(result='ok', variables=flattenData))

  }, warning = function(war) {
    return(list(result='warning', message= paste('warning >> ',war)))
  }, error = function(err) {
    return(list(result='error', message= paste('error >> ',err)))
  })

}
