#' @title destring variables
#'
#' @description Convert the string variables to numeric variables
#'
#' @param varDescr variable description
#' @param datafile input data(csv) file path
#' @param updatedDataPath Updated data file path
#' @param listOfVariables list of variables to perform destring operation

#'
#' @return A list contains the destringed variables
#'
#' @examples  destring([{"name":["uqnr"],"internalName":["uqnr"],"type":["character"],"dataType":["character"]}],'D:\\mde\\F1(1).csv',['uqnr'])
#'
#' @export destring

destring <- function( varDescr, datafile, updatedDataPath, listOfVariables ) {

  tryCatch({

    # flatten the jsonData (valRange and valFormat objects are the only ones flattend)
    flattenData <- flatten(varDescr, recursive = TRUE)

    # Read csv header to pick the column names
    csvHeader <- as.character(read_csv(file=datafile, n_max = 1, col_types = cols(.default = "c"), col_names = FALSE))
    matchingVariables <- intersect(listOfVariables, csvHeader)

    # read csv file
    DF_DATA <- .read_data(flattenData, file=datafile)
    matchingDF <- DF_DATA [,matchingVariables]
    #Check if all values are numeric
    canConvertVars <- lapply(matchingVariables, function(col) {
      #type.convert - Convert a character vector to logical, integer, numeric, complex or factor as appropriate.
      if (class(matchingDF) == "data.frame") {
        new_vector <- type.convert(matchingDF[[col]], na.strings = "NA", as.is = TRUE,
                                   numerals = c("no.loss"))
      }
      else {
        new_vector <- type.convert(matchingDF, na.strings = "NA", as.is = TRUE,
                                   numerals = c("no.loss"))
      }
      msg <- ''
      status <- FALSE
      varStats <- NA
      # if converted vector is numeric. mode is numeric for both integer, numeric and factor
      if(mode(new_vector) == 'numeric'){
        status <- TRUE
        # convert string values to numeric
        DF_DATA[col] <- lapply(DF_DATA[col], as.numeric)
        # calculate statistics for numeric variables
        varStats <- .varStats(DF_DATA,col)
      } else {
        #. find the reason, why destring is failed & set the message
        if(max(nchar(DF_DATA[[col]])) > 15){
          msg <- "Variable with width > 15 can't be destringed."

        } else {

          #. find the values, failed to destring, get the first value and show in message
          # logic to find the values can't be destringed
          # ********************************************* #
          #. exclude NA from the variable
          #. convert the variable to numeric, all non - numeric values will be converted to NA
          #. check any NA in the converted values, if any find the actual value from the variable
          #. if multiple values are available, now showing only the first one

          excludeNAPerdicate <- !is.na(DF_DATA[[col]])
          nonNumericPredicate <- suppressWarnings(is.na(as.numeric(DF_DATA[[col]][excludeNAPerdicate])))
          if(any(nonNumericPredicate)) {
            nonNumericVals <- DF_DATA[[col]][excludeNAPerdicate][nonNumericPredicate]
            if(length(nonNumericVals) > 0){
              msg <- paste("Value", shQuote(nonNumericVals[1]), "can't be destringed.", sep = " ")
            }
          }
        }
      }
      list(name=col, status=status, msg=msg, varStats=varStats)
    })

    # convert the data to numeric
    for (i in (1:length(canConvertVars))){
      if(canConvertVars[[i]]$status){
        DF_DATA[canConvertVars[[i]]$name] <- lapply(DF_DATA[canConvertVars[[i]]$name], as.numeric)
      }
    }

    DF_DATA[ is.na(DF_DATA) ] <- NA    #missing values replaced with NA
    # write.csv(DF_DATA, file=file(updateddatapath, encoding="UTF-8"), row.names = FALSE, quote = TRUE, na= "*")  #write to CSV
    write_csv(DF_DATA, updatedDataPath, na = "*", append = FALSE)

    # return number of records
    #cv <- toJSON(canConvertVars,pretty=TRUE,force=TRUE)
    return(list(result='ok', variables=canConvertVars))
  }, warning = function(war) {
    return(list(result='warning', message= paste('warning >> ',war)))
  }, error = function(err) {
    return(list(result='error', message= paste('error >> ',err)))
  })

}
