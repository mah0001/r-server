#' @title export dataset to fixed width text
#'
#' @description export dataset to fixed width text format
#'
#' @param varDescr variable description
#' @param datafile input data(csv) file path
#' @param outpath output fwf file path
#'
#' @return A list of variable
#'
#' @examples  exportFWF([{"name":["hhid"],"internalName":["hhid"],"catVal":[],"width":[4],"dcml":["0"],"StartPos":[1],"EndPos":[4],"type":["numeric"],"StorageType":"","ReadFormat":"","dataType":["double"]}],'D:\\mde\\f1.csv')
#'
#' @export exportFWF

exportFWF <- function( varDescr, datafile, outpath) {

  tryCatch({

    # read variable details from the json file
    jsonData <- varDescr

    # flatten the jsonData (valRange and valFormat objects are the only ones flattend)
    flattenData <- flatten(jsonData, recursive = TRUE)

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

      # Update JSON
      flattenData$width[[i]] <- length
      flattenData$StartPos[[i]] <- startCol
      flattenData$EndPos[[i]] <- endCol

      # Update storage type & read format

      if(flattenData$type[[i]] == "character"){

        # storage type "str<length>""
        # stata read format "%<length>s"
        flattenData$StorageType[[i]] <- paste("str", length, sep="")
        flattenData$ReadFormat[[i]] <- paste("%", length, "s", sep="")

      } else if( flattenData$type[[i]] == "numeric" ){

        # stata read format "%<length>.<decimal>s"

        flattenData$StorageType[[i]] <- .stata_datatype(DF_DATA[[variable.name]])
        if(dcml == 0){
          flattenData$ReadFormat[[i]] <- paste("%", length, "f", sep="")
        } else {
          flattenData$ReadFormat[[i]] <- paste("%", length, ".", dcml, "f", sep="")
        }


      } else if(flattenData$type[[i]] == "date") {

        # date read format & storage type
        flattenData$StorageType[[i]] <- "date"
        flattenData$ReadFormat[[i]] <- "%td"

      } else {
        # get type & read format from variable info
        flattenData$StorageType[[i]] <- vapply(DF_DATA[[variable.name]], typeof, character(1))[[1]]
        flattenData$ReadFormat[[i]] <- paste( "%", length, "." , dcml, "g", sep="")
      }

    }

    # write fixed width format file
    columnNames <- as.character(unlist(flattenData$internalName))
    columnWidths <- as.numeric(unlist(flattenData$width))
    columnRound <- c(rep(0,length(flattenData$name)))

    # select columns from data frame to make sure the column order and to remove deleted variables
    DF_DATA <- data.frame(DF_DATA[, columnNames])

    ## write fixed width file with options
    # width     - column width
    # colnames  - exclude header row
    # sep       - delemeter
    .write_fwf(DF_DATA,locale=lcl, encoding = "UTF-8", file = outpath, width = columnWidths, na="*", colnames = FALSE, sep = "")

    return(list(result='ok', data=flattenData ))
  }, warning = function(war) {
    return(list(result='warning', message= paste('warning >> ',war)))
  }, error = function(err) {
    return(list(result='error', message= paste('error >> ',err)))
  })

}
