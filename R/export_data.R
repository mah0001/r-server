#' @title export dataset to STATA/SPSS/CSV format
#'
#' @description export dataset to STATA/SPSS/CSV format
#'
#' @param varDescr variable description
#' @param datafile input data(csv) file path
#' @param type Type of data file (dta | sav | csv)
#' @param versionNo STATA version
#' @param outputfile Output file path
#'
#' @return A list contains the status
#'
#' @examples  export([{"name":["hhid"],"internalName":["hhid"],"labl":["Household identification"],"val":[],"dcml":["0"],"width":["16"],"type":["numeric"],"intrvl":["contin"],"dataType":["double"]}],'D:\\mde\\f1.csv', 'csv','D:\\out\\HND_2012_L2L.csv')
#'
#' @export export

export <- function( varDescr, datafile, type, versionNo, outputfile ) {

  tryCatch({

    # read variable details from the json file
    jsonData <- varDescr
    # flatten the jsonData (valRange and valFormat objects are the only ones flattend)
    flattenData <- flatten(jsonData, recursive = TRUE)

    # read data file
    DF_DATA <- .read_data(flattenData, file=datafile)

    # loop through the list of variables and assign label & value label to the dataframe
    for(j in 1:nrow(flattenData))
    {
      # variable name
      variable.name <- flattenData$name[[j]]

      # if variable name is changed, then update the column name
      if(variable.name != flattenData$internalName[[j]]){
        column.index <- which( colnames(DF_DATA)==flattenData$internalName[[j]])
        colnames(DF_DATA)[column.index] <- variable.name
      }

      # variable type
      variable.type <- flattenData$type[[j]]

      # If new variable add new variable to data as NA
      if(is.null(DF_DATA[[variable.name]])){
        DF_DATA[[variable.name]] <- NA
      }
      # for csv format
      if (toupper(type) == 'CSV') {
        # assign value label
        cat_values <- flattenData$val[[j]]
        if(!is.na(cat_values) && length(cat_values) > 0){
          #. modified the logic to handle the issue, showing blank when exporting to the non-categorical values to CSV
          labels_df <-as.data.frame(levels(factor(DF_DATA[[variable.name]])))
          colnames(labels_df) <- c('cat_val')
          labels_df["labl"] <- labels_df['cat_val']
          category_df <- merge(cat_values, labels_df, by.x='catValu', by.y ='cat_val', sort = FALSE, all=TRUE)

          catVal <- apply(category_df, 1, function(x) x[['catValu']])
          catLabl <- apply(category_df, 1, function(x){
            ifelse( (is.null(x[["labl.x"]]) || is.na(x[["labl.x"]]) || x[["labl.x"]] == "" ), x[["labl.y"]], x[["labl.x"]])
          })
          DF_DATA[[variable.name]] <- factor(DF_DATA[[variable.name]], labels = catLabl, levels =  catVal)

        }
      } else {

        # assign value label
        cat_values <- flattenData$val[[j]]
        if(!is.na(cat_values) && length(cat_values) > 0){

          # category label
          cat_labl <- cat_values[["labl"]]

          # category value
          cat_val <- cat_values[["catValu"]]

          # variable levels and labels
          for(i in 1:length(cat_val)){
            # set value label if label is not empty
            if(!is.null(cat_labl[[i]]) && !is.na(cat_labl[[i]]) && cat_labl[[i]] != ""){
              # trim label string, since dta doesn't support label of length greater than 30
              str_label <- cat_labl[[i]]
              #if(toupper(type) == 'DTA'){
              #if(nchar(str_label) > 30) {
              #str_label <- strtrim(str_label,30)
              #}
              #}
              # Convert category value to the variable type
              # to resolve `x` and `labels` must be same type
              value <- cat_val[[i]]
              if(!is.na(variable.type)){
                class(value) <- variable.type
              }
              # set value label using labelled packge. This is closer to orginal .DTA
              val_label(DF_DATA[[variable.name]], value) <- str_label
            }
          }
        }

        # set format for numeric type
        if (variable.type == "numeric"){

          # variable width
          variable.width <- 16

          if(!is.na(flattenData$width[[j]])){
            variable.width <- flattenData$width[[j]]
          }

          # variable decimal
          variable.dcml <- 0
          if(!is.na(flattenData$dcml[[j]])){
            variable.dcml <- flattenData$dcml[[j]]
          }

          # set format
          # (Link to stata format: http://www.stata.com/manuals13/dformat.pdf )
          # (Link to spss format: https://www.spss-tutorials.com/spss-variable-types-and-formats/)
          if(toupper(type) == 'DTA'){
            # variable format
            variable.format <- paste("%", variable.width, ".", variable.dcml,  "g", sep = "")
            attr(DF_DATA[[variable.name]],"format.stata") <- variable.format
          } else if(toupper(type) == 'SAV'){
            # variable format
            variable.format <- paste("F", variable.width, ".", variable.dcml, sep = "")
            attr(DF_DATA[[variable.name]],"format.spss") <- variable.format
          }

        } else if(variable.type == "character"){
          # variable width
          variable.width <- 9

          if(!is.na(flattenData$width[[j]])){
            variable.width <- flattenData$width[[j]]
          }

          # encode to UTF-8 to avoid unicode issues
          # Ref: https://stackoverflow.com/questions/23699271/force-character-vector-encoding-from-unknown-to-utf-8-in-r
          if(all(stri_enc_isutf8(DF_DATA[[variable.name]]), na.rm = TRUE)){
            DF_DATA[[variable.name]] <- stri_encode(DF_DATA[[variable.name]], "", "UTF-8")
          }

          #DF_DATA[[variable.name]] <- iconv(DF_DATA[[variable.name]],  "latin1", "UTF-8")
          if(toupper(type) == 'DTA'){
            # variable format
            variable.format <- paste("%", variable.width, "s", sep = "")
            attr(DF_DATA[[variable.name]],"format.stata") <- variable.format
          } else if(toupper(type) == 'SAV'){
            # variable format
            variable.format <- paste("A", variable.width, sep = "")
            attr(DF_DATA[[variable.name]],"format.stata") <- variable.format
          }

        } else if(variable.type == 'date') {
          #. TO DO: Correct the logic for date
          if(toupper(type) == 'DTA'){
            attr(DF_DATA[[variable.name]],"format.stata") <- "%d"
          } else if(toupper(type) == 'SAV'){
            attr(DF_DATA[[variable.name]],"format.spss") <- "EDATE11"
          }
        }

        # variable label (Must be set after Value Label is set)
        variable.label <- flattenData$labl[[j]]

        # unlist label if variable label is list
        if(typeof(variable.label) == "list" ){
          variable.label <- unlist(variable.label)
        }
        # assign var label
        if(!is.null(variable.label) && !is.na(variable.label) && variable.label != ""){
          if (is.numeric(variable.label)) {
            cVariable.Label <- as.character(variable.label)
          }
          else {
            cVariable.Label <- variable.label
          }

          if (length(cVariable.Label) > 1) {
            cVariable.Label <- paste(cVariable.Label,sep=",",collapse="")
          }
          var_label(DF_DATA[[variable.name]]) <- cVariable.Label
        }

      }

    }

    # re sequence variables based on the order
    columnNames <- as.character(unlist(flattenData$name))

    # added condition to resolve the error when exports datasets with single variable.
    if (typeof(DF_DATA) != "list") {
      DF_DATA <- DF_DATA[, columnNames]
    }
    #View(DF_DATA)

    if (toupper(type) == 'DTA') {
      # Write STATA- .dta file
      write_dta(DF_DATA, outputfile, version= versionNo)
    } else if (toupper(type) == 'SAV') {
      # Write SPSS- .sav file
      write_sav(DF_DATA, outputfile)
    } else if (toupper(type) == 'CSV'){
      # Write CSV file.
      write_csv(DF_DATA, outputfile, na = "", append = FALSE)
    }
    return(list(result='ok', file=outputfile ))
  }, warning = function(war) {
    return(list(result='warning', message= paste('warning >> ',war)))
  }, error = function(err) {
    return(list(result='error', message= paste('error >> ',err)))
  })

}
