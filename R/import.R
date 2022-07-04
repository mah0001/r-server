#' @title Write data file(CSV)
#'
#' @description This package import dataset and write data in csv format
#'
#' @param filepath Path to data file
#' @param type Type of data file (dta | sav | csv)
#' @param csvFile Output file path
#'
#' @return A list contains the status
#'
#' @examples  writeCSV('D:\\MetadataEditorSource\\MetadataEditor_Dev\\MetadataEditor\\Sample Data\\in\\HND_2012_L2L.dta', 'dta','D:\\MetadataEditorSource\\MetadataEditor_Dev\\MetadataEditor\\Sample Data\\in\\HND_2012_L2L.csv')
#'
#' @export writeCSV

writeCSV <- function(filepath="", type="", csvPath="") {
  tryCatch({

    #csvFile <- paste0(get_storage_root(), csvPath)
    #filepath <- paste0(get_storage_root(),filepath)
    csvFile <-  csvPath

    # create directory if not exists
    dir.create(dirname(csvFile), recursive = TRUE, showWarnings = FALSE)

    # read dataset files
    if (toupper(type) == 'DTA') {
      DF_DATA <- read_dta(filepath)
    } else if (toupper(type) == 'SAV') {
      DF_DATA <- read_spss(filepath)
    } else if (toupper(type) == 'SAS7BDAT') {
      DF_DATA <- read_sas(filepath)
    }  else if (toupper(type) == 'CSV') {
      #. to resolve the unicode issues, used readr::read_csv instead of read.csv method
      # DF_DATA <- read.csv(filepath, stringsAsFactors = TRUE)
      DF_DATA <- suppressWarnings(suppressMessages(read_csv(filepath)))
      DF_DATA[ is.na(DF_DATA) ] <- NA

      # get col names as variables
      variables <- colnames(DF_DATA)

      # set factor levels instead of label
      for(j in 1:length(variables)){

        varName <- variables[j]

        # If factor, set levels as labels, otherwise labels will be exported to CSV
        # Convert type as numeric
        if(is.factor(DF_DATA[[varName]])){
          labels <- as.factor(levels(DF_DATA[[varName]]))
          levels <- as.factor(labels(labels))
          DF_DATA[[varName]] <- factor(DF_DATA[[varName]], labels = levels)
          DF_DATA[[varName]] <- as.numeric( DF_DATA[[varName]] )
        }
      }
    }

    # Replace empty string with NA
    DF_DATA[sapply(DF_DATA, is.character)] <- lapply(DF_DATA[sapply(DF_DATA, is.character)],
                                                     function(x) zap_empty(x))

    DF_DATA[ is.na(DF_DATA) ] <- NA    #missing values replaced with NA


    # To resolve the unicode issues, used readr package instead of haven to write the csv file.
    write_csv(DF_DATA, csvFile, na = "*", append = FALSE)
    # write.csv(DF_DATA, file=file(csvFile, encoding="UTF-8"), row.names = FALSE, quote = TRUE, na= "*")


    return(list(result='ok', file=csvFile ))
  }, warning = function(war) {
    return(list(result='warning', message= paste('warning >> ',war)))
  }, error = function(err) {
    return(list(result='error', message= paste('error >> ',err)))
  })
}

get_r_version <-function(){
  return (R.version.string)
}

get_storage_root <-function(){

  #Sys.setenv("NADA_ROOT"="/Users/m2/Downloads/RDATA_OPENCPU")

  if (Sys.getenv("NADA_STORAGE_ROOT")==""){
    stop("NADA_STORAGE_ROOT is not set");
  }

  return (Sys.getenv("NADA_STORAGE_ROOT"));
}

upload <- function(fileUpload="",idno="") {
  storage_root=get_storage_root();
  if (!dir.exists(storage_root)) {dir.create(storage_root)}
  file.rename(fileUpload,paste0(storage_root,fileUpload))
  setwd(storage_root)
  print(getwd());
  print(ls())
}


#' @title Import dataset
#'
#' @description This package import datasets of different datatypes (STATA | SPSS | CSV)
#'
#' @param filepath Path to data file
#' @param type Type of data file (dta | sav | csv)
#' @param fileId unique file id
#' @param freqLimit frequency limit
#'
#' @return A list with variable metadata
#'
#' @examples  import('D:\\MetadataEditorSource\\MetadataEditor_Dev\\MetadataEditor\\Sample Data\\in\\HND_2012_L2L.dta', 'dta', 'F1', 36000)
#'
#' @export import

import <- function(filepath="", type="", fileId="", freqLimit="") {
  tryCatch({

    #filepath=paste0(get_storage_root(),filepath)

    if (toupper(type) == 'DTA') {
      DF_DATA <- read_dta(filepath)
    } else if (toupper(type) == 'SAV') {
      DF_DATA <- read_spss(filepath)
    } else if (toupper(type) == 'CSV') {
      #. to resolve the unicode issues, used readr::read_csv instead of read.csv method
      DF_DATA <- suppressWarnings(suppressMessages(read_csv(filepath)))
    }

    rowCount <- nrow(DF_DATA)  # get row count
    DF_DATA[ is.na(DF_DATA) ] <- NA    #missing values replaced with NA

    #Get the total number of variables
    variables <- colnames(DF_DATA)

    #Apply transformation on each variable
    varList <- lapply(variables,function(varName){

      attrVector <- c(names(attributes(DF_DATA[[varName]])))
      #If the variable does not have variable label it automatically takes the value labels which we don't want.
      #e.g hhid was appearing as Name if there was no label such as "HouseHold Id"
      if (is.element('label', attrVector)) {
        label <- attr(DF_DATA[[varName]],'label')
      } else {
        label <- ''
      }

      # If CSV, set variable name as label
      if (toupper(type) == 'CSV') {
        label <- varName
      }
      regexp <- "[[:digit:]]+"

      file.index = as.character(fileId)
      units <- "REAL" #TODO:
      varFormatSchema <- "other" #TODO: Where is it used??

      dcml <- 0
      location.width <- 0
      intrvl <- "contin"

      #Start: For numeric variables (For STATA, SPSS and in future other types that can set attributes in DF)
      if (sapply(DF_DATA[varName], is.numeric)) {

        location.width = 0
        #intrvl <- "contin" #Take sample and see if x has n% threshold and decide???

        if (!is.null(attr(DF_DATA[[varName]],"format.spss"))) {
          formatValue <- attr(DF_DATA[[varName]],"format.spss")
          formatValueSplit <- strsplit(formatValue, "[.]")
          if (lengths(formatValueSplit) == 2) {
            location.width <- str_extract(formatValueSplit[[1]][1:1], regexp)
            dcml <- str_extract(formatValueSplit[[1]][2:2], regexp)
          }
        }

        if (!is.null(attr(DF_DATA[[varName]],"format.stata"))) {
          formatValue <- attr(DF_DATA[[varName]],"format.stata")
          formatValueSplit <- strsplit(formatValue, "[.]")
          # Numerical: byte %8.0g, int %8.0g, long %12.0g, float %9.0g, double %10.0g,
          # Fixed Length Numerical: %9.2f
          # String: str# E.g; str14 means datatype
          # String Len: %#s E.g: %18s length 18
          # Date: %ty year

          if (lengths(formatValueSplit) == 2) {
            location.width <- str_extract(formatValueSplit[[1]][1:1], regexp)
            dcml <- str_extract(formatValueSplit[[1]][2:2], regexp)
          }
        }

      }
      #End: For numeric variables

      #Start: For character variables (For STATA, SPSS and in future other types that can set attributes in DF)
      if (sapply(DF_DATA[varName], is.character)) {

        location.width = 0

        if (!is.null(attr(DF_DATA[[varName]],"format.spss"))) {
          formatValue <- attr(DF_DATA[[varName]],"format.spss")
          formatValueSplit <- strsplit(formatValue, "[.]")
          if (lengths(formatValueSplit) == 1) {
            #For string width should be maximum it can hold
            #StringLen: Should be maximum length of character in data
            location.width <- str_extract(formatValueSplit[[1]], regexp)
            units <- "character"
          }
        }

        if (!is.null(attr(DF_DATA[[varName]],"format.stata"))) {
          formatValue <- attr(DF_DATA[[varName]],"format.stata")
          formatValueSplit <- strsplit(formatValue, "[.]")
          if (lengths(formatValueSplit) == 1) {
            #For string width should be maximum it can hold
            #StringLen: Should be maximum length of character in data
            location.width <- str_extract(formatValueSplit[[1]], regexp)
            units <- "character"
          }
        }

      }

      #. if type is date, get the length of the variable
      #. TO DO: mismatch with Nesstar, correct the logic
      #if(class(DF_DATA[varName][[1]]) == 'Date'){
      #  location.width <- format.info(format(DF_DATA[[varName]]))
      #}

      catList <- NA


      #If the variable is labelled and it has labels or if it is nominal/ordinal
      if (is.labelled(DF_DATA[[varName]]) && !is.null(attr(DF_DATA[[varName]],'labels'))) {
        intrvl <- "discrete"
        labels <- attr(DF_DATA[[varName]],"labels") #Get the labels for the variable

        #This can copy value as label if the incoming data file has duplicate category names.
        #e.g;HND_2012_L2L_UTF8_old.dta variable:H60
        #4 "Member of agricultural cooperative, group or settlement"
        #8 "Member of agricultural cooperative, group or settlement" when converted to DF
        #ALl other value labels are lost. The label is set as value
        #Right now nt doing any special case handling, assuming that it will be fixed at source data file.
        labels_df <- as.data.frame(labels) #convert variables to DF

        colnames(labels_df) <- c(varName) #Make the column same as variable name so that we can merge
        labels_df["labl"] <- rownames(labels_df) #Add column for labels
        uniqlabel <- nrow(labels_df)

        #Calculate freq. i.e Table to identify number of occurances for a value
        freqTable <- count(DF_DATA[varName])
        uniqfreq <- nrow(freqTable)

        # count is returning unicode, set column name as variable name to merge
        colnames(freqTable) <- c(varName, "freq")

        # Get the sysmiss row and append it back after the merge
        # To resolve the issue sysmiss row is missing for the categorical variable
        sysmiss <- subset(freqTable, is.na(freqTable[[varName]]))
        freqTable <- subset(freqTable, !is.na(freqTable[[varName]]))

        #merge total cat and labels. There might be categories which don't have label.
        #Their "labl" column will be NA
        #   sex  freq  labl
        # 1 1    1670   Male
        # 2 2    1819   Female
       if (uniqfreq<=100){
          #generate for all values (labelled or not)
          catMerge <- merge(freqTable,labels_df,by=varName, all = TRUE)
        } else if (uniqfreq >100 & uniqfreq < 5000){
          #minimum threshold  (20%)
          if ((uniqfreq * .20) <= uniqlabel) {
            #generate for all values (labelled or not)
            catMerge <- merge(freqTable,labels_df,by=varName, all = TRUE)
          } else {
            #only generate frequencies for labeled values
            catMerge <- merge(freqTable,labels_df,by=varName, all.y = TRUE)
          }
        } else{
          #only generate frequencies for labeled values
          catMerge <- merge(freqTable,labels_df,by=varName, all.y = TRUE)
        }

        # append sysmis row
        if(dim(sysmiss) && dim(sysmiss)[1] != 0) {
          labl <- ''
          catMerge <- rbind(catMerge, cbind(sysmiss, labl))
        }

        #Set frequency as 0 if NA
        catMerge$freq[ is.na(catMerge$freq) ] <- 0


        catList <- lapply(rownames(catMerge), function(rowName){
          catValue <- catMerge[rowName,varName]
          catLabl <- catMerge[rowName,"labl"]
          freq <- catMerge[rowName,"freq"]
          list(value=catValue,labl=catLabl,stats=list(type="freq",value=freq))
        })

      } else if (is.factor(DF_DATA[[varName]]) && toupper(type) == 'CSV') {

        intrvl <- "discrete"
        labels <- levels(DF_DATA[[varName]]) #Get the levels of the factor for the variable

        labels_df <- as.data.frame(labels) #convert variables to DF
        colnames(labels_df) <- c(varName) #Make the column same as variable name so that we can merge
        labels_df["labl"] <- rownames(labels_df) #Add column for labels

        #Set width based on the type
        location.width <- switch(typeof(DF_DATA[[varName]]), "integer" = 8, "double" = 10, 16)

        #Calculate freq
        freqTable <- count(DF_DATA[varName])
        colnames(freqTable) <- c(varName, "freq")

        # Get the sysmiss row and append it back after the merge
        # To resolve the issue sysmiss row is missing for the categorical variable
        sysmiss <- subset(freqTable, is.na(freqTable[[varName]]))
        freqTable <- subset(freqTable, !is.na(freqTable[[varName]]))

        #merge total cat and labels. There might be categories which don't have label. Their "labl" column will be NA
        catMerge <- merge(freqTable,labels_df,by=varName, all.y = TRUE)

         # append sysmis row
        if(dim(sysmiss) && dim(sysmiss)[1] != 0) {
          labl <- ''
          catMerge <- rbind(catMerge, cbind(sysmiss, labl))
        }

        catList <- lapply(rownames(catMerge), function(rowName){
          catValue <- catMerge[rowName,"labl"]
          catLabl <- catMerge[rowName,varName]
          freq <- catMerge[rowName,"freq"]
          list(value=catValue,labl=catLabl,stats=list(type="freq",value=freq))
        })

      }  else {
        # DO THIS: If we are not able to decide if the variable is "Discrete" or "Continuous" based on the
        # variable attributes (isLabelled or factor). Most of the STATA/SPSS should be covered under
        # is.labelled check.
        # TODO: This logic has to be discussed and refined

        #1. Check the number of distinct values
        #2. COmpare with a configurable parameter (DEFAULT 50).
              # IF less then set it as DISCRETE and set Label to empty ""
              # Otherwise set it as CONTINUOUS

        uniqueValues <- unique(DF_DATA[[varName]])
        lenUniqueValues <- length(uniqueValues)

        #freqLimit: Number of distinct values
        if (lenUniqueValues < freqLimit) {
          intrvl <- "discrete"
          #Calculate freq
          freqTable <- count(DF_DATA[varName])
          colnames(freqTable) <- c(varName, "freq")

          catList <- lapply(rownames(freqTable), function(rowName){
          catValue <- freqTable[rowName,varName]
          catLabl <- c("")
          freq <- freqTable[rowName,"freq"]
          list(value=catValue,labl=catLabl,stats=list(type="freq",value=freq))
        })

        }
      }

      #This will give each variable a unique id "V1","V2"....
      var_seq <- as.data.frame(paste(replicate(length(colnames(DF_DATA)),"V"),
                                     as.character(c(1:length(colnames(DF_DATA)))),sep=""))
      names(var_seq) <- list("var_seq") #Modify the column name
      row.names(var_seq) <- colnames(DF_DATA) #Give the rownames same as the variable name
      ID <- as.character(var_seq[varName,"var_seq"]) #Get the sequence ID for the variable

      sumstats <- .summary_stats(DF_DATA,varName)

      #todo - need to test with a data field type
      #variableType <- ifelse(class(DF_DATA[varName][[1]]) == 'Date', 'date', mode(DF_DATA[varName][[1]]))
      variableType <- mode(DF_DATA[varName][[1]])

      #this does not work
      #if(class(DF_DATA[varName][[1]]) == 'Date'){
      #  variableType<-"date"
      #}

      list(
        name = varName,
        file_id=file.index,
        vid=ID,
        var_dcml=dcml,
        var_intrvl=intrvl,
        loc_width=location.width,
        labl=label,
        #measure: If "Discrete" then set it to Nomial. How to figure out ordinal values?
        #stringLen: application will set the character.width as StringLen initially. Resequencing can change it
        #Width vs StringLen:For string width should be maximum it can hold, StringLen: Should be maximum length of character in data
        #missing
        #isTimeVariable
        #dataType: UNITS (REAL for numeric, CHARACTER for string/char)
        #startPos,EndPos
        #ImplictDecimal: 201 with implicit decimal 2 actually means 2.01
        var_valrng=list(range=list(UNITS=units,min=sumstats$min,max=sumstats$max,mean=sumstats$mean,stdev=sumstats$stdev)),
        var_sumstat=list(list(type="vald",value=sumstats$vald),list(type="invd",value=sumstats$invd)),
        var_catgry=catList,
        var_format=list(type=variableType,schema=varFormatSchema),
        #var_type=variableType
      )

    })

    return(list(result='ok', cnt=rowCount, variables=varList))
  }, warning = function(war) {
    warning(war)
    return(list(result='warning', message= paste('warning >> ',war)))
  }, error = function(err) {
    return(list(result='error', message= paste('error >> ',err)))
  })
}



# tryCatch({
# varxList <- lapply(variables,function(varName){
#   #print (DF_DATA[varName][[1]] =='Date' )
#   print(varName)
#   print (class(DF_DATA[varName][[1]]))
#   #if (class(DF_DATA[varName][[1]]) == 'Date'){
#   #print (class(DF_DATA[varName][[1]]))
#   #}
# });
# },
# warning = function(w){
#   # (Optional)
#   # Do this if an warning is caught...
#   print(w)
# },
# finally = {
#   # (Optional)
#   # Do this at the end before quitting the tryCatch structure...
# }
# )
