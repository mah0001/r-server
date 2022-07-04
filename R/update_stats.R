#' @title update variable statistics
#'
#' @description Update variable statistics when perform different actions
#'
#' @param typeOfActions different type of actions
#' @param csvpath input csv file path

#'
#' @return A list of updated variable statistics
#'
#' @examples  updateStats([{"action":"encodeMissing","name":"REGION","properties":[{"op":"=","val":"1"}]}],'D:\\mde\\F1.csv')
#'
#' @export updateStats

updateStats <- function(typeOfActions, csvpath) {

  tryCatch({

    listOfVariables <- typeOfActions[,'name']

    # read matching variables from CSV
    matchingDF <- .read_matchingVariables(listOfVariables, csvpath)

    #CALCULATE stats FOR variables that were DESTRINGED
    destringInputActionsPredicate <- typeOfActions$action=='destring'
    destringInputActions <- typeOfActions[destringInputActionsPredicate,c('name','properties')]
    destringedVarStatsList <- NA
    if (length(destringInputActions) >= 1) {
      destringedVarStatsList <- lapply(destringInputActions$name,function(varName){
        if (sapply(matchingDF[varName], is.numeric)) {
          .varStats(matchingDF,varName)
        }
      })
    }

    recalcStatsInputActionsPredicate <- typeOfActions$action=='recalcStats'
    recalcStatsInputActions <- typeOfActions[recalcStatsInputActionsPredicate,c('name','properties')]
    recalcStatsVarStatsList <- NA
    if (length(recalcStatsInputActions) >= 1) {
      recalcStatsVarStatsList <- lapply(recalcStatsInputActions$name,function(varName){

        freqTable <- count(matchingDF[[varName]])
        colnames(freqTable) <- c("Value","freq")
        catList <- recalcStatsInputActions[recalcStatsInputActions$name == varName, 'properties']

        if(length(catList) >= 1){
          catgryDF <- as.data.frame(catList)
          catgry <- lapply(catgryDF$catValu, function(val){
            if(is.na(val)){
              catg <- catgryDF[is.na(catgryDF$catValu),]
              freq <-  freqTable[is.na(freqTable$Value), 'freq']
            }
            else{
              catg <- na.omit(catgryDF[catgryDF$catValu==val,])
              freq <- na.omit(freqTable[freqTable$Value==val, 'freq'])
            }
            if(length(freq) == 0 || is.na(freq)){
              freq <- 0
            }
            list(catValu=val,labl=catg$labl,labelled=TRUE,catStat=list(type="freq",text=freq))
          })
          append(.varStats(matchingDF,varName), list(catgry=catgry))
        }
      })
    }

    #CALCULATE stats FOR variables that were ENCODED WITH MISSING VALUES
    encMissInputActionsPredicate <- typeOfActions$action=='encodeMissing'
    encMissInputActions <- typeOfActions[encMissInputActionsPredicate,c('name','properties')]
    encMissVarStatsList <- lapply(encMissInputActions$name,function(varName){

      # Replace the DF data with NA for user specified missing values
      missingData <- subset(encMissInputActions, name==varName)[,2][[1]]
      # missingData ->
      #    op  val val2
      # 1  =   999
      # 2  >=   40
      # 3  <=   57
      # 4  ..   60  100

      # apply missing conditions if any then calaculate statistics
      if(length(missingData) >0) {

        #. apply equal to operator
        equalOpPredicate <- missingData$op == '='
        equalOpData <- missingData[equalOpPredicate,]

        # if the type is character, then apply the missing equal to condition
        replaceValPredicate <- is.na(matchingDF[,varName])
        if(!sapply(matchingDF[varName], is.numeric)){
          for(val in equalOpData$val){
            replaceValPredicate <- replaceValPredicate | matchingDF[,varName] == val
          }
        } else {
          for(val in equalOpData$val){
            # type is numeric then parse the values to numeric and apply condition
            replaceValPredicate <- replaceValPredicate | matchingDF[,varName] == as.numeric(val)
          }

          #. apply range operator
          rangeOpPredicate <- missingData$op == '..'
          rangeOpData <- missingData[rangeOpPredicate,]
          if(length(rangeOpData$op) == 1){
            replaceValPredicate <- replaceValPredicate | (matchingDF[,varName] >= as.numeric(rangeOpData$val) & matchingDF[,varName] <= as.numeric(rangeOpData$val2))
          }

          #. apply greater than operator
          gteOpPredicate <- missingData$op == '>='
          gteOpData <- missingData[gteOpPredicate,]
          if (length(gteOpData$op) >= 1) {
            replaceValPredicate <- replaceValPredicate | (matchingDF[,varName] >= as.numeric(gteOpData$val))
          }

          #. apply less than operator
          lteOpPredicate <- missingData$op == '<='
          lteOpData <- missingData[lteOpPredicate,]
          if (length(lteOpData$op) >= 1) {
            replaceValPredicate <- replaceValPredicate | (matchingDF[,varName] <= as.numeric(lteOpData$val))
          }
        }
        # to remove NA from replaceValPredicate, to resolve the error of variables with NA(missing) values
        replaceValPredicate <- replaceValPredicate | is.na(replaceValPredicate)
        matchingDF[replaceValPredicate,varName]<-NA
      }
      #. calculate variable statistics
      .varStats(matchingDF,varName)
    })

    statsList <- list(destringedVarStatsList,encMissVarStatsList, recalcStatsVarStatsList)

    return (list(result='ok', data=statsList))
  }, warning = function(war) {
    return(list(result='warning', message= paste('warning >> ',war)))
  }, error = function(err) {
    return(list(result='error', message= paste('error >> ',err)))
  })
}
