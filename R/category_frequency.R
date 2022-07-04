#' @title get category frequency
#'
#' @description calculate frequencies of variable categories when apply categories from variable repo
#'
#' @param varRepoInput variable repository details
#' @param csvpath input csv file path

#'
#' @return A list of updated variable statistics
#'
#' @examples  categoryFrequency({"variables":["REGION"],"catgry":[{"catStat":{"text":""},"catValu":1,"labelled":true,"labl":"One"},{"catStat":{"text":""},"catValu":2,"labelled":true,"labl":"Two"},{"catStat":{"text":""},"catValu":3,"labelled":true,"labl":"Three"}]},'D:\\mde\\f1.csv')
#'
#' @export categoryFrequency

categoryFrequency <- function(varRepoInput, csvpath) {

  tryCatch({

    listOfVariables <- varRepoInput$variables
    catList <- varRepoInput$catgry
    catgryDF <- as.data.frame(catList)

    # read matching variables from CSV
    matchingDF <- .read_matchingVariables(listOfVariables, csvpath)

    repoStats <- lapply(listOfVariables, function(varName){
      catgry <- .calculate_catFrequency(matchingDF, catgryDF, varName)
      list(name=varName, catgry=catgry)
    })

    return (list(result='ok', data=repoStats))
  }, warning = function(war) {
    return(list(result='warning', message= paste('warning >> ',war)))
  }, error = function(err) {
    return(list(result='error', message= paste('error >> ',err)))
  })
}
