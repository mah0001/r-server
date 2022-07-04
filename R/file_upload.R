#' @title file uploads
#'
#' @description upload a file
#'
#' @param file file to upload
#'
#' @return file upload
#'
#'
#' @export file_upload
file_upload <- function(file){
  ##if(!grepl(".csv$", file)){
  ##  stop("Uploaded file must be a .csv file!")
  ##}
  list(
    message = paste("hello", file, "! This is", R.Version()$version.string)
  )

  read.csv(file);
}


#' @title hello
#'
#' @description hello
#'
#'
#' @return hello
#'
#'
#' @export hello
hello <- function(hello_text="hi"){
  ##if(!grepl(".csv$", file)){
  ##  stop("Uploaded file must be a .csv file!")
  ##}
  print(hello_text);

  list(
    message = paste("hello", hello_text, "! This is", R.Version()$version.string)
  )

  #read.csv(file, ...);
}


