#' @title Send Meta-Features to the Database
#' @description Save Meta Features with results obtained in the database
#' @seealso \code{\link[utils]{head}}
#' @examples \dontrun{ sendToDatabase()
#' }
sendToDatabase <- function(){
  library(devtools)
  #install_github("gregce/ipify")
  library(ipify)
  library("aws.s3")
  library("datasets")

  
  #sessionInfo()
  get_bucket(bucket = 'rautoml')
  # write file to S3
  put_object("tmp", object = paste(get_ip(),".csv", sep=""), bucket = "rautoml")
}
