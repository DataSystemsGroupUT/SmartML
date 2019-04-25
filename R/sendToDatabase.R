#' @title Send Results to Knowledge Base
#'
#' @description Connect to the cloud knowledge base to store the results obtained to be used in meta-learning of future runs.
#'
#' @return None
#'
#' @examples sendToDatabase()
#'
#' @noRd
#'
#' @import devtools
#' @importFrom rjson fromJSON
#' @importFrom httr POST
#'
#' @keywords internal

sendToDatabase <- function(){
  #Get IP
  cntIP <- fromJSON(readLines("http://api.hostip.info/get_json.php", warn=F))$ip

  #Update knowledge base
  updateKB <- try(
    {
      tmp <- paste(readLines(system.file("extdata", "tmp", package = "SmartML", mustWork = TRUE)), collapse="\n")
      res <- POST("https://jncvt2k156.execute-api.eu-west-1.amazonaws.com/default/s3-trigger-rautoml", body = list(data = paste(tmp, "&DATA&", sep=""),
                                                                                                                   fName = paste(cntIP,".csv&FILENAME&", sep=""),
                                                                                                                   encode = "json"))
      write("", file=system.file("extdata", "tmp", package = "SmartML", mustWork = TRUE),append=TRUE) #Empty the tmp file
    })
  if(inherits(updateKB, "try-error"))
    print('Failed to update Knowledge base.')

}
