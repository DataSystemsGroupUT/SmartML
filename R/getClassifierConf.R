#' @title Read Classifier Parameters
#' @description Read Classifier Algorithm Parameters from Json file
#' @param classifierName classifier Algorithm Name
#' @keywords AutoML, SMAC
#' @seealso \code{\link[utils]{head}}
#' @return Data Frame of Classifier Parameters
#' @examples \dontrun{ getClassifierConf(classifierAlgorithm)
#' }
getClassifierConf <- function(classifierName) {
  library("rjson")
  #Open the Classifier Parameters Configuration File
  classifierConfDir <- paste('./classifiersData/', classifierName,'.json',sep="")
  result <- fromJSON(file = classifierConfDir)

  return(result)
}
