#' @title Get Classifier Parameters Configuration.
#'
#' @description Read the Json file of the configuration of the parameters of a specific classifier.
#'
#' @param classifierName String value of the classifier Name.
#'
#' @return List of the parameters of the classifier.
#'
#' @examples getClassifierConf('knn').
#'
#' @noRd
#'
#' @keywords internal

getClassifierConf <- function(classifierName) {
  library("rjson")
  #Open the Classifier Parameters Configuration File
  classifierConfDir <- paste('./classifiersData/', classifierName,'.json',sep="")
  result <- fromJSON(file = classifierConfDir)

  return(result)
}
