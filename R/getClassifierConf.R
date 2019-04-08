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
#' @importFrom rjson fromJSON
#'
#' @noRd
#'
#' @keywords internal

getClassifierConf <- function(classifierName) {
  #Open the Classifier Parameters Configuration File
  classifierConfDir <- system.file("extdata", paste(classifierName,'.json',sep=""), package = "SmartML", mustWork = TRUE)
  result <- fromJSON(file = classifierConfDir)
  return(result)
}
