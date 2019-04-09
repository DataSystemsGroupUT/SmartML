#' @title Perform Interpretability on Model.
#'
#' @description Perform Model interpretability on the select model by obtaining two plots for feature importance and feature interaction.
#'
#' @param model Fitted Model of any of the chosen classifiers and fitted on the training set.
#' @param x Dataframe of the training set.
#'
#' @return List of two plots of feature importance and feature interaction.
#'
#' @examples interpret(\code{model}, data.frame(salary = c(623, 515, 611, 729, 843), class = c (0, 0, 0, 1, 1)))
#'
#' @importFrom iml FeatureImp Interaction Predictor
#'
#' @noRd
#'
#' @keywords internal

Loss <- function(actual, predicted){
  err <- 0
  for(i in 1:length(actual)){
    act <- as.character(actual[i])
    pred <- substring(as.character(predicted[i]), 2)
    if (act != pred)
      err <- err + 1
  }
  return(err/length(actual))
}

interpret <- function(model, x){
  clas = as.factor(x$class)
  X = x[which(names(x) != "class")]
  X[] <- lapply(X, function(x) {
    as.double(as.character(x))
  })
  predictor = Predictor$new(model, data = X, y = as.factor(clas))
  out <- list()
  out$featImp <- FeatureImp$new(predictor, loss = Loss)
  out$interact = Interaction$new(predictor)
  return(out)
}
