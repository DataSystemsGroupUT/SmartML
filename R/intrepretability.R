library(ggplot2)
library("iml")

interpret <- function(model, x){
  clas = as.factor(x$class)
  X = x[which(names(x) != "class")]
  X[] <- lapply(X, function(x) {
    as.double(as.character(x))
  })
  predictor = Predictor$new(model, data = X, y = as.factor(clas))
  out <- list()
  out$featImp <- FeatureImp$new(predictor, loss = "mae")
  out$interact = Interaction$new(predictor)
  #print(plot(out$interact))
  return(out)
}
