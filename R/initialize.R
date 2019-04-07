#' @title Initialize the SMAC model.
#'
#' @description Initialize the SMAC model with the classifier default parameter configuration.
#'
#' @param classifierName String of the classifier algorithm name.
#' @param result List of the converted classifier json parameter configuration into set of vectors and lists.
#' @param initParams String of the initial parameter configuration of \code{classifierName} to start the model with.
#'
#' @return
#'
#' @examples
#'
#' @noRd
#'
#' @keywords internal

initialize <- function(classifierName, result, initParams) {
  #get list of Classifier Parameters
  params <- result$params
  #get list of GrandParent parametes
  gparams <- result$parents
  #Create dataFrame for classifier default parameters
  defaultParams <- data.frame(matrix(ncol = length(params)+1, nrow = 1))
  colnames(defaultParams) <- c(params, 'performance')
  i <- 1
  while(i <= length(gparams)){
    parI <- gparams[i]
    defaultParams[[parI]] <- result[[parI]]$'default'
    require <- result[[parI]]$'requires'[[result[[parI]]$'default']]$'require'
    gparams <- c(gparams, require)
    i <- i + 1
  }
  initParams <- unlist(strsplit(initParams, "#"))

  j <- 1
  for(i in colnames(defaultParams)){
    if(i == 'performance' || i == 'nodesize')
      next
    if(initParams[j] == 'NA')
      defaultParams[[i]] <- NA
    else
      defaultParams[[i]] <- initParams[j]

    j <- j + 1
  }
  defaultParams[["EI"]] <- NA
  return (defaultParams)
}
