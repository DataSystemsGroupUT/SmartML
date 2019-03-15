#' @title
#'
#' @description
#'
#' @param
#'
#' @return
#'
#' @examples
#'
#' @noRd
#'
#' @keywords internal

initialize <- function(classifierName, trainingSet, validationSet, result, initParams) {
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
  cat('params: ', initParams, '\n')
  initParams <- unlist(strsplit(initParams, "#"))

  j <- 1
  for(i in colnames(defaultParams)){
    if(i == 'performance' || i == 'nodesize')
      next
    print(i)
    if(initParams[j] == 'NA')
      defaultParams[[i]] <- NA
    else
      defaultParams[[i]] <- initParams[j]

    j <- j + 1
  }
  #print('finally:')
  #print(defaultParams)
  defaultParams[["EI"]] <- NA
  return (defaultParams)
}
