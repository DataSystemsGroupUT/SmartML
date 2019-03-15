#' @title Output Classifier Parameter Configuration.
#'
#' @description Get the classifier parameter configuration in a human readable format.
#'
#' @param classifierName String of the name of classifier algorithm used now.
#' @param result List of the converted classifier json parameter configuration into set of vectors and lists.
#' @param initParams String of parameters of \code{classifierName} separated by #.
#'
#' @return String of the human readable output in HTML format.
#'
#' @examples outClassifierConf('knn', list(params = c('k'), parents = c('k'), k = list(default = '7', require = c())), '1')
#'
#' @noRd
#'
#' @keywords internal

outClassifierConf <- function(classifierName, result, initParams) {
  #get list of Classifier Parameters
  params <- result$params
  #get list of GrandParent parametes
  gparams <- result$parents
  #Create dataFrame for classifier default parameters
  defaultParams <- data.frame(matrix(ncol = length(params), nrow = 1))
  colnames(defaultParams) <- c(params)

  i <- 1
  while(i <= length(gparams)){
    parI <- gparams[i]
    defaultParams[[parI]] <- result[[parI]]$'default'
    require <- result[[parI]]$'requires'[[result[[parI]]$'default']]$'require'
    gparams <- c(gparams, require)
    i <- i + 1
  }
  initParams <- unlist(strsplit(initParams, "#"))

  out <- ''
  j <- 1
  for(i in colnames(defaultParams)){
    if(i == 'nodesize')
      next
    #print(i)
    out <- paste(out, ' -<i>', i, ':</i>', initParams[j], collapse = '')
    j <- j + 1
  }

  #print(out)
  return(out)
}
