#' @title Output Classifier Configuration into a readable string
#' @description Get Default parameters of certain classifier algorithm
#' @param classifierName Name of Classifier Algorithm
#' @param result Data Frame from the JSON of parameters of classifier
#' @keywords AutoML, SmartML
#' @seealso \code{\link[utils]{head}}
#' @return String of Classifier Parameters
#' @examples \dontrun{ outClassifierConf(classifierAlgorithm, classifierConf)
#' }
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
  #print('%%%%%%%%')
  #print(out)
  return(out)
}
