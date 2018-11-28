#' @title Fitting SMAC Model based on current best parameters
#' @description Fit the regression tree of SMAC model on dataset
#' @param params parameters to run the classifier with
#' @param bestPerf performance of best parameters on forest trees' folds
#' @param trainingSet Training section of the dataset
#' @param validationSet validation section of the dataset
#' @param classifierAlgorithm type of classifier algorithm that will be used
#' @param tree SMAC random Forest model
#' @keywords AutoML, SMAC
#' @seealso \code{\link[utils]{head}}
#' @return parameters with corresponding performance
#' @examples \dontrun{ fitModel(params, trainingSet, validationSet, tree)
#' }
fitModel <- function(params, bestPerf, trainingSet, validationSet, foldedSet, classifierAlgorithm, tree, B = 10) {
  #fit SMAC model using the current best parameters
  #get current best parameters
  cntParams <- params
  cntParamStr <- paste( unlist(cntParams), collapse='#')
  #initiate a variable to store its performance on each decision tree of the forest
  perf <- c()
  for(i in 1:B){
    cntNode <- tree[tree$fold==i & is.na(tree$parent), ]
    #Get position to add the new node
    cParent <- NA
    cChild <- NA
    if(nrow(cntNode) > 0){
      cParent <- cntNode$rowN
      while(!is.na(cntNode[[1]])){
        cParent <- cntNode$rowN
        if(cntParamStr > as.character(cntNode$params)){
          cntNode <- tree[as.integer(cntNode$rightChild), ]
          cChild <- 5 #pointer position to right node
        }
        else if(cntParamStr < as.character(cntNode$params)){
          cntNode <- tree[as.integer(cntNode$leftChild), ]
          cChild <- 4 #pointer position to left node
        }
        else{
          return(list(bp = params, t = tree, p=bestPerf))
        }
      }
    }

    if(length(bestPerf) >= i)
      perf <- bestPerf
    else
      perf <- c(perf, runClassifier(trainingSet[foldedSet[[i]], ], validationSet, cntParams, classifierAlgorithm))

    #row number of new node to be added
    newRowN <- nrow(tree) + 1
    #Update parent's child
    if(!is.na(cChild))
      tree[cParent, cChild] <- newRowN
    #Add new node with current configuration
    df <- data.frame(fold = i, parent = cParent, params = cntParamStr, leftChild = NA, rightChild = NA, performance = perf[i], rowN = newRowN)
    tree <- rbind(tree, df)
  }

  cntParams$performance <- mean(perf)
  return(list(t = tree, p=perf, bp=cntParams))
}
