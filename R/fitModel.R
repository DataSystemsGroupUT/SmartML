#' @title Fit SMAC Model.
#'
#' @description Fit the trees of the SMAC forest model by adding new nodes to each of the forest trees.
#'
#' @param params A string of parameter configuration values for the current classifier to be tuned (parameters are separated by #).
#' @param bestPerf Vector of performance values of the best parameter configuration on the folds of the SMAC model.
#' @param trainingSet Dataframe of the training set.
#' @param validationSet Dataframe of the validation Set.
#' @param foldedSet List of the folds of the dataset in each tree of the SMAC forest.
#' @param classifierAlgorithm String of the name of classifier algorithm used now.
#' @param tree List of data frames, representing the data structure for the forest of trees of the SMAC model.
#' @param B number of trees in the forest of trees of SMAC optimization algorithm (default = 10).
#' @param metric Metric to be used in evaluation:
#' \itemize{
#' \item "acc" - Accuracy,
#' \item "fscore" - Micro-Average of F-Score of each label,
#' \item "recall" - Micro-Average of Recall of each label,
#' \item "precision" - Micro-Average of Precision of each label
#' }
#'
#' @return List of: \code{t} trees of fitted SMAC Model - \code{p} performance of current parameter configuration on whole dataset - \code{bp} Current added parameter configuration.
#'
#' @examples fitModel('1', c(0.91, 0.89), data.frame(salary = c(623, 515, 611, 729, 843), class = c (0, 0, 0, 1, 1)), data.frame(salary = c(400, 800), class = c (0, 1)), list(c(1,2,4), c(3,5)), 'knn', data.frame(fold = c(), parent = c(), params = c(), leftChild = c(), rightChild = c(), performance = c(), rowN = c()), 2).
#'
#' @noRd
#'
#' @keywords internal

fitModel <- function(params, bestPerf, trainingSet, validationSet, foldedSet, classifierAlgorithm, tree, B = 10, metric = 'acc') {
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
      perf <- c(perf, (runClassifier(trainingSet[foldedSet[[i]], ], validationSet, cntParams, classifierAlgorithm, metric = metric))$perf)

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
