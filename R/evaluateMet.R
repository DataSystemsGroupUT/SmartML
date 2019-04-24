#' @title Evaluate Fitted Model.
#'
#' @description Evaluate Predictions obtained from a specific model based on true labels, its predictions, and the evaluation metric.
#'
#' @param yTrue Vector of true labels.
#' @param pred Vector of predicted labels.
#' @param metric Metric to be used in evaluation:
#' \itemize{
#' \item "acc" - Accuracy,
#' \item "avg-fscore" - Average of F-Score of each label,
#' \item "avg-recall" - Average of Recall of each label,
#' \item "avg-precision" - Average of Precision of each label,
#' \item "fscore" - Micro-Average of F-Score of each label,
#' \item "recall" - Micro-Average of Recall of each label,
#' \item "precision" - Micro-Average of Precision of each label.
#' }
#'
#' @return Float number representing the evaluation.
#'
#' @examples
#' \dontrun{
#' result1 <- autoRLearn(10, 'sampleDatasets/shuttle/train.arff', 'sampleDatasets/shuttle/test.arff')
#' }
#'
#' @noRd
#'
#' @keywords internal
#'
evaluateMet <- function(yTrue, pred, metric = 'acc'){
  cm = as.matrix(table(Actual = yTrue, Predicted = pred)) # create the confusion matrix
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class

  oneVsAll = lapply(1 : nc,
                    function(i){
                      v = c(cm[i,i],
                            rowsums[i] - cm[i,i],
                            colsums[i] - cm[i,i],
                            n-rowsums[i] - colsums[i] + cm[i,i]);
                      return(matrix(v, nrow = 2, byrow = T))})

  s = matrix(0, nrow = 2, ncol = 2)
  for(i in 1 : nc){s = s + oneVsAll[[i]]}

  if (metric == 'acc'){
    perf <- sum(diag) / n
  }
  else if(metric == 'avg-precision'){
    precision <- diag / colsums
    perf <- mean(precision)
  }
  else if(metric == 'avg-recall'){
    recall <- diag / rowsums
    perf <- mean(recall)
  }
  else if(metric == 'avg-fscore'){
    precision <- diag / colsums
    recall <- diag / rowsums
    f1 <- 2 * precision * recall / (precision + recall)
    perf <- mean(f1)
  }
  else{
    perf <- (diag(s) / apply(s,1, sum))[1];

  }
  return(perf)
}
