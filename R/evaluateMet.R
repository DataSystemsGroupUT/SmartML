#' @title Evaluate Fitted Model.
#'
#' @description Evaluate Predictions obtained from a specific model based on true labels, its predictions, and the evaluation metric.
#'
#' @param yTrue Vector of true labels.
#' @param pred Vector of predicted labels.
#' @param metric Metric to be used in evaluation:
#' \itemize{
#' \item "acc" - Accuracy,
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
#' @importFrom  e1071 svm naiveBayes
evaluateMet <- function(yTrue, pred, metric = "acc"){
  if (metric == 'acc'){

  }
  else if(metric == 'fscore'){

  }
  else if(metric == 'recall'){
    score <- Recall_micro(y_pred = pred, y_true = truth, labels)
  }
  else{

  }
}
