#' @title Send Meta-Features to Temporary File
#' @description Save Meta Features with results obtained in the database
#' @param df data frame with meta features
#' @param algorithmName Name of best classifier algorithm
#' @param bestParams best parameters chosen
#' @seealso \code{\link[utils]{head}}
#' @examples \dontrun{ sendToTmp(metaFeats, "svmkk", list(x=1, y=2, z=3))
#' }
sendToTmp <- function(df, algorithmName, bestParams, perf) {
  #print('H1')
  df$params <- sprintf("'%s'", paste( unlist(bestParams), collapse='#'))
  #print('H2')
  df$performance <- perf
  #print('H3')
  df$classifierAlgorithm <- sprintf("'%s'", algorithmName)
  #print('H4')
  query <- sprintf("%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s",
                   df$datasetRatio, df$featuresKurtStdDev, df$featuresKurtMean, df$featuresKurtMax, df$featuresKurtMin, df$featuresSkewStdDev,
                   df$featuresSkewMean, df$featuresSkewMax, df$featuresSkewMin, df$symbolsStdDev, df$symbolsSum, df$symbolsMean, df$classProbStdDev,
                   df$classProbMean, df$classProbMax, df$classProbMin, df$classEntropy, df$ratioNumToCat, df$nCatFeatures, df$nNumFeatures,
                   df$nInstances, df$nFeatures, df$nClasses, df$lognFeatures, df$lognInstances, df$classifierAlgorithm, df$params, df$maxTime, "'acc'",df$performance)
  #print('H5')
  write(query,file="tmp",append=TRUE)
}
