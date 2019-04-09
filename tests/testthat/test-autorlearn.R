context("test-autorlearn")

test_that("option1", {
  result1 <- autoRLearn(1, system.file("extdata", "shuttle/train.arff", package = "SmartML"), system.file("extdata", "shuttle/train.arff", package = "SmartML"), option = 1, preProcessF = 'pca', nComp = 3, nModels = 2)
  result1$clfs  #Vector of recommended nModels classifiers
  result1$params #Vector of initial suggested parameter configurations of nModels recommended classifiers
})

