# Title     : Testing the Main Package Function
# Objective : Package Testing
# Created by: s-moh
# Created on: 11/12/2020
library(SmartML)
library(tidyverse)
library(R.utils)
library(mlr)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3filters)
library(readr)
library(data.table)
library(stringr)
library(jsonlite)
library(tictoc)

#################################################################################################
# Classification

"lrn1 <- lrn('classif.rpart', predict_type = 'prob')
lrn2 <- lrn('classif.ranger', predict_type = 'prob')
lrn3 <- lrn('classif.svm', predict_type = 'prob')

rpart_cv1 = po('learner_cv', lrn1, id = 'lrn1')
ranger_cv1 = po('learner_cv', lrn2, id = 'lrn2')
svm_cv1 = po('learner_cv', lrn3, id = 'lrn3')
lrns = c(rpart_cv1, ranger_cv1, svm_cv1)

level0 = gunion(list(
  lrns))  %>>%
  po('featureunion', id = 'union1')

ensemble = level0 %>>% LearnerClassifAvg$new(id = 'classif.avg')
ensemble$plot(html = FALSE)

ens_lrn = GraphLearner$new(ensemble)
ens_lrn$predict_type = 'prob'

task = mlr_tasks$get('iris')
train.idx = sample(seq_len(task$nrow), 120)
test.idx  = setdiff(seq_len(task$nrow), train.idx)

perf <- ens_lrn$train(task, train.idx)$predict(task, test.idx)$score(msr('classif.acc'))
print(perf)"

#################################################################################################

data_train <- readr::read_csv('inst/extdata/tictactoe_train.csv') %>%
  as.data.table()

data_test <- readr::read_csv('inst/extdata/tictactoe_test.csv') %>%
  as.data.table()

data_train[, class := factor(class, levels = unique(class)) %>% sort()]
data_test[, class := factor(class, levels = unique(class)) %>% sort()]

opt <- SmartML::evocate(df_train = data_train,
               df_test = data_test,
               models = c('rpart', 'ranger', 'svm'),
               #'svm(done)', 'kknn(done)', 'ranger(done)', 'rpart(done)',
               #'xgboost(done)', 'cv_glmnet(done)', 'naive_bayes(done)'
               optimizationAlgorithm = 'hyperband',
               maxTime = 5, ensemble_size = 3)

print(opt)
gc()
