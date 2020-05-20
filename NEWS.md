# SmartML 0.3.0.1

* Hotfix, fixed some dependency issues relating to dplyr

# SmartML 0.3.0

## Features

* Added Ranger, XGBoost, fastNaiveBayes and LiblineaR high performing algorithms
* Added the autoRLearn_ function, which assumes that the data is in perfect shape and can be loaded from a dataframe, unlike autoRLearn which can only load from a data file outside R.
* Added Hyperband and Bayesian Optimization Hyperband to the new autoRLearn_
* Added some extra temporary dependencies which will be removed in the following months (all tidyverse packages other than purrr)
* Fixed some small mistakes in the code and jsons

## Current Roadmap

* fix metalearning, at the moment it doesn't work. There's something wrong with the AWS server we are using.
* change the dplyr back end to use data.table with dtplyr
* merge autoRLearn and autoRLearn_ into a single function, which can both load from a data file and in R.
* Rewrite SMAC, as requested by Sherif.

## Extra info

* brurucy is a new and active maintainer
* Nightly and experimental versions, independent from the Data Systems Lab, are being developed at https://github.com/brurucy/witchcraft
* Updates will be conservative and focused on non-breaking changes, up to release 1.0.
