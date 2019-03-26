<img src = "https://bigdata.cs.ut.ee/smartml/images/banner.png">

## SmartML: 
Curently, SmartML is an R-Package representing a meta learning-based framework for automated selection and hyperparameter tuning for machine learning algorithms. Being meta-learning based, the framework is able to simulate the role of the machine learning expert. In particular, the framework is equipped with a continuously updated knowledge base that stores information about the meta-features of all processed datasets along with the associated performance of the different classifiers and their tuned parameters. Thus, for any new dataset, SmartML automatically extracts its meta features and searches its knowledge base for the best performing algorithm to start its optimization process. In addition, SmartML makes use of the new runs to continuously enrich its knowledge base to improve its performance and robustness for future runs.

<img src = "https://bigdata.cs.ut.ee/smartml/images/arch.jpg">

## SmartML

The goal of SmartML is to automate the process of classifier algorithm selection, and hyper-parameter tuning in supervised machine learning using a modified version of SMAC bayesian optimization that prefers explitation more than exploration thanks to Meta-Learning. 
1. SmartML is the first R package to deal with the sueprvised machine learning automation, and it is built over 16 different classifier algorithms from different R packages. <br>
2. In addition, we offer different data preprocessing, and feature engineering algorithms that can be specified by user and applied on tabular datasets of either CSV or ARFF extensions easily.
3. SmartML has a collaborative knowledge base that grows by time as more users are using our tool. So, we collect some statistical meta-features about their datasets with performance of different classifiers on these datasets. The collected data helps in the process of classifier algorithm selection by recommending $n$ candidate best classifiers to perform well on a new dataset. Then, time budget allowed is divided among these $n$ classifiers for their hyper-parameter tuning using SMAC.
4. Finally, SmartML has the ability to do some model interpretability plots for feature importance and interaction by help of ```iml``` package for ML model interpretability.
5. SmartML has a web service for the tool with a simple R Shiny interface that can be found <a href = "https://bigdata.cs.ut.ee/smartml/index.html"> HERE </a>

## Installation

You can install the released version of SmartML from [Github](https://github.com/DataSystemsGroupUT/Auto-Machine-Learning) with:

``` r
install_github("DataSystemsGroupUT/Auto-Machine-Learning")
```

---
## Documentation
```
Documentation for the SmartML R package can be found <a href = ""> HERE </a>
```

## Example

This is a basic example which shows you how to run SmartML simply:

```{r example}
#' Option 2 = Classifier Selection Only
result1 <- autoRLearn(1, 'sampleDatasets/car/train.arff', 'sampleDatasets/car/test.arff', option = 2, preProcessF = 'normalize', nModels = 2) #option 2 runs for Classifier Algorithm Selection Only
result1$clfs  #Vector of recommended nModels classifiers
result1$params #Vector of initial suggested parameter configurations of nModels classifiers
#' Option 1 = Both Classifier Selection and Parameter Optimization
result2 <- autoRLearn(10, 'sampleDatasets/shuttle/train.arff', 'sampleDatasets/shuttle/test.arff', interp = 1) # Option 1 runs for both classifier algorithm selection and parameter tuning for 10 minutes.
result2$clfs #best classifier found
result2$params #parameter configuration for best classifier
result2$perf #performance of chosen classifier on testing set
library(ggplot2)
print(plot(result2$interp$featImp)) #Feature Importance Plot

```

#### Publication

SmartML has been accepted as a DEMO paper at EDBT 19 in Lisbon Portugal <a href = "http://openproceedings.org/2019/conf/edbt/EDBT19_paper_235.pdf">[PDF]</a>:
```
Mohamed Maher, Sherif Sakr.,SMARTML: A Meta Learning-Based Framework for Automated Selection and Hyperparameter Tuning for Machine Learning Algorithms (2019). Advances in Database Technology-EDBT 2019: 22nd International Conference on Extending Database Technology, Lisbon, Portugal, March 26-29.
```
