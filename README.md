<img src = "https://bigdata.cs.ut.ee/smartml/images/banner.png">


### SmartML: 
Cuurently, SmartML is an R-Package representing a meta learning-based framework for automated selection and hyperparameter tuning for machine learning algorithms. Being meta-learning based, the framework is able to simulate the role of the machine learning expert. In particular, the framework is equipped with a continuously updated knowledge base that stores information about the meta-features of all processed datasets along with the associated performance of the different classifiers and their tuned parameters. Thus, for any new dataset, SmartML automatically extracts its meta features and searches its knowledge base for the best performing algorithm to start its optimization process. In addition, SmartML makes use of the new runs to continuously enrich its knowledge base to improve its performance and robustness for future runs
---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r setup, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```
# SmartML

The goal of SmartML is to ...

## Installation

You can install the released version of SmartML from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("SmartML")
```

## Example

This is a basic example which shows you how to solve a common problem:

```{r example}
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so:

```{r cars}
summary(cars)
```

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date.

You can also embed plots, for example:

```{r pressure, echo = FALSE}
plot(pressure)
```

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub!
