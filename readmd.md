# diaggnosticplots

The goal of this library is to make useful plots for evaluation of machine learning models available in the ggplot2 universe. The computation are done using the [caret](https://cran.r-project.org/web/packages/caret/index.html) package. At the moment, the goal is to have the following geometries:

+ geom_lift: this plots the [lift](https://en.wikipedia.org/wiki/Lift_(data_mining)) as a functino of the cutoff
+ geom_gain: this plots the [gain](http://themainstreamseer.blogspot.ch/2012/07/understanding-and-interpreting-gain-and.html) curve: it orders the observations according to their score and then plots the percentage of observations tested versus the percentage of events found
+ geom_calib: plots the calibration curve

## Usage
