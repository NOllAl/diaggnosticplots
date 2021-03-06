% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_lift.R
\name{geom_lift}
\alias{geom_lift}
\title{Geom for lift curve}
\usage{
geom_lift(mapping = NULL, data = NULL, stat = "lift",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE, ...)
}
\arguments{
\item{mapping}{Set of aestetic mappings. Required aesthetics are `score` and
`class`.}

\item{data}{The data to be displayer in this layer.}

\item{positive_class}{character indicating the "positive" class of interest.}
}
\value{
ggplot
}
\description{
Plots the lift curve for evaluating a trained classification
model.
}
\examples{
library(dplyr)
library(mlbench)
data("BreastCancer")
my_data <- BreastCancer \%>\%
  mutate(Cell.size = as.numeric(Cell.size)) \%>\%
    select(Cell.size, Class)

fit <- glm(Class ~ ., data = my_data, family = "binomial")
my_data$pred <- predict(fit, newdata = my_data)
my_data \%>\%
  ggplot(aes(score = pred, class = Class)) +
  geom_lift(positive_class = "malignant")
}

