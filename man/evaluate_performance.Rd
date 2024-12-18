% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate_performance.R
\name{evaluate_performance}
\alias{evaluate_performance}
\title{Evaluate Performance of DAA Methods}
\usage{
evaluate_performance(
  test_results,
  true_status,
  metrics = c("sensitivity", "specificity", "precision", "f1_score"),
  conf_level = 0.95
)
}
\arguments{
\item{test_results}{List of method results, where each element contains logical vectors
of predicted differential abundance status}

\item{true_status}{Logical vector of true differential abundance status}

\item{metrics}{Character vector specifying which metrics to calculate. Available options:
\itemize{
\item "sensitivity": True positive rate (TPR)
\item "specificity": True negative rate (TNR)
\item "precision": Positive predictive value (PPV)
\item "f1_score": Harmonic mean of precision and sensitivity
\item "accuracy": Overall prediction accuracy
\item "mcc": Matthews correlation coefficient
}}

\item{conf_level}{Confidence level for intervals (default: 0.95)}
}
\value{
An object of class 'daa_performance' (data.frame) containing:
\itemize{
\item Requested performance metrics for each method
\item Confidence intervals for sensitivity, specificity, and precision
\item Rankings for applicable metrics
}
}
\description{
This function evaluates the performance of differential abundance analysis (DAA) methods
by calculating various performance metrics and their confidence intervals.
}
\details{
The function calculates performance metrics based on confusion matrix values.
Confidence intervals are computed using binomial test. All metrics (except MCC)
are bounded between 0 and 1. MCC is bounded between -1 and 1.
}
\examples{
\dontrun{
# Create sample test results
test_results <- list(
  method1 = c(TRUE, FALSE, TRUE, FALSE),
  method2 = c(TRUE, TRUE, FALSE, FALSE)
)
true_status <- c(TRUE, FALSE, TRUE, FALSE)

# Evaluate performance
perf <- evaluate_performance(
  test_results,
  true_status,
  metrics = c("sensitivity", "specificity", "precision")
)
print(perf)
plot(perf, metric = "sensitivity")
}
}
