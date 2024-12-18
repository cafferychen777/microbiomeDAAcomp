% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensitivity_analysis.R
\name{sensitivity_analysis}
\alias{sensitivity_analysis}
\title{Perform Sensitivity Analysis for DAA Methods}
\usage{
sensitivity_analysis(data, parameters, method_names)
}
\arguments{
\item{data}{Microbiome data object containing count matrix and metadata}

\item{parameters}{List of parameters to vary, where:
\itemize{
\item Names are parameter names (e.g., "alpha", "min_count")
\item Values are vectors of parameter values to test
}}

\item{method_names}{Character vector of DAA methods to analyze}
}
\value{
A list of class "daa_sensitivity" containing:
\itemize{
\item results: Nested list of sensitivity results for each method
\item parameter_effects: Data frame summarizing parameter effects
\item overlap_ratios: Matrix of result overlap between parameter values
\item effect_sizes: Numeric vector of effect sizes for parameter changes
}
}
\description{
Conducts sensitivity analysis for differential abundance analysis (DAA) methods
by systematically varying input parameters and evaluating their impact on results.
}
\details{
The function evaluates:
\itemize{
\item Changes in number of significant features
\item Result overlap between parameter values
\item Effect sizes of parameter variations
\item Stability of results across parameter ranges
}

For each parameter and method combination, the function:
\enumerate{
\item Establishes baseline results using first parameter value
\item Compares results from other parameter values to baseline
\item Calculates overlap ratios and effect sizes
\item Aggregates results for easy interpretation
}
}
\examples{
# Create sample data
set.seed(123)
counts <- matrix(
  rpois(100 * 20, lambda = 10),
  nrow = 100,
  ncol = 20,
  dimnames = list(
    paste0("Feature", 1:100),
    paste0("Sample", 1:20)
  )
)

metadata <- data.frame(
  group = factor(rep(c("Control", "Treatment"), each = 10)),
  row.names = colnames(counts)
)

microbiome_data <- list(
  counts = counts,
  metadata = metadata
)

# Run sensitivity analysis
sensitivity_results <- sensitivity_analysis(
  data = microbiome_data,
  parameters = list(
    alpha = c(0.01, 0.05, 0.1),
    min_count = c(5, 10, 20)
  ),
  method_names = c("DESeq2", "ALDEx2")
)

# Access results
str(sensitivity_results)

# Get parameter effects for a specific method
deseq2_alpha_results <- sensitivity_results$DESeq2$alpha
print(deseq2_alpha_results)
}
\seealso{
\code{\link{run_daa_methods}} for running individual DAA methods
}
