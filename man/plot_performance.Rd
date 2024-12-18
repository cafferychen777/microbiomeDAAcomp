% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_performance.R
\name{plot_performance}
\alias{plot_performance}
\title{Create Visualization for Performance Metrics}
\usage{
plot_performance(
  results,
  plot_type = c("heatmap", "boxplot", "violin"),
  theme = "default",
  colors = c("#4A90E2", "#50C878", "#E67E22"),
  use_plotly = FALSE
)
}
\arguments{
\item{results}{A data frame of class 'daa_performance' containing performance metrics
Must include 'method' column and at least one metric column}

\item{plot_type}{Type of plot to generate. Options:
\itemize{
\item "heatmap": Creates a heatmap showing all metrics for each method
\item "boxplot": Creates boxplots for each metric grouped by method
\item "violin": Creates violin plots with jittered points for distribution
}}

\item{theme}{Visual theme settings. Options:
\itemize{
\item "default": Clean minimal theme
\item "dark": Dark background theme
\item "classic": Classic theme with white background
}}

\item{colors}{Colors for the plots. Default: c("#4A90E2", "#50C878", "#E67E22")}

\item{use_plotly}{Logical indicating whether to convert plot to interactive plotly object}
}
\value{
A ggplot2 object (or plotly object if use_plotly is TRUE) containing:
\itemize{
\item Performance visualization based on specified plot_type
\item Confidence intervals (if available in input data)
\item Interactive features (if plotly is enabled)
}
}
\description{
Generate various types of plots to visualize and compare performance metrics
across different differential abundance analysis methods. Supports multiple visualization
types and customizable themes.
}
\details{
Visualize Performance Comparison Results

The function automatically handles:
\itemize{
\item Confidence interval visualization for boxplot and violin plots
\item Interactive features when plotly package is available
\item Automatic scaling for different metrics
\item Proper angle rotation for method labels
}
}
\examples{
# Create sample test results with more data points
set.seed(123)
n_samples <- 100
test_results <- list(
  "DESeq2" = sample(c(TRUE, FALSE), n_samples, replace = TRUE, prob = c(0.7, 0.3)),
  "edgeR" = sample(c(TRUE, FALSE), n_samples, replace = TRUE, prob = c(0.6, 0.4)),
  "limma" = sample(c(TRUE, FALSE), n_samples, replace = TRUE, prob = c(0.5, 0.5))
)
true_status <- sample(c(TRUE, FALSE), n_samples, replace = TRUE)

# Calculate performance metrics
perf <- evaluate_performance(
  test_results,
  true_status,
  metrics = c("sensitivity", "specificity", "precision", "f1_score", "accuracy")
)

# Create different types of plots
# Heatmap visualization
p1 <- plot_performance(perf, plot_type = "heatmap", theme = "default")
if (interactive()) print(p1)

# Boxplot visualization
p2 <- plot_performance(perf, plot_type = "boxplot", theme = "classic")
if (interactive()) print(p2)

\dontrun{
  # Violin plot visualization
  p3 <- plot_performance(perf, plot_type = "violin", theme = "dark")
  if (interactive()) print(p3)
}

}
