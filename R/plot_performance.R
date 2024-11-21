#' Visualize Performance Comparison Results
#' 
#' @title Create Visualization for Performance Metrics
#' @description Generate various types of plots to visualize and compare performance metrics
#' across different differential abundance analysis methods. Supports multiple visualization
#' types and customizable themes.
#' 
#' @param results A data frame of class 'daa_performance' containing performance metrics
#'        Must include 'method' column and at least one metric column
#' @param plot_type Type of plot to generate. Options:
#'        \itemize{
#'          \item "heatmap": Creates a heatmap showing all metrics for each method
#'          \item "boxplot": Creates boxplots for each metric grouped by method
#'          \item "violin": Creates violin plots with jittered points for distribution
#'        }
#' @param theme Visual theme settings. Options:
#'        \itemize{
#'          \item "default": Clean minimal theme
#'          \item "dark": Dark background theme
#'          \item "classic": Classic theme with white background
#'        }
#' @param colors Colors for the plots. Default: c("#4A90E2", "#50C878", "#E67E22")
#'
#' @return A ggplot2 object (or plotly object if plotly is available) containing:
#'         \itemize{
#'           \item Performance visualization based on specified plot_type
#'           \item Confidence intervals (if available in input data)
#'           \item Interactive features (if plotly is available)
#'         }
#'
#' @details
#' The function automatically handles:
#' - Confidence interval visualization for boxplot and violin plots
#' - Interactive features when plotly package is available
#' - Automatic scaling for different metrics
#' - Proper angle rotation for method labels
#'
#' @export
#'
#' @examples
#' # Create sample test results with more data points
#' set.seed(123)
#' n_samples <- 100
#' test_results <- list(
#'   "DESeq2" = sample(c(TRUE, FALSE), n_samples, replace = TRUE, prob = c(0.7, 0.3)),
#'   "edgeR" = sample(c(TRUE, FALSE), n_samples, replace = TRUE, prob = c(0.6, 0.4)),
#'   "limma" = sample(c(TRUE, FALSE), n_samples, replace = TRUE, prob = c(0.5, 0.5))
#' )
#' true_status <- sample(c(TRUE, FALSE), n_samples, replace = TRUE)
#' 
#' # Calculate performance metrics
#' perf <- evaluate_performance(
#'   test_results,
#'   true_status,
#'   metrics = c("sensitivity", "specificity", "precision", "f1_score", "accuracy")
#' )
#' 
#' # Create different types of plots
#' # Heatmap visualization
#' p1 <- plot_performance(perf, plot_type = "heatmap", theme = "default")
#' if (interactive()) print(p1)
#' 
#' # Boxplot visualization
#' p2 <- plot_performance(perf, plot_type = "boxplot", theme = "classic")
#' if (interactive()) print(p2)
#' 
#' \dontrun{
#'   # Violin plot visualization
#'   p3 <- plot_performance(perf, plot_type = "violin", theme = "dark")
#'   if (interactive()) print(p3)
#' }
#' 
#' @importFrom ggplot2 ggplot aes geom_tile geom_boxplot geom_violin geom_point
#' @importFrom ggplot2 facet_wrap theme_minimal theme_dark theme_classic
#' @importFrom tidyr pivot_longer
#' @importFrom plotly ggplotly layout
#' @importFrom tidyselect all_of
#' @importFrom utils packageVersion
plot_performance <- function(results, 
                           plot_type = c("heatmap", "boxplot", "violin"),
                           theme = "default",
                           colors = c("#4A90E2", "#50C878", "#E67E22")) {
    # Input validation
    plot_type <- match.arg(plot_type)
    if (!inherits(results, "daa_performance")) {
        stop("results must be a daa_performance object")
    }
    
    # Check the number of data points
    if (plot_type == "violin" && nrow(results) < 3) {
        warning("Not enough data points for violin plot. Switching to boxplot.")
        plot_type <- "boxplot"
    }
    
    # Get metric columns (excluding method, rank and CI columns)
    metric_cols <- names(results)[!grepl("(method|_rank|_ci)", names(results))]
    
    # Prepare data for plotting
    plot_data <- tidyr::pivot_longer(
        results,
        cols = tidyselect::all_of(metric_cols),
        names_to = "metric",
        values_to = "value"
    )
    
    # Define more professional metric labels
    metric_labels <- c(
        "accuracy" = "Accuracy",
        "f1_score" = "F1 Score",
        "precision" = "Precision",
        "sensitivity" = "Sensitivity",
        "specificity" = "Specificity"
    )
    
    # Create base plot based on type
    p <- switch(plot_type,
        "heatmap" = {
            # Create heatmap
            ggplot2::ggplot(plot_data, 
                           ggplot2::aes(x = method, y = metric, fill = value)) +
                ggplot2::geom_tile() +
                ggplot2::scale_fill_viridis_c(limits = c(0, 1)) +
                ggplot2::geom_text(
                    ggplot2::aes(label = sprintf("%.2f", value)),
                    color = "white",
                    size = 4
                ) +
                ggplot2::labs(
                    title = "Performance Metrics Heatmap",
                    fill = "Value"
                )
        },
        "boxplot" = {
            # Create boxplot
            ggplot2::ggplot(plot_data, 
                           ggplot2::aes(x = method, y = value, fill = method)) +
                ggplot2::geom_boxplot(alpha = 0.8, width = 0.5) +
                ggplot2::facet_wrap(~metric, 
                                  scales = "free_y",
                                  labeller = as_labeller(metric_labels)) +
                ggplot2::scale_y_continuous(limits = c(0, 1),
                                          breaks = seq(0, 1, 0.25),
                                          labels = scales::percent_format()) +
                ggplot2::scale_fill_manual(values = colors) +
                ggplot2::labs(
                    y = "Performance Score",
                    x = "Method"
                ) +
                ggplot2::guides(fill = "none")
        },
        "violin" = {
            # Modify the implementation of the violin plot
            ggplot2::ggplot(plot_data, 
                           ggplot2::aes(x = method, y = value, fill = method)) +
                # First add a boxplot as the foundation
                ggplot2::geom_boxplot(width = 0.2, alpha = 0.4) +
                # Add violin plot
                ggplot2::geom_violin(alpha = 0.7, scale = "width") +
                ggplot2::facet_wrap(~metric, 
                                  scales = "free_y",
                                  labeller = as_labeller(metric_labels)) +
                ggplot2::scale_y_continuous(limits = c(0, 1),
                                          breaks = seq(0, 1, 0.25),
                                          labels = scales::percent_format()) +
                ggplot2::scale_fill_manual(values = colors) +
                ggplot2::labs(
                    y = "Performance Score",
                    x = "Method"
                ) +
                ggplot2::guides(fill = "none")
        }
    )
    
    # Apply theme
    p <- p + switch(theme,
        "default" = ggplot2::theme_minimal(),
        "dark" = ggplot2::theme_dark(),
        "classic" = ggplot2::theme_classic(),
        ggplot2::theme_minimal()  # Default fallback
    ) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, 
                                              hjust = 1,
                                              face = "bold"),
            axis.text.y = ggplot2::element_text(face = "bold"),
            axis.title = ggplot2::element_text(face = "bold", size = 12),
            strip.text = ggplot2::element_text(face = "bold", size = 11),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.spacing = ggplot2::unit(1.5, "lines"),
            plot.title = ggplot2::element_text(hjust = 0.5,
                                             face = "bold",
                                             size = 14),
            plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10)
        )
    
    # Add Plotly support at the end of the function
    if (requireNamespace("plotly", quietly = TRUE)) {
        p <- plotly::layout(
            plotly::ggplotly(p),
            showlegend = FALSE,
            margin = list(t = 50)
        )
    }
    
    return(p)
}

#' Check if a Data Frame Has a Specific Column
#'
#' @param df A data frame to check
#' @param col_name Character string specifying the column name to look for
#'
#' @return Logical value indicating whether the column exists (TRUE) or not (FALSE)
#'
#' @examples
#' df <- data.frame(a = 1:3, b = letters[1:3])
#' has_column(df, "a")  # Returns TRUE
#' has_column(df, "c")  # Returns FALSE
#'
#' @keywords internal
has_column <- function(df, col_name) {
    col_name %in% colnames(df)
}

#' Plot Method for DAA Performance Objects
#'
#' @description
#' Automatically creates a visualization of DAA performance results using ggplot2.
#'
#' @param object A daa_performance object containing performance metrics
#' @param ... Additional arguments passed to plot_performance
#'
#' @return A ggplot2 object
#'
#' @method autoplot daa_performance
#' @export
autoplot.daa_performance <- function(object, ...) {
    plot_performance(object, ...)
}

#' Print Method for DAA Performance Plot Objects
#'
#' @param x A daa_performance_plot object to print
#' @param ... Additional arguments passed to print methods
#'
#' @return Invisibly returns the input object
#'
#' @examples
#' \dontrun{
#' perf_plot <- plot_performance(performance_data)
#' print(perf_plot)
#' }
#'
#' @export
print.daa_performance_plot <- function(x, ...) {
    if (inherits(x, "plotly")) {
        print(x)
    } else {
        print(ggplot2::ggplot_build(x))
    }
} 