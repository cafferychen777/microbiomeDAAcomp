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
#' \dontrun{
#' # Create heatmap visualization
#' p1 <- plot_performance(results, plot_type = "heatmap", theme = "default")
#' 
#' # Create boxplot with confidence intervals
#' p2 <- plot_performance(results, plot_type = "boxplot", theme = "dark")
#' 
#' # Create interactive violin plot
#' p3 <- plot_performance(results, plot_type = "violin", theme = "classic")
#' 
#' # Print plots
#' print(p1)
#' print(p2)
#' print(p3)
#' }
#' 
#' @importFrom ggplot2 ggplot aes geom_tile geom_boxplot geom_violin geom_point
#' @importFrom ggplot2 facet_wrap theme_minimal theme_dark theme_classic
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>%
#' @importFrom plotly ggplotly
plot_performance <- function(results, 
                           plot_type = c("heatmap", "boxplot", "violin"),
                           theme = "default") {
    # Input validation
    plot_type <- match.arg(plot_type)
    if (!inherits(results, "daa_performance")) {
        stop("results must be a daa_performance object")
    }
    
    # Get metric columns (excluding method, rank and CI columns)
    metric_cols <- names(results)[!grepl("(method|_rank|_ci)", names(results))]
    
    # Prepare data for plotting
    plot_data <- results %>%
        tidyr::pivot_longer(
            cols = all_of(metric_cols),
            names_to = "metric",
            values_to = "value"
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
                    ggplot2::aes(label = sprintf("%.3f", value)),
                    color = "white",
                    size = 3
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
                ggplot2::geom_boxplot() +
                ggplot2::facet_wrap(~metric, scales = "free_y") +
                ggplot2::labs(
                    title = "Performance Metrics Boxplot",
                    y = "Value"
                )
        },
        "violin" = {
            # Create violin plot
            ggplot2::ggplot(plot_data, 
                           ggplot2::aes(x = method, y = value, fill = method)) +
                ggplot2::geom_violin() +
                ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.2)) +
                ggplot2::facet_wrap(~metric, scales = "free_y") +
                ggplot2::labs(
                    title = "Performance Metrics Violin Plot",
                    y = "Value"
                )
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
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            plot.title = ggplot2::element_text(hjust = 0.5)
        )
    
    # Add confidence intervals if available
    if (any(grepl("_ci_", names(results)))) {
        if (plot_type %in% c("boxplot", "violin")) {
            p <- p + ggplot2::geom_errorbar(
                data = results %>%
                    tidyr::pivot_longer(
                        cols = metric_cols,
                        names_to = "metric",
                        values_to = "value"
                    ),
                ggplot2::aes(
                    ymin = get(paste0(metric, "_ci_lower")),
                    ymax = get(paste0(metric, "_ci_upper"))
                ),
                width = 0.2,
                color = "darkgray"
            )
        }
    }
    
    # Add interactive features if plotly is available
    if (requireNamespace("plotly", quietly = TRUE)) {
        p <- plotly::ggplotly(p)
    }
    
    return(p)
}

# Helper function to check if a column exists
has_column <- function(data, col) {
    col %in% names(data)
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

# Add print method for plots
#' @export
print.daa_performance_plot <- function(x, ...) {
    if (inherits(x, "plotly")) {
        print(x)
    } else {
        print(ggplot2::ggplot_build(x))
    }
} 