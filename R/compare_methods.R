#' Compare Different DAA Methods
#' 
#' @param performance_results Data frame of performance metrics
#' @param comparison_type Type of comparison to perform
#' @param ... Additional arguments for specific comparison types
#'
#' @return List containing comparison results and visualizations
#' @export
#'
#' @examples
#' \dontrun{
#' comparison <- compare_methods(performance_results, comparison_type = "statistical")
#' }
compare_methods <- function(performance_results, 
                          comparison_type = c("statistical", "visual", "comprehensive"),
                          ...) {
    comparison_type <- match.arg(comparison_type)
    
    # Input validation
    if (!is.data.frame(performance_results)) {
        stop("performance_results must be a data frame")
    }
    
    required_cols <- c("method", "accuracy", "precision", "recall", "f1_score")
    if (!all(required_cols %in% colnames(performance_results))) {
        stop("performance_results must contain columns: ", 
             paste(required_cols, collapse = ", "))
    }
    
    # Initialize results list
    results <- list()
    
    # Perform comparison based on type
    if (comparison_type %in% c("statistical", "comprehensive")) {
        # Statistical comparison using Friedman test
        friedman_test <- friedman.test(
            y = performance_results$accuracy,
            groups = performance_results$method
        )
        
        # Post-hoc analysis using Nemenyi test
        posthoc <- PMCMRplus::nemenyi.test(
            y = performance_results$accuracy,
            groups = performance_results$method
        )
        
        results$statistical <- list(
            friedman = friedman_test,
            posthoc = posthoc
        )
    }
    
    if (comparison_type %in% c("visual", "comprehensive")) {
        # Create performance visualization
        perf_plot <- ggplot2::ggplot(performance_results, 
                                    ggplot2::aes(x = method)) +
            ggplot2::geom_boxplot(ggplot2::aes(y = accuracy, fill = method)) +
            ggplot2::theme_minimal() +
            ggplot2::labs(title = "Performance Comparison of DAA Methods",
                         y = "Accuracy",
                         x = "Method")
        
        results$visual <- list(
            performance_plot = perf_plot
        )
    }
    
    # Add summary statistics
    results$summary <- performance_results %>%
        dplyr::group_by(method) %>%
        dplyr::summarise(
            mean_accuracy = mean(accuracy),
            sd_accuracy = sd(accuracy),
            mean_precision = mean(precision),
            mean_recall = mean(recall),
            mean_f1 = mean(f1_score)
        )
    
    class(results) <- c("daa_comparison", "list")
    return(results)
}

# Add print method for daa_comparison class
#' @export
print.daa_comparison <- function(x, ...) {
    cat("DAA Method Comparison Results\n")
    cat("============================\n\n")
    
    if (!is.null(x$summary)) {
        cat("Performance Summary:\n")
        print(x$summary)
        cat("\n")
    }
    
    if (!is.null(x$statistical)) {
        cat("Statistical Test Results:\n")
        print(x$statistical$friedman)
        cat("\n")
    }
} 