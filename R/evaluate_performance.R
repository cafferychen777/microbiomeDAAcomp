#' Evaluate Performance of DAA Methods
#' 
#' This function evaluates the performance of differential abundance analysis (DAA) methods
#' by calculating various performance metrics and their confidence intervals.
#' 
#' @param test_results List of method results, where each element contains logical vectors 
#'        of predicted differential abundance status
#' @param true_status Logical vector of true differential abundance status
#' @param metrics Character vector specifying which metrics to calculate. Available options:
#'        \itemize{
#'          \item "sensitivity": True positive rate (TPR)
#'          \item "specificity": True negative rate (TNR)
#'          \item "precision": Positive predictive value (PPV)
#'          \item "f1_score": Harmonic mean of precision and sensitivity
#'          \item "accuracy": Overall prediction accuracy
#'          \item "mcc": Matthews correlation coefficient
#'        }
#' @param conf_level Confidence level for intervals (default: 0.95)
#'
#' @return An object of class 'daa_performance' (data.frame) containing:
#'         \itemize{
#'           \item Requested performance metrics for each method
#'           \item Confidence intervals for sensitivity, specificity, and precision
#'           \item Rankings for applicable metrics
#'         }
#' 
#' @details
#' The function calculates performance metrics based on confusion matrix values.
#' Confidence intervals are computed using binomial test. All metrics (except MCC) 
#' are bounded between 0 and 1. MCC is bounded between -1 and 1.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create sample test results
#' test_results <- list(
#'   method1 = c(TRUE, FALSE, TRUE, FALSE),
#'   method2 = c(TRUE, TRUE, FALSE, FALSE)
#' )
#' true_status <- c(TRUE, FALSE, TRUE, FALSE)
#' 
#' # Evaluate performance
#' perf <- evaluate_performance(
#'   test_results,
#'   true_status,
#'   metrics = c("sensitivity", "specificity", "precision")
#' )
#' print(perf)
#' plot(perf, metric = "sensitivity")
#' }
evaluate_performance <- function(test_results, true_status, 
                               metrics = c("sensitivity", "specificity", "precision", "f1_score"),
                               conf_level = 0.95) {
    # Verify input
    if (!is.list(test_results) || length(test_results) == 0) {
        stop("results must be a list of method results")
    }
    
    # Verify metrics parameters
    valid_metrics <- c("sensitivity", "specificity", "precision", "f1_score", "accuracy", "mcc")
    invalid_metrics <- setdiff(metrics, valid_metrics)
    if (length(invalid_metrics) > 0) {
        stop("should be one of: ", paste(valid_metrics, collapse = ", "))
    }
    
    # Computing Performance Metrics
    results <- lapply(test_results, function(pred) {
        # Verify input length
        if (length(pred) != length(true_status)) {
            stop("length of predicted and true status must match")
        }
        
        # Validate logical value
        if (!is.logical(pred) || !is.logical(true_status)) {
            stop("predicted status must be logical")
        }
        
        # Calculate the confusion matrix
        tp <- sum(pred & true_status)
        fp <- sum(pred & !true_status)
        tn <- sum(!pred & !true_status)
        fn <- sum(!pred & true_status)
        
        # Calculation of basic indicators
        sens <- if (tp + fn == 0) 0 else tp / (tp + fn)
        spec <- if (tn + fp == 0) 0 else tn / (tn + fp)
        prec <- if (tp + fp == 0) 0 else tp / (tp + fp)
        
        # Calculate F1 score
        f1 <- if (prec + sens == 0) 0 else 2 * (prec * sens) / (prec + sens)
        
        # Calculation accuracy
        acc <- (tp + tn) / (tp + tn + fp + fn)
        
        # Calculate MCC
        mcc_num <- (tp * tn - fp * fn)
        mcc_den <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
        mcc <- if (mcc_den == 0) 0 else mcc_num / mcc_den
        
        # Calculate confidence intervals
        sens_ci <- if (tp + fn == 0) c(0, 0) else 
                   as.numeric(binom.test(tp, tp + fn, conf.level = conf_level)$conf.int)
        spec_ci <- if (tn + fp == 0) c(0, 0) else 
                   as.numeric(binom.test(tn, tn + fp, conf.level = conf_level)$conf.int)
        prec_ci <- if (tp + fp == 0) c(0, 0) else 
                   as.numeric(binom.test(tp, tp + fp, conf.level = conf_level)$conf.int)
        
        # Indicators of return requests
        metrics_values <- list(
            sensitivity = as.numeric(sens),
            specificity = as.numeric(spec),
            precision = as.numeric(prec),
            f1_score = as.numeric(f1),
            accuracy = as.numeric(acc),
            mcc = as.numeric(mcc),
            sensitivity_ci_lower = as.numeric(sens_ci[1]),
            sensitivity_ci_upper = as.numeric(sens_ci[2]),
            specificity_ci_lower = as.numeric(spec_ci[1]),
            specificity_ci_upper = as.numeric(spec_ci[2]),
            precision_ci_lower = as.numeric(prec_ci[1]),
            precision_ci_upper = as.numeric(prec_ci[2])
        )
        
        # Return only the requested metrics
        return(metrics_values[c(metrics, 
                              paste0(intersect(c("sensitivity", "specificity", "precision"), metrics), "_ci_lower"),
                              paste0(intersect(c("sensitivity", "specificity", "precision"), metrics), "_ci_upper"))])
    })
    
    # Convert to data frame
    result_df <- do.call(rbind, results)
    result_df <- as.data.frame(result_df, stringsAsFactors = FALSE)
    result_df$method <- names(test_results)
    
    # Add the rankings first, because at this point all values are of numeric type
    rank_metrics <- intersect(c("sensitivity", "specificity", "precision"), metrics)
    for (metric in rank_metrics) {
        if (metric %in% names(result_df)) {
            rank_col <- paste0(metric, "_rank")
            result_df[[rank_col]] <- rank(-as.numeric(result_df[[metric]]), ties.method = "min")
        }
    }
    
    # Handling ranges of numeric columns
    numeric_cols <- setdiff(names(result_df), "method")
    for (col in numeric_cols) {
        if (!is.null(result_df[[col]]) && length(result_df[[col]]) > 0) {
            # Conversion to numeric values
            result_df[[col]] <- as.numeric(result_df[[col]])
            
            # Scope based on listing
            if (!grepl("_rank$", col)) {  
                if (col == "mcc") {
                    # MCC ranges from -1 to 1
                    result_df[[col]] <- pmin(pmax(result_df[[col]], -1), 1)
                } else {
                    # All other metrics (including confidence intervals) are between 0 and 1.
                    result_df[[col]] <- pmin(pmax(result_df[[col]], 0), 1)
                }
            }
        }
    }
    
    # Add Class
    class(result_df) <- c("daa_performance", "data.frame")
    
    return(result_df)
}

#' Print method for daa_performance objects
#' @param x daa_performance object
#' @param ... Additional arguments
#' @export
print.daa_performance <- function(x, ...) {
    cat("DAA Method Performance Evaluation\n")
    cat("===============================\n")
    NextMethod()
}

#' Plot method for daa_performance objects
#' @param x A daa_performance object
#' @param metric Performance metric to plot ("sensitivity", "specificity", "precision", "f1_score")
#' @param colors Vector of colors for bars and points (default: c("#2C3E50", "#E74C3C", "#3498DB", "#2ECC71"))
#' @param base_size Base font size (default: 12)
#' @param width Bar width (default: 0.6)
#' @param conf_level Confidence level for intervals (default: 0.95)
#' @param ... Additional arguments passed to plotting functions
#' @export
plot.daa_performance <- function(x, metric = "sensitivity", 
                               colors = c("#2C3E50", "#E74C3C", "#3498DB", "#2ECC71"),
                               base_size = 12,
                               width = 0.6,
                               conf_level = 0.95,
                               ...) {
    # Validate metric parameter
    valid_metrics <- c("sensitivity", "specificity", "precision", "f1_score")
    if (!metric %in% valid_metrics) {
        stop(sprintf("Invalid metric. Must be one of: %s", 
             paste(valid_metrics, collapse = ", ")))
    }
    
    # Create plot data
    plot_data <- data.frame(
        Method = x$method,
        Value = x[[metric]],
        stringsAsFactors = FALSE
    )
    
    # Define metric labels
    metric_labels <- c(
        sensitivity = "Sensitivity",
        specificity = "Specificity",
        precision = "Precision",
        f1_score = "F1 Score"
    )
    
    # Create enhanced plot
    p <- ggplot2::ggplot(plot_data, 
                        ggplot2::aes(x = stats::reorder(.data$Method, .data$Value),
                                   y = .data$Value)) +
        # Add bars
        ggplot2::geom_bar(stat = "identity", 
                         fill = colors[1],
                         alpha = 0.8,
                         width = width) +
        # Add points
        ggplot2::geom_point(size = 3, 
                           color = colors[2]) +
        # Add confidence intervals if available
        {
            ci_lower <- paste0(metric, "_ci_lower")
            ci_upper <- paste0(metric, "_ci_upper")
            if (all(c(ci_lower, ci_upper) %in% names(x))) {
                ggplot2::geom_errorbar(
                    ggplot2::aes(ymin = x[[ci_lower]], 
                                ymax = x[[ci_upper]]),
                    width = width/2,
                    color = colors[2],
                    linewidth = 0.8
                )
            }
        } +
        # Customize labels and theme
        ggplot2::labs(
            title = NULL,
            x = NULL,
            y = metric_labels[metric],
            caption = if(any(grepl("_ci_", names(x)))) {
                sprintf("Error bars represent %.0f%% confidence intervals", 
                        conf_level * 100)
            } else {
                NULL
            }
        ) +
        # Scale y axis from 0 to 1
        ggplot2::scale_y_continuous(
            limits = c(0, 1),
            breaks = seq(0, 1, 0.2),
            labels = scales::percent_format(accuracy = 1)
        ) +
        # Custom theme
        ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, 
                                              hjust = 1,
                                              face = "bold"),
            axis.text.y = ggplot2::element_text(face = "bold"),
            axis.title.y = ggplot2::element_text(face = "bold",
                                               margin = ggplot2::margin(r = 10)),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.border = ggplot2::element_rect(fill = NA, 
                                               color = "black",
                                               linewidth = 0.5),
            plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10)
        )
    
    return(p)
} 