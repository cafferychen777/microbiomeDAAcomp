#' Evaluate DAA Method Performance
#' 
#' @param results List of results from different DAA methods
#' @param true_status Vector of true differential abundance status (for simulated data)
#' @param metrics Character vector of performance metrics to calculate
#'
#' @return Data frame containing performance metrics for each method
#' @export
#'
#' @examples
#' \dontrun{
#' performance <- evaluate_performance(results, true_status, 
#'                                   metrics = c("sensitivity", "specificity"))
#' }
evaluate_performance <- function(results, 
                               true_status, 
                               metrics = c("sensitivity", "specificity", "precision")) {
    # Input validation
    if (!is.list(results)) {
        stop("results must be a list of method results")
    }
    if (!is.vector(true_status)) {
        stop("true_status must be a vector")
    }
    
    # Validate metrics
    valid_metrics <- c("sensitivity", "specificity", "precision", 
                      "accuracy", "f1_score", "npv", "mcc")
    metrics <- match.arg(metrics, valid_metrics, several.ok = TRUE)
    
    # Initialize results data frame
    performance_df <- data.frame(method = names(results))
    
    # Calculate performance metrics for each method
    for (method_name in names(results)) {
        predicted_status <- results[[method_name]]
        
        # Create confusion matrix
        confusion <- table(
            Predicted = predicted_status,
            Actual = true_status
        )
        
        # Extract confusion matrix elements
        TP <- confusion["TRUE", "TRUE"]
        TN <- confusion["FALSE", "FALSE"]
        FP <- confusion["TRUE", "FALSE"]
        FN <- confusion["FALSE", "TRUE"]
        
        # Calculate requested metrics
        for (metric in metrics) {
            performance_df[performance_df$method == method_name, metric] <- 
                switch(metric,
                    "sensitivity" = TP / (TP + FN),
                    "specificity" = TN / (TN + FP),
                    "precision" = TP / (TP + FP),
                    "accuracy" = (TP + TN) / (TP + TN + FP + FN),
                    "f1_score" = 2 * (TP / (TP + FP) * TP / (TP + FN)) / 
                                (TP / (TP + FP) + TP / (TP + FN)),
                    "npv" = TN / (TN + FN),  # Negative Predictive Value
                    "mcc" = (TP * TN - FP * FN) / 
                           sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))  # Matthews Correlation Coefficient
                )
        }
    }
    
    # Add confidence intervals
    if (length(true_status) >= 30) {  # Only if sample size is sufficient
        for (metric in metrics) {
            ci_lower <- paste0(metric, "_ci_lower")
            ci_upper <- paste0(metric, "_ci_upper")
            
            performance_df[, ci_lower] <- NA
            performance_df[, ci_upper] <- NA
            
            for (i in 1:nrow(performance_df)) {
                # Calculate Wilson score interval
                p <- performance_df[i, metric]
                n <- length(true_status)
                z <- 1.96  # 95% confidence level
                
                denominator <- 1 + z^2/n
                center <- (p + z^2/(2*n))/denominator
                interval <- z * sqrt(p*(1-p)/n + z^2/(4*n^2))/denominator
                
                performance_df[i, ci_lower] <- max(0, center - interval)
                performance_df[i, ci_upper] <- min(1, center + interval)
            }
        }
    }
    
    # Add method ranks
    if (nrow(performance_df) > 1) {
        for (metric in metrics) {
            rank_col <- paste0(metric, "_rank")
            performance_df[, rank_col] <- rank(-performance_df[, metric])
        }
    }
    
    class(performance_df) <- c("daa_performance", "data.frame")
    return(performance_df)
}

# Add print method for daa_performance class
#' @export
print.daa_performance <- function(x, ...) {
    cat("DAA Method Performance Evaluation\n")
    cat("===============================\n\n")
    
    # Round numeric columns to 3 decimal places
    numeric_cols <- sapply(x, is.numeric)
    x[numeric_cols] <- round(x[numeric_cols], 3)
    
    print.data.frame(x)
}

# Add plot method for daa_performance class
#' @export
plot.daa_performance <- function(x, metric = NULL, ...) {
    if (is.null(metric)) {
        metric <- names(x)[2]  # Use first metric by default
    }
    
    if (!metric %in% names(x)) {
        stop("Specified metric not found in performance results")
    }
    
    # Create performance plot
    ggplot2::ggplot(x, ggplot2::aes(x = method, y = .data[[metric]], fill = method)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::geom_errorbar(
            ggplot2::aes(
                ymin = .data[[paste0(metric, "_ci_lower")]],
                ymax = .data[[paste0(metric, "_ci_upper")]]
            ),
            width = 0.2
        ) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            title = paste("Performance Comparison -", metric),
            y = metric,
            x = "Method"
        ) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
} 