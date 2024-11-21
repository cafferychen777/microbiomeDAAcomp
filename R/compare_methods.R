#' Compare Different DAA Methods
#' 
#' This function performs comparative analysis of different differential abundance analysis (DAA) methods
#' using various approaches including statistical tests and visualizations.
#' 
#' @param performance_results Data frame containing performance metrics for different DAA methods.
#'        Must include columns: method, accuracy, precision, recall, f1_score
#' @param comparison_type Type of comparison to perform:
#'        \itemize{
#'          \item "statistical": Performs Friedman test and post-hoc analysis
#'          \item "visual": Creates performance visualization plots
#'          \item "comprehensive": Combines both statistical and visual analyses
#'        }
#' @param ... Additional arguments passed to internal functions
#'
#' @return A list of class "daa_comparison" containing:
#'         \itemize{
#'           \item statistical: Results from Friedman test and post-hoc analysis
#'           \item visual: ggplot objects showing performance comparisons
#'           \item summary: Summary statistics for each method
#'         }
#' @export
#'
#' @examples
#' \dontrun{
#' # Create sample performance results
#' perf_data <- data.frame(
#'   method = rep(c("method1", "method2"), each = 10),
#'   accuracy = runif(20, 0.8, 0.95),
#'   precision = runif(20, 0.8, 0.95),
#'   recall = runif(20, 0.8, 0.95),
#'   f1_score = runif(20, 0.8, 0.95)
#' )
#' 
#' # Run comprehensive comparison
#' results <- compare_methods(perf_data, comparison_type = "comprehensive")
#' 
#' # Run only statistical comparison
#' stat_results <- compare_methods(perf_data, comparison_type = "statistical")
#' }
compare_methods <- function(performance_results, 
                          comparison_type = c("statistical", "visual", "comprehensive"),
                          ...) {
    comparison_type <- match.arg(comparison_type)
    
    # 1. 首先验证输入类型
    if (!is.data.frame(performance_results)) {
        stop("performance_results must be a data frame")
    }
    
    # 2. 然后验证数据是否为空
    if (nrow(performance_results) == 0) {
        stop("performance_results must contain data")
    }
    
    # 3. 验证必需的列
    required_cols <- c("method", "accuracy", "precision", "recall", "f1_score")
    if (!all(required_cols %in% colnames(performance_results))) {
        stop("performance_results must contain columns: ", 
             paste(required_cols, collapse = ", "))
    }
    
    # 初始化结果列表
    results <- list(
        statistical = NULL,
        visual = NULL,
        summary = NULL
    )
    
    # 执行统计比较
    if (comparison_type %in% c("statistical", "comprehensive")) {
        n_methods <- length(unique(performance_results$method))
        if (n_methods < 2) {
            stop("groups must have more than one level")
        }
        
        # 准备数据用于 Friedman 测试
        performance_matrix <- matrix(
            performance_results$accuracy,
            ncol = n_methods,
            byrow = TRUE
        )
        colnames(performance_matrix) <- unique(performance_results$method)
        rownames(performance_matrix) <- seq_len(nrow(performance_matrix))
        
        # 执行 Friedman 测试
        tryCatch({
            friedman_test <- stats::friedman.test(performance_matrix)
            posthoc <- PMCMRplus::frdAllPairsNemenyiTest(performance_matrix)
            
            results$statistical <- list(
                friedman = friedman_test,
                posthoc = posthoc
            )
        }, error = function(e) {
            warning("Failed to perform statistical tests: ", e$message)
            results$statistical <- NULL
        })
    }
    
    # 创建可视化
    if (comparison_type %in% c("visual", "comprehensive")) {
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
    
    # 添加汇总统计
    grouped_data <- dplyr::group_by(performance_results, method)
    results$summary <- dplyr::summarise(grouped_data,
        mean_accuracy = mean(accuracy),
        sd_accuracy = sd(accuracy),
        mean_precision = mean(precision),
        mean_recall = mean(recall),
        mean_f1 = mean(f1_score)
    )
    
    # 根据比较类型过滤结果
    if (comparison_type == "statistical") {
        results$visual <- NULL
    } else if (comparison_type == "visual") {
        results$statistical <- NULL
    }
    
    class(results) <- c("daa_comparison", "list")
    attr(results, "print.function") <- print.daa_comparison
    return(results)
}

#' Print Method for DAA Comparison Objects
#'
#' @param x A daa_comparison object to print
#' @param ... Additional arguments passed to print methods
#'
#' @return Invisibly returns the input object
#'
#' @examples
#' \dontrun{
#' comparison_results <- compare_methods(performance_data)
#' print(comparison_results)
#' }
#'
#' @export
print.daa_comparison <- function(x, ...) {
    cat("DAA Method Comparison Results\n")
    cat("============================\n\n")
    
    if (!is.null(x$summary)) {
        cat("Performance Summary:\n")
        print(x$summary)
        cat("\n")
    }
    
    cat("Statistical Test Results:\n")
    if (!is.null(x$statistical) && !is.null(x$statistical$friedman)) {
        print(x$statistical$friedman)
        if (!is.null(x$statistical$posthoc)) {
            cat("\nPost-hoc Analysis:\n")
            print(x$statistical$posthoc)
        }
    } else {
        cat("No statistical test results available\n")
    }
    cat("\n")
    
    invisible(x)
} 