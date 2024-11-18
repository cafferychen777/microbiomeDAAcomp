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
    results$summary <- performance_results %>%
        dplyr::group_by(method) %>%
        dplyr::summarise(
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

# 修改打印方法
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