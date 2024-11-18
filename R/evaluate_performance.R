#' Evaluate DAA method performance
#' @param results List of method results
#' @param true_status True differential abundance status
#' @param metrics Optional vector of metrics to calculate
#' @param conf_level Confidence level for intervals (default: NULL)
#' @return S3 class object of type 'daa_performance'
evaluate_performance <- function(test_results, true_status, metrics = c("sensitivity", "specificity", "precision", "f1_score")) {
    # 验证输入
    if (!is.list(test_results) || length(test_results) == 0) {
        stop("results must be a list of method results")
    }
    
    # 验证 metrics 参数
    valid_metrics <- c("sensitivity", "specificity", "precision", "f1_score", "accuracy", "mcc")
    invalid_metrics <- setdiff(metrics, valid_metrics)
    if (length(invalid_metrics) > 0) {
        stop("should be one of: ", paste(valid_metrics, collapse = ", "))
    }
    
    # 计算性能指标
    results <- lapply(test_results, function(pred) {
        # 验证输入长度
        if (length(pred) != length(true_status)) {
            stop("length of predicted and true status must match")
        }
        
        # 验证逻辑值
        if (!is.logical(pred) || !is.logical(true_status)) {
            stop("predicted status must be logical")
        }
        
        # 计算混淆矩阵
        tp <- sum(pred & true_status)
        fp <- sum(pred & !true_status)
        tn <- sum(!pred & !true_status)
        fn <- sum(!pred & true_status)
        
        # 计算基本指标
        sens <- if (tp + fn == 0) 0 else tp / (tp + fn)
        spec <- if (tn + fp == 0) 0 else tn / (tn + fp)
        prec <- if (tp + fp == 0) 0 else tp / (tp + fp)
        
        # 计算 F1 分数
        f1 <- if (prec + sens == 0) 0 else 2 * (prec * sens) / (prec + sens)
        
        # 计算准确率
        acc <- (tp + tn) / (tp + tn + fp + fn)
        
        # 计算 MCC
        mcc_num <- (tp * tn - fp * fn)
        mcc_den <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
        mcc <- if (mcc_den == 0) 0 else mcc_num / mcc_den
        
        # 计算置信区间
        ci_level <- 0.95
        sens_ci <- if (tp + fn == 0) c(0, 0) else 
                   as.numeric(binom.test(tp, tp + fn, conf.level = ci_level)$conf.int)
        spec_ci <- if (tn + fp == 0) c(0, 0) else 
                   as.numeric(binom.test(tn, tn + fp, conf.level = ci_level)$conf.int)
        prec_ci <- if (tp + fp == 0) c(0, 0) else 
                   as.numeric(binom.test(tp, tp + fp, conf.level = ci_level)$conf.int)
        
        # 返回请求的指标
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
        
        # 只返回请求的指标
        return(metrics_values[c(metrics, 
                              paste0(intersect(c("sensitivity", "specificity", "precision"), metrics), "_ci_lower"),
                              paste0(intersect(c("sensitivity", "specificity", "precision"), metrics), "_ci_upper"))])
    })
    
    # 转换为数据框
    result_df <- do.call(rbind, results)
    result_df <- as.data.frame(result_df, stringsAsFactors = FALSE)
    result_df$method <- names(test_results)
    
    # 先添加排名，因为这时所有值都是数值类型
    rank_metrics <- intersect(c("sensitivity", "specificity", "precision"), metrics)
    for (metric in rank_metrics) {
        if (metric %in% names(result_df)) {
            rank_col <- paste0(metric, "_rank")
            result_df[[rank_col]] <- rank(-as.numeric(result_df[[metric]]), ties.method = "min")
        }
    }
    
    # 处理数值列的范围
    numeric_cols <- setdiff(names(result_df), "method")
    for (col in numeric_cols) {
        if (!is.null(result_df[[col]]) && length(result_df[[col]]) > 0) {
            # 转换为数值
            result_df[[col]] <- as.numeric(result_df[[col]])
            
            # 根据列名确定范围
            if (!grepl("_rank$", col)) {  # 不处理排名列
                if (col == "mcc") {
                    # MCC 在 -1 到 1 之间
                    result_df[[col]] <- pmin(pmax(result_df[[col]], -1), 1)
                } else {
                    # 其他所有指标（包括置信区间）都在 0 到 1 之间
                    result_df[[col]] <- pmin(pmax(result_df[[col]], 0), 1)
                }
            }
        }
    }
    
    # 添加类
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
#' @param ... Additional arguments passed to plotting functions
#' @export
plot.daa_performance <- function(x, metric = "sensitivity", ...) {
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
    
    # Create plot using ggplot2
    p <- ggplot2::ggplot(plot_data, 
                        ggplot2::aes(x = .data$Method, 
                                   y = .data$Value)) +
        ggplot2::geom_col(fill = "steelblue") +
        ggplot2::labs(title = paste("Method Performance -", metric),
                     y = metric) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    
    # Add confidence intervals if available
    ci_lower <- paste0(metric, "_ci_lower")
    ci_upper <- paste0(metric, "_ci_upper")
    if (all(c(ci_lower, ci_upper) %in% names(x))) {
        plot_data$CI_lower <- x[[ci_lower]]
        plot_data$CI_upper <- x[[ci_upper]]
        p <- p + ggplot2::geom_errorbar(
            ggplot2::aes(ymin = .data$CI_lower, 
                        ymax = .data$CI_upper),
            width = 0.2
        )
    }
    
    return(p)
} 