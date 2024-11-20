#' Perform Sensitivity Analysis for DAA Methods
#' 
#' @description
#' Conducts sensitivity analysis for differential abundance analysis (DAA) methods
#' by systematically varying input parameters and evaluating their impact on results.
#' 
#' @param data Microbiome data object containing count matrix and metadata
#' @param parameters List of parameters to vary, where:
#'        \itemize{
#'          \item Names are parameter names (e.g., "alpha", "min_count")
#'          \item Values are vectors of parameter values to test
#'        }
#' @param method_names Character vector of DAA methods to analyze
#'
#' @return A list of class "daa_sensitivity" containing:
#'         \itemize{
#'           \item results: Nested list of sensitivity results for each method
#'           \item parameter_effects: Data frame summarizing parameter effects
#'           \item overlap_ratios: Matrix of result overlap between parameter values
#'           \item effect_sizes: Numeric vector of effect sizes for parameter changes
#'         }
#'
#' @details
#' The function evaluates:
#' - Changes in number of significant features
#' - Result overlap between parameter values
#' - Effect sizes of parameter variations
#' - Stability of results across parameter ranges
#'
#' For each parameter and method combination, the function:
#' 1. Establishes baseline results using first parameter value
#' 2. Compares results from other parameter values to baseline
#' 3. Calculates overlap ratios and effect sizes
#' 4. Aggregates results for easy interpretation
#'
#' @seealso 
#' \code{\link{run_daa_methods}} for running individual DAA methods
#'
#' @export
#'
#' @examples
#' # Create sample data
#' set.seed(123)
#' counts <- matrix(
#'   rpois(100 * 20, lambda = 10),
#'   nrow = 100,
#'   ncol = 20,
#'   dimnames = list(
#'     paste0("Feature", 1:100),
#'     paste0("Sample", 1:20)
#'   )
#' )
#' 
#' metadata <- data.frame(
#'   group = factor(rep(c("Control", "Treatment"), each = 10)),
#'   row.names = colnames(counts)
#' )
#' 
#' microbiome_data <- list(
#'   counts = counts,
#'   metadata = metadata
#' )
#' 
#' # Run sensitivity analysis
#' sensitivity_results <- sensitivity_analysis(
#'   data = microbiome_data,
#'   parameters = list(
#'     alpha = c(0.01, 0.05, 0.1),
#'     min_count = c(5, 10, 20)
#'   ),
#'   method_names = c("DESeq2", "ALDEx2")
#' )
#' 
#' # Access results
#' str(sensitivity_results)
#' 
#' # Get parameter effects for a specific method
#' deseq2_alpha_results <- sensitivity_results$DESeq2$alpha
#' print(deseq2_alpha_results)
sensitivity_analysis <- function(data, parameters, method_names) {
    # Input validation
    if (!is.list(parameters)) {
        stop("parameters must be a list")
    }
    
    if (!is.character(method_names)) {
        stop("method_names must be a character vector")
    }
    
    if (length(parameters) == 0) {
        return(structure(
            list(),
            class = c("daa_sensitivity", "list")
        ))
    }
    
    # Initialize results list
    results <- list()
    
    # For each method
    for (method in method_names) {
        method_results <- list()
        
        # For each parameter
        for (param_name in names(parameters)) {
            param_values <- parameters[[param_name]]
            
            # 处理空参数值的情况
            if (length(param_values) == 0) {
                method_results[[param_name]] <- data.frame(
                    parameter_value = numeric(0),
                    n_significant = numeric(0),
                    overlap_ratio = numeric(0),
                    effect_size = numeric(0),
                    stringsAsFactors = FALSE,
                    row.names = character(0)  # 确保行名也是空的
                )
                next
            }
            
            # 创建结果数据框
            param_results <- data.frame(
                parameter_value = param_values,
                n_significant = rep(NA, length(param_values)),
                overlap_ratio = rep(NA, length(param_values)),
                effect_size = rep(NA, length(param_values)),
                stringsAsFactors = FALSE
            )
            
            # 只有在参数值非空时才执行分析
            if (length(param_values) > 0) {
                # Store baseline results
                baseline_params <- list()
                baseline_params[[param_name]] <- param_values[1]
                baseline_results <- try(
                    run_daa_method(data, method = method, params = baseline_params),
                    silent = TRUE
                )
                
                if (!inherits(baseline_results, "try-error")) {
                    baseline_sig <- get_significant_features(baseline_results)
                    
                    # Test each parameter value
                    for (i in seq_along(param_values)) {
                        current_params <- list()
                        current_params[[param_name]] <- param_values[i]
                        
                        current_results <- try(
                            run_daa_method(data, method = method, params = current_params),
                            silent = TRUE
                        )
                        
                        if (!inherits(current_results, "try-error")) {
                            current_sig <- get_significant_features(current_results)
                            
                            param_results$n_significant[i] <- length(current_sig)
                            param_results$overlap_ratio[i] <- calculate_overlap_ratio(
                                baseline_sig, 
                                current_sig
                            )
                            param_results$effect_size[i] <- calculate_effect_size(
                                baseline_results,
                                current_results
                            )
                        }
                    }
                }
            }
            
            method_results[[param_name]] <- param_results
        }
        
        results[[method]] <- method_results
    }
    
    # 确保返回值的类型正确
    structure(results, class = c("daa_sensitivity", "list"))
}

#' Calculate overlap ratio between two sets of significant features
#' @param set1 First set of feature names
#' @param set2 Second set of feature names
#' @return Numeric value between 0 and 1
calculate_overlap_ratio <- function(set1, set2) {
    if (length(set1) == 0 || length(set2) == 0) return(0)
    intersection <- length(intersect(set1, set2))
    union <- length(union(set1, set2))
    return(intersection / union)
}

#' Calculate effect size between two analysis results
#' @param result1 First analysis result
#' @param result2 Second analysis result
#' @return Numeric value representing effect size
calculate_effect_size <- function(result1, result2) {
    # 如果输入是测试用例中的简单列表格式
    if (!is.null(result1$test_statistics) && !is.null(result2$test_statistics)) {
        stats1 <- result1$test_statistics
        stats2 <- result2$test_statistics
    } else {
        # 否则使用通用的提取函数
        stats1 <- get_test_statistics(result1)
        stats2 <- get_test_statistics(result2)
    }
    
    # 输入验证
    if (is.null(stats1) || is.null(stats2)) {
        return(NA_real_)
    }
    
    if (length(stats1) == 0 || length(stats2) == 0) {
        return(NA_real_)
    }
    
    # 确保两个向量长度相同
    if (length(stats1) != length(stats2)) {
        return(NA_real_)
    }
    
    # 移除 NA 值
    valid_indices <- !is.na(stats1) & !is.na(stats2)
    if (!any(valid_indices)) {
        return(NA_real_)
    }
    
    stats1 <- stats1[valid_indices]
    stats2 <- stats2[valid_indices]
    
    # 计算相关性
    if (length(unique(stats1)) == 1 || length(unique(stats2)) == 1) {
        return(NA_real_)
    }
    
    cor_val <- try(cor(stats1, stats2, method = "spearman"), silent = TRUE)
    
    if (inherits(cor_val, "try-error")) {
        return(NA_real_)
    }
    
    return(cor_val)
}

#' Get significant features from DAA results
#' @param results DAA analysis results
#' @return Character vector of significant feature names
get_significant_features <- function(results) {
    if (is.null(results) || length(results) == 0) {
        return(character(0))
    }
    # 根据结果类型返回显著特征
    if ("padj" %in% names(results)) {
        return(names(results$padj[results$padj < 0.05]))
    } else if ("significant" %in% names(results)) {
        return(names(results$significant[results$significant]))
    }
    return(character(0))
}

#' Get test statistics from DAA results
#' @param results DAA analysis results
#' @return Numeric vector of test statistics
get_test_statistics <- function(results) {
    if (is.null(results) || length(results) == 0) {
        return(numeric(0))
    }
    
    # 根据结果类型返回检验统计量
    if ("stat" %in% names(results)) {
        stats <- results$stat
    } else if ("statistic" %in% names(results)) {
        stats <- results$statistic
    } else if ("W" %in% names(results)) {
        stats <- results$W
    } else {
        return(numeric(0))
    }
    
    # 确保返回数值向量
    stats <- as.numeric(stats)
    if (any(is.na(stats))) {
        return(numeric(0))
    }
    
    return(stats)
} 