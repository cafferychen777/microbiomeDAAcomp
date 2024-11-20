#' Run Multiple DAA Methods
#'
#' @title Execute Multiple Differential Abundance Analysis Methods
#' @description Run multiple differential abundance analysis (DAA) methods on microbiome data
#' and return standardized results for comparison. Supports multiple popular methods including
#' DESeq2, ALDEx2, and ANCOM-BC.
#'
#' @param data A list containing count matrix and group information, or a phyloseq object
#' @param methods Character vector of method names to run. Options:
#'        \itemize{
#'          \item "DESeq2": Differential expression analysis based on negative binomial distribution
#'          \item "ALDEx2": ANOVA-like differential expression procedure
#'          \item "ANCOM-BC": Analysis of composition of microbiomes with bias correction
#'        }
#' @param count_matrix Optional matrix of counts if not providing full data object.
#'        Rows represent features (e.g., taxa), columns represent samples
#' @param group_info Optional factor of group assignments if not providing full data object.
#'        Must match the order of samples in count_matrix
#' @param alpha Significance level for differential abundance calling (default: 0.05)
#' @param p_adjust_method Method for p-value adjustment. Options include:
#'        "BH" (default), "bonferroni", "holm", "hochberg", "BY"
#' @param ... Additional arguments passed to individual methods
#'
#' @return A list containing:
#'         \itemize{
#'           \item results: List of results from each method
#'           \item summary: Data frame comparing results across methods
#'           \item runtime: Vector of computation times for each method
#'           \item parameters: List of parameters used for each method
#'         }
#'
#' @details
#' The function handles:
#' \itemize{
#'   \item Input validation and data preprocessing
#'   \item Method-specific parameter optimization
#'   \item Error handling and warnings
#'   \item Standardization of results across methods
#'   \item Performance timing
#' }
#'
#' Common use cases include:
#' \itemize{
#'   \item Comparing results across multiple DAA methods
#'   \item Benchmarking method performance
#'   \item Ensemble analysis of differential abundance
#' }
#'
#' @examples
#' # Example 1: Basic usage with simulated data
#' set.seed(123)
#' n_features <- 100
#' n_samples <- 20
#' 
#' # Generate baseline counts
#' counts <- matrix(rnbinom(n_features * n_samples, 
#'                         size = 3, 
#'                         prob = 0.5), 
#'                 nrow = n_features)
#' 
#' # Create clear group differences
#' group_labels <- factor(rep(c("Control", "Treatment"), each = n_samples/2))
#' diff_features <- 1:20  # First 20 features are differential
#' fold_change <- 5
#' counts[diff_features, group_labels == "Treatment"] <- 
#'     counts[diff_features, group_labels == "Treatment"] * fold_change
#' 
#' # Add informative names
#' rownames(counts) <- paste0("Feature", 1:n_features)
#' colnames(counts) <- paste0("Sample", 1:n_samples)
#' 
#' # Example 2: Basic method comparison
#' results <- run_daa_methods(
#'   count_matrix = counts,
#'   group_info = group_labels,
#'   methods = c("DESeq2", "ALDEx2"),
#'   alpha = 0.05
#' )
#' 
#' # Example 3: Examining results
#' # View summary statistics
#' print(results$summary)
#' 
#' # Check runtime performance
#' print(results$runtime)
#' 
#' # Access method-specific results
#' head(results$results$DESeq2)
#' 
#' # Example 4: Visualization of results
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   # Prepare performance comparison data
#'   plot_data <- data.frame(
#'     Method = names(results$results),
#'     Significant = sapply(results$results, 
#'                         function(x) sum(x$padj < 0.05, na.rm = TRUE)),
#'     Runtime = results$runtime
#'   )
#'   
#'   # Create publication-ready plot
#'   p <- ggplot2::ggplot(plot_data, 
#'                        ggplot2::aes(x = Method, 
#'                                    y = Significant, 
#'                                    fill = Method)) +
#'     ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
#'     ggplot2::theme_minimal() +
#'     ggplot2::labs(
#'       title = "Comparison of DAA Methods",
#'       subtitle = paste("Fold change =", fold_change,
#'                       "| True positives =", length(diff_features)),
#'       y = "Number of Significant Features",
#'       x = "Method"
#'     ) +
#'     ggplot2::theme(
#'       axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
#'       plot.title = ggplot2::element_text(face = "bold"),
#'       legend.position = "none"
#'     )
#'   
#'   print(p)
#' }
#' 
#' # Example 5: Advanced usage with custom parameters
#' \dontrun{
#'   results_custom <- run_daa_methods(
#'     count_matrix = counts,
#'     group_info = group_labels,
#'     methods = c("DESeq2", "ALDEx2", "ANCOM-BC"),
#'     alpha = 0.01,
#'     p_adjust_method = "bonferroni"
#'   )
#'   
#'   # Compare results with different parameters
#'   print(results_custom$summary)
#' }
#'
#' @seealso 
#' \itemize{
#'   \item \code{\link[DESeq2]{DESeq}} for DESeq2 method details
#'   \item \code{\link[ALDEx2]{aldex}} for ALDEx2 method details
#'   \item \code{\link[ANCOMBC]{ancombc}} for ANCOM-BC method details
#' }
#'
#' @references 
#' \itemize{
#'   \item Love, M.I., et al. (2014) Moderated estimation of fold change and 
#'         dispersion for RNA-seq data with DESeq2. Genome Biology, 15, 550.
#'   \item Fernandes, A.D., et al. (2014) Anova-like differential expression 
#'         (ALDEx) analysis for mixed population RNA-seq. PLoS ONE, 9, e89731.
#' }
#'
#' @importFrom DESeq2 DESeqDataSetFromMatrix DESeq results
#' @importFrom ALDEx2 aldex aldex.clr
#' @importFrom ANCOMBC ancombc
#' @importFrom parallel mclapply
#' @importFrom phyloseq otu_table sample_data
#'
#' @export
run_daa_methods <- function(data = NULL, count_matrix = NULL, group_info = NULL,
                          methods = c("DESeq2", "ALDEx2", "ANCOM-BC"),
                          alpha = 0.05, p_adjust_method = "BH", ...) {
    # Input validation and data extraction
    if (is.null(data) && (is.null(count_matrix) || is.null(group_info))) {
        stop("Must provide either 'data' object or both 'count_matrix' and 'group_info'")
    }

    if (!is.null(data)) {
        if (inherits(data, "phyloseq")) {
            count_matrix <- as.matrix(phyloseq::otu_table(data))
            group_info <- phyloseq::sample_data(data)[[1]]
        } else if (is.list(data)) {
            count_matrix <- data$counts
            group_info <- data$group_info
        }
    }

    # 验证方法参数
    valid_methods <- c("DESeq2", "ALDEx2", "ANCOM-BC")
    methods <- match.arg(arg = methods, choices = valid_methods, several.ok = TRUE)

    # 初始化结果列表
    results <- list()
    for (m in methods) {
        results[[m]] <- NULL
    }

    # 初始化运行时间向量
    runtime <- numeric(length(methods))
    names(runtime) <- methods

    # 运行每个方法
    for (method in methods) {
        start_time <- Sys.time()

        tryCatch({
            method_result <- switch(method,
                "DESeq2" = run_deseq2(count_matrix, group_info, alpha, p_adjust_method, ...),
                "ALDEx2" = run_aldex2(count_matrix, group_info, alpha, p_adjust_method, ...),
                "ANCOM-BC" = run_ancombc(count_matrix, group_info, alpha, ...)
            )
            if (is.null(method_result)) {
                warning(paste(method, "returned NULL result"))
            }
            results[[method]] <- method_result
        }, error = function(e) {
            warning(paste(method, "error:", e$message))
            # 确保错误时也保持 NULL 值
            results[[method]] <- NULL
        })

        runtime[method] <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    }

    # 确保结果列表有正确的名称
    if (length(results) == 0) {
        results <- setNames(vector("list", length(methods)), methods)
    }

    # 创建结果摘要
    summary <- create_summary(results, methods)

    # 返回结果
    list(
        results = results,
        summary = summary,
        runtime = runtime,
        parameters = list(
            alpha = alpha,
            p_adjust_method = p_adjust_method
        )
    )
}

#' Run DESeq2 Analysis
#'
#' @param counts Count matrix with features as rows and samples as columns
#' @param groups Factor or vector specifying group assignments for samples
#' @param alpha Significance level for differential abundance testing
#' @param p_adjust_method Method for p-value adjustment
#' @param ... Additional arguments passed to DESeq2 functions
#'
#' @return A data frame containing DESeq2 results with columns:
#'         baseMean, log2FoldChange, lfcSE, stat, pvalue, padj,
#'         feature, and significant
#'
#' @keywords internal
run_deseq2 <- function(counts, groups, alpha = 0.05, p_adjust_method = "BH", ...) {
    # 预处理数据：添加小的伪计数以避免零值问题
    counts <- counts + 1

    # 检查数据有效性
    if (all(counts == counts[1,1])) {
        warning("All counts are identical, DESeq2 analysis may not be meaningful")
        return(NULL)
    }

    # 创建 DESeqDataSet 对象并运行分析
    tryCatch({
        dds <- DESeq2::DESeqDataSetFromMatrix(
            countData = round(counts),
            colData = data.frame(group = factor(groups)),
            design = ~ group
        )

        dds <- DESeq2::DESeq(dds, quiet = TRUE, fitType = "local")
        res <- DESeq2::results(dds, alpha = alpha, pAdjustMethod = p_adjust_method)

        # 转换为数据框并添加必要的列
        result_df <- as.data.frame(res)
        result_df$feature <- rownames(result_df)
        result_df$significant <- result_df$padj < alpha

        return(result_df)
    }, error = function(e) {
        warning(paste("DESeq2 error:", e$message))
        return(NULL)
    })
}

#' Run ALDEx2 Analysis
#'
#' @param counts Count matrix with features as rows and samples as columns
#' @param groups Factor or vector specifying group assignments for samples
#' @param alpha Significance level for differential abundance testing
#' @param p_adjust_method Method for p-value adjustment
#' @param ... Additional arguments passed to ALDEx2 functions
#'
#' @return A data frame containing ALDEx2 results including effect sizes
#'         and statistical test results
#'
#' @keywords internal
run_aldex2 <- function(counts, groups, alpha, p_adjust_method, ...) {
    # 确保 groups 是字符向量
    groups <- as.character(groups)

    tryCatch({
        ald <- ALDEx2::aldex(as.matrix(counts), groups, test = "t", effect = TRUE)
        return(as.data.frame(ald))
    }, error = function(e) {
        warning(paste("ALDEx2 error:", e$message))
        return(NULL)
    })
}

#' Run ANCOM-BC Analysis
#'
#' @param counts Count matrix with features as rows and samples as columns
#' @param groups Factor or vector specifying group assignments for samples
#' @param alpha Significance level for differential abundance testing
#' @param ... Additional arguments passed to ANCOM-BC functions
#'
#' @return A list containing ANCOM-BC results including bias-corrected
#'         abundance differences and statistical significance
#'
#' @keywords internal
run_ancombc <- function(counts, groups, alpha, ...) {
    ancombc_out <- ANCOMBC::ancombc(
        phyloseq::otu_table(counts, taxa_are_rows = TRUE),
        phyloseq::sample_data(data.frame(group = groups)),
        formula = "group",
        p_adj_method = p_adjust_method
    )
    return(ancombc_out$res)
}

#' Create Summary of DAA Results
#'
#' @param results List of results from different DAA methods
#' @param methods Character vector of method names
#'
#' @return A data frame with columns:
#'         \itemize{
#'           \item Method: Name of the DAA method
#'           \item N_Significant: Number of features called as significant
#'           \item Mean_Effect: Mean absolute effect size
#'         }
#'
#' @keywords internal
create_summary <- function(results, methods) {
    # 如果所有结果都为 NULL，返回空的摘要
    if (all(sapply(results, is.null))) {
        return(data.frame(
            Method = methods,
            N_Significant = 0,
            Mean_Effect = 0,
            stringsAsFactors = FALSE
        ))
    }

    data.frame(
        Method = methods,
        N_Significant = sapply(results, function(x) {
            if (is.null(x) || !("padj" %in% names(x))) return(0)
            sum(x$padj < 0.05, na.rm = TRUE)
        }),
        Mean_Effect = sapply(results, function(x) {
            if (is.null(x) || !("log2FoldChange" %in% names(x))) return(0)
            mean(abs(x$log2FoldChange), na.rm = TRUE)
        }),
        stringsAsFactors = FALSE
    )
}
