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
#' - Input validation and data preprocessing
#' - Method-specific parameter optimization
#' - Error handling and warnings
#' - Standardization of results across methods
#' - Performance timing
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Using phyloseq object
#' data(GlobalPatterns)
#' results <- run_daa_methods(
#'   data = GlobalPatterns,
#'   methods = c("DESeq2", "ALDEx2"),
#'   alpha = 0.05
#' )
#' 
#' # Using count matrix and group info
#' results <- run_daa_methods(
#'   count_matrix = counts,
#'   group_info = groups,
#'   methods = "DESeq2",
#'   p_adjust_method = "bonferroni"
#' )
#' 
#' # Access results
#' head(results$summary)
#' plot(results$runtime)
#' }
#' 
#' @importFrom DESeq2 DESeqDataSetFromMatrix DESeq results
#' @importFrom ALDEx2 aldex aldex.clr
#' @importFrom ANCOMBC ancombc
#' @importFrom parallel mclapply
#' @importFrom phyloseq otu_table sample_data
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

# Helper function for DESeq2
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

# Helper function for ALDEx2
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

# Helper function for ANCOM-BC
run_ancombc <- function(counts, groups, alpha, ...) {
    ancombc_out <- ANCOMBC::ancombc(
        phyloseq::otu_table(counts, taxa_are_rows = TRUE),
        phyloseq::sample_data(data.frame(group = groups)),
        formula = "group",
        p_adj_method = p_adjust_method
    )
    return(ancombc_out$res)
}

# Helper function to create results summary
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