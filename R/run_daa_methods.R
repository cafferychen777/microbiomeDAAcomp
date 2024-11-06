#' Run Multiple DAA Methods
#' 
#' @title Execute Multiple Differential Abundance Analysis Methods
#' @description Run multiple DAA methods on the same dataset and return standardized results
#' for comparison.
#' 
#' @param data A list containing count matrix and group information, or a phyloseq object
#' @param methods Character vector of method names to run. 
#'        Supported methods: "DESeq2", "ALDEx2", "ANCOM-BC"
#' @param count_matrix Optional matrix of counts if not providing full data object
#' @param group_info Optional factor of group assignments if not providing full data object
#' @param alpha Significance level for differential abundance calling (default: 0.05)
#' @param paired Logical; whether the data is paired (default: FALSE)
#' @param p_adjust_method Method for p-value adjustment (default: "BH")
#' @param cores Number of cores for parallel processing (default: 1)
#' @param ... Additional arguments passed to individual methods
#'
#' @return A list containing:
#'   \item{results}{List of results from each method}
#'   \item{summary}{Data frame comparing results across methods}
#'   \item{runtime}{Vector of computation times}
#'   \item{parameters}{List of parameters used for each method}
#' @export
#'
#' @examples
#' \dontrun{
#' # Run multiple methods on simulated data
#' sim_data <- simulate_data(n_samples = 60, n_taxa = 100)
#' results <- run_daa_methods(
#'   data = sim_data,
#'   methods = c("DESeq2", "ALDEx2"),
#'   alpha = 0.05
#' )
#' 
#' # Run with custom parameters
#' results <- run_daa_methods(
#'   count_matrix = counts,
#'   group_info = groups,
#'   methods = "DESeq2",
#'   p_adjust_method = "bonferroni"
#' )
#' }
#' 
#' @importFrom DESeq2 DESeqDataSetFromMatrix DESeq results
#' @importFrom ALDEx2 aldex aldex.clr
#' @importFrom ANCOMBC ancombc
#' @importFrom parallel mclapply
run_daa_methods <- function(data = NULL,
                          methods = c("DESeq2", "ALDEx2", "ANCOM-BC"),
                          count_matrix = NULL,
                          group_info = NULL,
                          alpha = 0.05,
                          paired = FALSE,
                          p_adjust_method = "BH",
                          cores = 1,
                          ...) {
    
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
    
    methods <- match.arg(methods, several.ok = TRUE)
    
    # Initialize results storage
    results <- list()
    runtime <- numeric(length(methods))
    parameters <- list()
    
    # Run each method
    for (i in seq_along(methods)) {
        method <- methods[i]
        start_time <- Sys.time()
        
        tryCatch({
            results[[method]] <- switch(method,
                "DESeq2" = run_deseq2(count_matrix, group_info, alpha, p_adjust_method, ...),
                "ALDEx2" = run_aldex2(count_matrix, group_info, alpha, p_adjust_method, ...),
                "ANCOM-BC" = run_ancombc(count_matrix, group_info, alpha, ...)
            )
            runtime[i] <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
            parameters[[method]] <- list(alpha = alpha, 
                                      p_adjust_method = p_adjust_method,
                                      paired = paired)
        }, error = function(e) {
            warning(sprintf("Error in method %s: %s", method, e$message))
            results[[method]] <- NULL
            runtime[i] <- NA
        })
    }
    
    # Create summary comparison
    summary <- create_summary(results, methods)
    
    return(list(
        results = results,
        summary = summary,
        runtime = setNames(runtime, methods),
        parameters = parameters
    ))
}

# Helper function for DESeq2
run_deseq2 <- function(counts, groups, alpha, p_adjust_method, ...) {
    dds <- DESeq2::DESeqDataSetFromMatrix(
        countData = round(counts),
        colData = data.frame(group = groups),
        design = ~ group
    )
    dds <- DESeq2::DESeq(dds)
    res <- DESeq2::results(dds, alpha = alpha, pAdjustMethod = p_adjust_method)
    return(as.data.frame(res))
}

# Helper function for ALDEx2
run_aldex2 <- function(counts, groups, alpha, p_adjust_method, ...) {
    ald <- ALDEx2::aldex(counts, groups, test = "t", effect = TRUE)
    return(as.data.frame(ald))
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
    summary_df <- data.frame(
        Method = methods,
        N_Significant = sapply(results, function(x) sum(x$padj < 0.05, na.rm = TRUE)),
        Mean_Effect = sapply(results, function(x) mean(abs(x$log2FoldChange), na.rm = TRUE))
    )
    return(summary_df)
} 