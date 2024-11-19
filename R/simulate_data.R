#' Simulate Microbiome Data
#' 
#' @title Simulate Microbiome Count Data for Method Evaluation
#' @description Generate realistic microbiome count data with controlled differential abundance patterns
#' for evaluating differential abundance analysis (DAA) methods. Supports various parameters to
#' simulate different scenarios and data characteristics commonly observed in microbiome studies.
#' 
#' @param n_samples Number of samples to simulate (total across all groups)
#' @param n_taxa Number of taxa to simulate
#' @param n_diff Number of differentially abundant taxa
#' @param fold_changes Numeric vector of fold changes for differential taxa. 
#'        If single value, applied to all differential taxa
#' @param group_sizes Numeric vector specifying size of each group. Must sum to n_samples.
#'        Default is equal group sizes
#' @param dispersion Dispersion parameter for negative binomial distribution (default: 0.3).
#'        Controls the amount of variability in counts
#' @param lib_sizes Vector of library sizes. If NULL, randomly generated from
#'        negative binomial distribution
#' @param zero_prob Probability of generating zero counts (default: 0.3).
#'        Controls sparsity of the data
#' @param phylo_tree Logical; whether to generate a phylogenetic tree (default: FALSE)
#' @param seed Random seed for reproducibility
#'
#' @return A list containing:
#'         \itemize{
#'           \item counts: Count matrix (taxa in rows, samples in columns)
#'           \item truth: Logical vector indicating which taxa are truly differential
#'           \item group_info: Factor indicating group membership
#'           \item phylo_tree: Phylogenetic tree (if requested)
#'         }
#'
#' @details
#' The simulation process:
#' 1. Generates base abundances from log-normal distribution
#' 2. Applies fold changes to selected taxa in treatment group
#' 3. Generates counts using negative binomial distribution
#' 4. Introduces zeros according to zero_prob
#' 5. Optionally generates phylogenetic tree
#'
#' The function ensures:
#' - Realistic abundance distributions
#' - Controlled differential abundance patterns
#' - Appropriate levels of overdispersion
#' - Realistic sparsity patterns
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic simulation with default parameters
#' sim_data <- simulate_data(
#'   n_samples = 60,
#'   n_taxa = 100,
#'   n_diff = 10
#' )
#' 
#' # Complex simulation with custom parameters
#' sim_data <- simulate_data(
#'   n_samples = 100,
#'   n_taxa = 200,
#'   n_diff = 20,
#'   fold_changes = c(2, 3, 4),
#'   group_sizes = c(40, 60),
#'   zero_prob = 0.4,
#'   phylo_tree = TRUE,
#'   seed = 123
#' )
#' 
#' # Access simulated data
#' dim(sim_data$counts)
#' table(sim_data$truth)
#' table(sim_data$group_info)
#' }
#' 
#' @importFrom stats rnorm rnbinom rbinom
#' @importFrom ape rtree
simulate_data <- function(n_samples, 
                         n_taxa, 
                         n_diff = 0,
                         fold_changes = 2,
                         group_sizes = NULL,
                         dispersion = 0.3,
                         lib_sizes = NULL,
                         zero_prob = 0.3,
                         phylo_tree = FALSE,
                         seed = NULL) {
    
    # Input validation
    if (!is.null(seed)) set.seed(seed)
    
    # 处理 group_sizes
    if (is.null(group_sizes)) {
        group_sizes <- rep(n_samples/2, 2)
    } else {
        if (length(group_sizes) != 2) {
            stop("group_sizes must be a vector of length 2")
        }
        if (sum(group_sizes) != n_samples) {
            stop("Sum of group sizes must equal n_samples")
        }
    }
    
    # 其他验证
    if (n_diff > n_taxa) {
        stop("Number of differential taxa cannot exceed total number of taxa")
    }
    
    # Generate base abundances
    base_abundances <- exp(rnorm(n_taxa, mean = 2, sd = 1))
    
    # Generate library sizes if not provided
    if (is.null(lib_sizes)) {
        lib_sizes <- rnbinom(n_samples, mu = 1e4, size = 20)
    }
    
    # Create group information - 修改这部分以使用确切的组大小
    group_info <- factor(rep(c("A", "B"), group_sizes))
    
    # Select differential taxa
    diff_taxa <- sample(1:n_taxa, n_diff)
    fold_changes <- rep(fold_changes, length.out = n_diff)
    
    # Generate counts matrix
    counts <- matrix(0, nrow = n_taxa, ncol = n_samples)
    for (i in 1:n_samples) {
        # Adjust abundances based on group and differential status
        curr_abundances <- base_abundances
        if (group_info[i] == "B") {
            curr_abundances[diff_taxa] <- curr_abundances[diff_taxa] * fold_changes
        }
        
        # Generate counts
        lambda <- curr_abundances * lib_sizes[i]
        counts[, i] <- rnbinom(n_taxa, mu = lambda, size = 1/dispersion)
        
        # Add zeros according to zero_prob
        zero_mask <- rbinom(n_taxa, 1, zero_prob)
        counts[zero_mask == 1, i] <- 0
    }
    
    # Generate phylogenetic tree if requested
    tree <- if (phylo_tree) ape::rtree(n_taxa) else NULL
    
    # Create result object
    result <- list(
        counts = counts,
        truth = rep(FALSE, n_taxa),
        group_info = group_info,
        phylo_tree = tree
    )
    result$truth[diff_taxa] <- TRUE
    
    # Add row and column names
    rownames(result$counts) <- paste0("Taxa", 1:n_taxa)
    colnames(result$counts) <- paste0("Sample", 1:n_samples)
    
    return(result)
} 